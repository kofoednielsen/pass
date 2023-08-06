//! Mostly copied from:
//! <https://github.com/snapview/tokio-tungstenite/blob/71d06d4495b6315181ced85b82a28044d3383854/examples/server-custom-accept.rs>
//!
//! I would love to use something simpler...

use std::{
    collections::HashMap,
    convert::Infallible,
    env,
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use futures_channel::mpsc::{unbounded, UnboundedSender};
use futures_util::{future, pin_mut, stream::TryStreamExt, StreamExt};
use hyper::{
    header::{
        HeaderValue, CONNECTION, SEC_WEBSOCKET_ACCEPT, SEC_WEBSOCKET_KEY, SEC_WEBSOCKET_VERSION,
        UPGRADE,
    },
    server::conn::AddrStream,
    service::{make_service_fn, service_fn},
    upgrade::Upgraded,
    Body, Method, Request, Response, Server, StatusCode, Version,
};
use tokio_tungstenite::tungstenite::{
    handshake::derive_accept_key,
    protocol::{Message, Role},
};
use tokio_tungstenite::WebSocketStream;

mod api;
mod snake;
use crate::api::{ClientRequest, PlayerAction, ServerResponse};

use self::snake::GameState;

type Tx = UnboundedSender<Message>;
type PeerMap = Arc<Mutex<HashMap<SocketAddr, (Tx, Option<String>)>>>;
type State = Arc<Mutex<GameState>>;

fn send(tx: &UnboundedSender<Message>, response: &ServerResponse) {
    let response = serde_json::to_string(response).unwrap();
    tx.unbounded_send(Message::Text(response)).unwrap();
}

async fn handle_connection(
    state: State,
    peer_map: PeerMap,
    ws_stream: WebSocketStream<Upgraded>,
    addr: SocketAddr,
) {
    println!("WebSocket connection established: {addr}");

    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();

    send(&tx, &ServerResponse::State(state.lock().unwrap().poll()));

    peer_map.lock().unwrap().insert(addr, (tx, None));

    let (outgoing, incoming) = ws_stream.split();

    let broadcast_incoming = incoming.try_for_each(|msg| {
        println!("Received a message from {addr}: {msg:?}");

        match msg {
            Message::Text(request) => {
                let mut peer_map_lock = peer_map.lock().unwrap();
                let current_name = &mut peer_map_lock.get_mut(&addr).unwrap().1;
                let request: ClientRequest = serde_json::from_str(&request).expect("invalid input");
                match (&request.action, &current_name) {
                    (PlayerAction::Join, Some(_)) => {
                        panic!("can only join once per WebSocket connection")
                    }
                    (PlayerAction::Join, None) => {
                        *current_name = Some(request.name.clone());
                    }
                    (_, None) => {
                        panic!("you need to join before you can start performing actions")
                    }
                    (_, Some(_)) => {}
                }
                let mut state = state.lock().unwrap();
                let (server_state, switchers) = state
                    .handle_request(&request.name, request.action.clone())
                    .unwrap();
                drop(state);

                let response = ServerResponse::State(server_state);
                for (recp, name) in peer_map_lock.values() {
                    send(recp, &response);
                    if let Some(name) = name {
                        if switchers.contains(&name) {
                            send(recp, &ServerResponse::Switch)
                        }
                    }
                }
            }
            _ => {}
        }

        future::ok(())
    });

    let receive_from_others = rx.map(Ok).forward(outgoing);

    pin_mut!(broadcast_incoming, receive_from_others);
    future::select(broadcast_incoming, receive_from_others).await;

    println!("{addr} disconnected");
    peer_map.lock().unwrap().remove(&addr);
}

async fn handle_request(
    state: State,
    peer_map: PeerMap,
    mut req: Request<Body>,
    addr: SocketAddr,
) -> Result<Response<Body>, Infallible> {
    println!("Received a new, potentially ws handshake");
    println!("The request's path is: {}", req.uri().path());
    println!("The request's headers are:");
    for (ref header, _value) in req.headers() {
        println!("* {header}");
    }
    let upgrade = HeaderValue::from_static("Upgrade");
    let websocket = HeaderValue::from_static("websocket");
    let headers = req.headers();
    let key = headers.get(SEC_WEBSOCKET_KEY);
    let derived = key.map(|k| derive_accept_key(k.as_bytes()));
    if req.method() != Method::GET
        || req.version() < Version::HTTP_11
        || !headers
            .get(CONNECTION)
            .and_then(|h| h.to_str().ok())
            .map(|h| {
                h.split(|c| c == ' ' || c == ',')
                    .any(|p| p.eq_ignore_ascii_case(upgrade.to_str().unwrap()))
            })
            .unwrap_or(false)
        || !headers
            .get(UPGRADE)
            .and_then(|h| h.to_str().ok())
            .map(|h| h.eq_ignore_ascii_case("websocket"))
            .unwrap_or(false)
        || !headers
            .get(SEC_WEBSOCKET_VERSION)
            .map(|h| h == "13")
            .unwrap_or(false)
        || key.is_none()
        || req.uri() != "/socket"
    {
        return Ok(Response::new(Body::from("Hello World!")));
    }
    let ver = req.version();
    tokio::task::spawn(async move {
        match hyper::upgrade::on(&mut req).await {
            Ok(upgraded) => {
                handle_connection(
                    state,
                    peer_map,
                    WebSocketStream::from_raw_socket(upgraded, Role::Server, None).await,
                    addr,
                )
                .await;
            }
            Err(e) => println!("upgrade error: {e}"),
        }
    });
    let mut res = Response::new(Body::empty());
    *res.status_mut() = StatusCode::SWITCHING_PROTOCOLS;
    *res.version_mut() = ver;
    res.headers_mut().append(CONNECTION, upgrade);
    res.headers_mut().append(UPGRADE, websocket);
    res.headers_mut()
        .append(SEC_WEBSOCKET_ACCEPT, derived.unwrap().parse().unwrap());
    Ok(res)
}

#[tokio::main]
async fn main() -> Result<(), hyper::Error> {
    let state = State::default();
    let peer_map = PeerMap::default();

    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string())
        .parse()
        .unwrap();

    let make_svc = make_service_fn(move |conn: &AddrStream| {
        let remote_addr = conn.remote_addr();
        let state = state.clone();
        let peer_map = peer_map.clone();
        let service = service_fn(move |req| {
            handle_request(state.clone(), peer_map.clone(), req, remote_addr)
        });
        async { Ok::<_, Infallible>(service) }
    });

    let server = Server::bind(&addr).serve(make_svc);

    server.await?;

    Ok::<_, hyper::Error>(())
}
