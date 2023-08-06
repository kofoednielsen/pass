use std::{
    collections::HashMap,
    env,
    io::Error,
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use futures_channel::mpsc::{unbounded, UnboundedSender};
use futures_util::{future, pin_mut, stream::TryStreamExt, StreamExt};
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::tungstenite::protocol::Message;
use tokio_tungstenite::{accept_async, WebSocketStream};

mod api;
mod snake;
use crate::api::{ClientRequest, PlayerAction, ServerResponse};

use self::snake::GameState;

type Tx = UnboundedSender<Message>;
type PeerMap = Arc<Mutex<HashMap<SocketAddr, (Tx, Option<String>)>>>;
type State = Arc<Mutex<GameState>>;

async fn handle_connection(
    state: State,
    peer_map: PeerMap,
    ws_stream: WebSocketStream<TcpStream>,
    addr: SocketAddr,
) {
    println!("WebSocket connection established: {addr}");

    let send = |tx: &UnboundedSender<Message>, response: &ServerResponse| {
        let response = serde_json::to_string(response).unwrap();
        tx.unbounded_send(Message::Text(response)).unwrap();
    };

    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();

    send(
        &tx,
        &ServerResponse::State(state.lock().unwrap().get_state()),
    );

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
                for (recp, name) in peer_map_lock.values_mut() {
                    send(recp, &response);
                    if let Some(some_name) = name {
                        if switchers.contains(&some_name) {
                            *name = None;
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
    let mut peer_map_lock = peer_map.lock().unwrap();
    let client_left = peer_map_lock.remove(&addr).unwrap().1;
    // TODO
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    let state = State::default();
    let peer_map = PeerMap::default();

    // Create the event loop and TCP listener we'll accept connections on.
    let try_socket = TcpListener::bind(&addr).await;
    let listener = try_socket.expect("Failed to bind");
    println!("Listening on: {addr}");

    // Let's spawn the handling of each connection in a separate task.
    while let Ok((tcp_stream, addr)) = listener.accept().await {
        let state = state.clone();
        let peer_map = peer_map.clone();
        tokio::spawn(async move {
            println!("Incoming TCP connection from: {addr}");

            let ws_stream = accept_async(tcp_stream)
                .await
                .expect("Error during the websocket handshake occurred");
            handle_connection(state.clone(), peer_map.clone(), ws_stream, addr).await
        });
    }

    Ok(())
}
