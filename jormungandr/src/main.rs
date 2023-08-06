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

fn send(tx: &UnboundedSender<Message>, response: &ServerResponse) {
    let response = serde_json::to_string(response).unwrap();
    tx.unbounded_send(Message::Text(response)).unwrap();
}

#[derive(Debug, Default, Clone)]
struct State {
    peer_map: Arc<Mutex<HashMap<SocketAddr, (Tx, Option<String>)>>>,
    game_state: Arc<Mutex<GameState>>,
}

impl State {
    fn connect(&self, addr: SocketAddr, tx: Tx) {
        let current_state = self.game_state.lock().unwrap().get_state();
        send(&tx, &ServerResponse::State(current_state));
        self.peer_map.lock().unwrap().insert(addr, (tx, None));
    }

    fn request(&self, addr: SocketAddr, request: ClientRequest) -> Result<(), Error> {
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        let current_name = &mut peer_map_lock.get_mut(&addr).unwrap().1;
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
        self.inner_send_request(request)
    }

    fn inner_send_request(&self, request: ClientRequest) -> Result<(), Error> {
        let mut state = self.game_state.lock().unwrap();
        let (server_state, switchers) = state
            .handle_request(&request.name, request.action.clone())
            .unwrap();
        drop(state);

        let response = ServerResponse::State(server_state);
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        for (recp, peer_name) in peer_map_lock.values_mut() {
            send(recp, &response);
            if let Some(name) = peer_name {
                if switchers.contains(&name) {
                    send(recp, &ServerResponse::Switch { name: name.clone() });
                    *peer_name = None;
                }
            }
        }

        Ok(())
    }

    fn disconnect(&self, addr: SocketAddr) -> Result<(), Error> {
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        let disconnected_name = peer_map_lock.remove(&addr).unwrap().1;
        // TODO: Send updated server state here also
        if let Some(disconnected_name) = disconnected_name {
            self.inner_send_request(ClientRequest {
                name: disconnected_name.clone(),
                action: PlayerAction::Leave,
            })
        } else {
            Ok(())
        }
    }
}

async fn handle_connection(state: &State, ws_stream: WebSocketStream<TcpStream>, addr: SocketAddr) {
    println!("WebSocket connection established: {addr}");

    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();
    state.connect(addr, tx);
    let (outgoing, incoming) = ws_stream.split();

    let broadcast_incoming = incoming.try_for_each(|msg| {
        println!("Received a message from {addr}: {msg:?}");

        match msg {
            Message::Text(request) => {
                let request: ClientRequest = serde_json::from_str(&request).expect("invalid input");
                state.request(addr, request).unwrap();
            }
            _ => {}
        }

        future::ok(())
    });

    let receive_from_others = rx.map(Ok).forward(outgoing);

    pin_mut!(broadcast_incoming, receive_from_others);
    future::select(broadcast_incoming, receive_from_others).await;

    println!("{addr} disconnected");
    state.disconnect(addr).unwrap()
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    let listener = TcpListener::bind(&addr).await.expect("failed to bind");
    println!("Listening on: {addr}");

    let state = State::default();

    // Let's spawn the handling of each connection in a separate task.
    while let Ok((tcp_stream, addr)) = listener.accept().await {
        let state = state.clone();
        tokio::spawn(async move {
            println!("Incoming TCP connection from: {addr}");

            let ws_stream = accept_async(tcp_stream)
                .await
                .expect("error during the websocket handshake occurred");
            handle_connection(&state, ws_stream, addr).await
        });
    }

    Ok(())
}
