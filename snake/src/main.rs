use std::{
    collections::HashMap,
    env, eprintln, fmt, io,
    net::SocketAddr,
    sync::{Arc, Mutex},
};

use futures_channel::mpsc::{unbounded, UnboundedSender};
use futures_util::{future, pin_mut, stream::TryStreamExt, StreamExt};
use rand::{rngs::ThreadRng, thread_rng};
use tokio::{
    net::{TcpListener, TcpStream},
    time,
};
use tokio_tungstenite::tungstenite::protocol::Message;
use tokio_tungstenite::{accept_async, WebSocketStream};

mod api;
mod snake;

use crate::{
    api::{ClientRequest, PlayerAction, ServerResponse},
    snake::STEP_INTERVAL,
};

#[derive(Debug)]
pub enum Error {
    PlayerNotFound,
    JoinOnce,
    JoinBeforeAction,
    InvalidInput(serde_json::Error),
    Io(io::Error),
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Self::InvalidInput(e)
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PlayerNotFound => write!(f, "can't do action on non-existing player"),
            Self::JoinOnce => write!(f, "can only join once per WebSocket connection"),
            Self::JoinBeforeAction => write!(
                f,
                "you need to join before you can start performing actions"
            ),
            Self::InvalidInput(e) => write!(f, "invalid input: {e}"),
            Self::Io(e) => write!(f, "IO error: {e}"),
        }
    }
}

impl std::error::Error for Error {}

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
        println!("Connect: {addr}");
        let current_state = self.game_state.lock().unwrap().get_state();
        send(&tx, &ServerResponse::State(current_state));
        self.peer_map.lock().unwrap().insert(addr, (tx, None));
    }

    fn request(&self, addr: SocketAddr, request: String) -> Result<(), Error> {
        println!("Request: {addr}, {request}");
        let request: ClientRequest = serde_json::from_str(&request)?;
        {
            let mut peer_map_lock = self.peer_map.lock().unwrap();
            let current_name = &mut peer_map_lock.get_mut(&addr).unwrap().1;
            match (&request.action, &current_name) {
                (PlayerAction::Join, Some(_)) => {
                    return Err(Error::JoinOnce);
                }
                (PlayerAction::Join, None) => {
                    *current_name = Some(request.name.clone());
                }
                (_, None) => {
                    return Err(Error::JoinBeforeAction);
                }
                (_, Some(_)) => {}
            }
        }
        self.handle_request(request)?;
        self.publish_state()
    }

    fn disconnect(&self, addr: SocketAddr) -> Result<(), Error> {
        println!("Disconnect: {addr}");
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        let disconnected_name = &peer_map_lock.remove(&addr).unwrap().1;
        if let Some(disconnected_name) = disconnected_name {
            let name = disconnected_name.clone();
            drop(peer_map_lock);
            self.handle_request(ClientRequest {
                name,
                action: PlayerAction::Leave,
            })?;
            self.publish_state()
        } else {
            Ok(())
        }
    }

    fn step(&self, rng: &mut ThreadRng) -> Result<(), Error> {
        let switchers = self.game_state.lock().unwrap().step(rng);
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        for (recp, peer_name) in peer_map_lock.values_mut() {
            if let Some(name) = peer_name {
                if switchers.contains(&name) {
                    send(recp, &ServerResponse::Switch { name: name.clone() });
                    *peer_name = None;
                }
            }
        }
        drop(peer_map_lock);

        self.publish_state()?;

        Ok(())
    }

    fn handle_request(&self, request: ClientRequest) -> Result<(), Error> {
        let mut state = self.game_state.lock().unwrap();
        state.handle_request(&request.name, request.action.clone())
    }

    fn publish_state(&self) -> Result<(), Error> {
        let server_state = self.game_state.lock().unwrap().get_state();
        let response = ServerResponse::State(server_state);
        let mut peer_map_lock = self.peer_map.lock().unwrap();
        for (recp, _) in peer_map_lock.values_mut() {
            send(recp, &response);
        }

        Ok(())
    }
}

async fn handle_connection(state: &State, ws_stream: WebSocketStream<TcpStream>, addr: SocketAddr) {
    // Insert the write part of this peer to the peer map.
    let (tx, rx) = unbounded();
    state.connect(addr, tx);
    let (outgoing, incoming) = ws_stream.split();

    let broadcast_incoming = incoming.try_for_each(|msg| {
        match msg {
            Message::Text(request) => state
                .request(addr, request)
                .unwrap_or_else(|e| eprintln!("{e}")),
            _ => eprintln!("unhandled message: {msg:?}"),
        }

        future::ok(())
    });

    let receive_from_others = rx.map(Ok).forward(outgoing);

    pin_mut!(broadcast_incoming, receive_from_others);
    future::select(broadcast_incoming, receive_from_others).await;

    state.disconnect(addr).unwrap_or_else(|e| eprintln!("{e}"))
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    let listener = TcpListener::bind(&addr).await.expect("failed to bind");
    println!("Listening on: {addr}");

    let state = State::default();

    let state_step = state.clone();
    tokio::spawn(async move {
        let mut interval = time::interval(STEP_INTERVAL);

        loop {
            interval.tick().await;
            let mut rng = thread_rng();
            state_step
                .step(&mut rng)
                .unwrap_or_else(|e| eprintln!("{e}"));
        }
    });

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
