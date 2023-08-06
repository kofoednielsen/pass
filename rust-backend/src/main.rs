use std::env;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

use futures_util::{future, SinkExt, StreamExt, TryStreamExt};
use serde_json::{from_str, to_string};
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::accept_async;
use tokio_tungstenite::tungstenite::error::Error;
use tokio_tungstenite::tungstenite::Message;

use pass::*;

#[derive(Debug)]
struct Client {}

#[tokio::main]
async fn main() -> Result<()> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    // Create the event loop and TCP listener we'll accept connections on.
    let try_socket = TcpListener::bind(&addr).await;
    let listener = try_socket.expect("Failed to bind");
    println!("Listening on: {addr}");

    let state = Arc::new(Mutex::new(GameState {}));
    let clients = Arc::new(Mutex::new(vec![]));

    while let Ok((stream, _)) = listener.accept().await {
        let peer = stream
            .peer_addr()
            .expect("connected streams should have a peer address");
        println!("Peer address: {peer}");

        tokio::spawn(accept_connection(
            state.clone(),
            clients.clone(),
            peer,
            stream,
        ));
    }

    Ok(())
}

async fn accept_connection(
    state: Arc<Mutex<GameState>>,
    clients: Arc<Mutex<Vec<Client>>>,
    peer: SocketAddr,
    stream: TcpStream,
) {
    let ws_stream = accept_async(stream)
        .await
        .expect("Error during the websocket handshake occurred");
    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    println!("New WebSocket connection: {peer}");

    while let Some(msg) = ws_receiver.next().await {
        match msg {
            Ok(Message::Text(s)) => {
                let req: ClientRequest = from_str(&s).unwrap();
                state.lock().unwrap().request(req).unwrap();
            }
            Ok(Message::Close(_)) => {
                // TODO
            }
            Err(Error::ConnectionClosed | Error::Protocol(_) | Error::Utf8) => return,
            Err(e) => panic!("{e}"),
            _ => {}
        }
    }
}
