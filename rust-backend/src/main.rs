use std::env;
use std::io::Error;
use std::net::SocketAddr;
use std::sync::Arc;

use futures_util::{future, StreamExt, TryStreamExt, SinkExt};
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::accept_async;

use pass::*;

#[derive(Debug)]
struct Client {}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = env::args()
        .nth(1)
        .unwrap_or_else(|| "127.0.0.1:8080".to_string());

    // Create the event loop and TCP listener we'll accept connections on.
    let try_socket = TcpListener::bind(&addr).await;
    let listener = try_socket.expect("Failed to bind");
    println!("Listening on: {addr}");

    let state = Arc::new(GameState {});
    // let clients = [];

    while let Ok((stream, _)) = listener.accept().await {
        let peer = stream
            .peer_addr()
            .expect("connected streams should have a peer address");
        println!("Peer address: {peer}");

        tokio::spawn(accept_connection(state.clone(), peer, stream));
    }

    Ok(())
}

async fn accept_connection(state: Arc<GameState>, peer: SocketAddr, stream: TcpStream) {
    let ws_stream = accept_async(stream)
        .await
        .expect("Error during the websocket handshake occurred");
    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    println!("New WebSocket connection: {peer}");

    while let Some(msg) = ws_receiver.next().await {
        let msg = msg.unwrap();
        if msg.is_text() || msg.is_binary() {
            ws_sender.send(msg).await.unwrap();
        }
    }
}
