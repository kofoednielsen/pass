use crate::api::{Direction, Player, PlayerAction, Position, ServerState};

use std::io::Error;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct GameState {}

impl GameState {
    pub fn handle_request(
        &mut self,
        name: &str,
        action: PlayerAction,
    ) -> Result<(ServerState, Vec<String>), Error> {
        dbg!(name, action);
        Ok((self.get_state(), vec![]))
    }

    pub fn poll(&self) -> ServerState {
        self.get_state()
    }

    pub fn get_state(&self) -> ServerState {
        let players = vec![Player {
            name: "abc".into(),
            suffx: "the destroyer".into(),
            invincible: true,
            facing: Direction::Right,
            position: Position { x: 10, y: 10 },
            health: 10,
        }];
        let projectiles = vec![Position { x: 10, y: 11 }, Position { x: 11, y: 10 }];

        ServerState {
            theme: "snake",
            players,
            projectiles,
        }
    }
}
