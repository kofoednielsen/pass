mod api;

pub use api::*;
pub use std::io::Result;

#[derive(Clone, Debug, PartialEq)]
pub struct GameState {}

impl GameState {
    pub fn request(&self, request: ClientRequest) -> Result<()> {
        dbg!(request);
        Ok(())
    }

    pub fn get_state(&self) -> State<'_> {
        let players = &[Player {
            name: "abc",
            suffx: "the destroyer",
            invincible: true,
            facing: Direction::Right,
            position: Position { x: 10, y: 20 },
            health: 10,
        }];
        let projectiles = &[Position { x: 10, y: 20 }, Position { x: 20, y: 10 }];

        State {
            theme: "snake",
            players,
            projectiles,
        }
    }
}
