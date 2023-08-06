use crate::api::{Player, PlayerAction, Position, ServerState};

use std::{collections::VecDeque, io::Error};

#[derive(Clone, Debug, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq)]
struct PlayerData {
    player: Player,
    tail: VecDeque<Position>,
    /// The last arrow key that the user pressed.
    desired_direction: Direction,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct GameState {
    players: Vec<PlayerData>,
    current_apple: Option<Position>,
}

impl GameState {
    pub fn handle_request(
        &mut self,
        name: &str,
        action: PlayerAction,
    ) -> Result<(ServerState, Vec<String>), Error> {
        dbg!(name, action);
        Ok((self.get_state(), vec![]))
    }

    pub fn step(&mut self) -> Option<ServerState> {
        // Move all players, and create tail in their place
        for player in self.players.iter_mut() {
            player.tail.push_front(player.player.position.clone());
            match player.desired_direction {
                // Coordinate space: (0, 0) is top right corner
                Direction::Up => {
                    player.player.position.y -= 1;
                }
                Direction::Down => {
                    player.player.position.y += 1;
                }
                Direction::Left => {
                    player.player.position.x -= 1;
                }
                Direction::Right => {
                    player.player.position.x += 1;
                }
            }
        }

        // Check player to player collisions
        let mut dead_players = Vec::new();
        for (idx, player) in self.players.iter().enumerate() {
            for other_player in self.players.iter() {
                if player.player.position == other_player.player.position
                    || other_player.tail.contains(&player.player.position)
                {
                    dead_players.push(idx);
                }
            }
        }

        // Remove dead players
        todo!();

        // Shorten tails (if no collision with apple)
        for player in self.players.iter_mut() {
            if Some(player.player.position) == self.current_apple {
                continue;
            }
            player.tail.pop_back();
        }

        Some(self.get_state())
    }

    pub fn get_state(&self) -> ServerState {
        let players = vec![Player {
            name: "abc".into(),
            suffx: "the destroyer".into(),
            invincible: true,
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
