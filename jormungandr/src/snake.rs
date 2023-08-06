use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::fmt;

use crate::api::{Player, PlayerAction, Position, ServerState};

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    msg: &'static str,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.msg)
    }
}

impl std::error::Error for Error {}

#[derive(Clone, Debug, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq)]
struct PlayerData {
    position: Position,
    tail: VecDeque<Position>,
    /// The last arrow key that the user pressed.
    desired_direction: Direction,
    client_count: usize,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct GameState {
    players: HashMap<String, PlayerData>,
    current_apple: Option<Position>,
}

impl GameState {
    pub fn handle_request(
        &mut self,
        name: &str,
        action: PlayerAction,
    ) -> Result<Vec<String>, Error> {
        let player = self.players.entry(name.to_string());
        let switchers = vec![];
        match (player, action) {
            (Entry::Occupied(mut player), PlayerAction::Join) => {
                // Explicitly allow two clients to be logged in as the same
                // player, since it's fun!
                player.get_mut().client_count += 1;
            }
            (Entry::Occupied(mut player), PlayerAction::Leave) => {
                player.get_mut().client_count -= 1;
                // Do not add players to switchers here, they know it themselves...
            }
            (Entry::Vacant(player), PlayerAction::Join) => {
                // TODO: Find the least populated spot on the map
                let starting_position = Position { x: 3, y: 3 };
                player.insert(PlayerData {
                    position: starting_position,
                    tail: VecDeque::new(),
                    // Just pick some sensible default
                    desired_direction: Direction::Down,
                    client_count: 1,
                });
            }
            (Entry::Vacant(_), PlayerAction::Leave) => {
                eprintln!("WARN: Somehow player {name} left without it being in the state");
            }
            (Entry::Occupied(mut player), PlayerAction::Up) => {
                player.get_mut().desired_direction = Direction::Up;
            }
            (Entry::Occupied(mut player), PlayerAction::Down) => {
                player.get_mut().desired_direction = Direction::Down;
            }
            (Entry::Occupied(mut player), PlayerAction::Left) => {
                player.get_mut().desired_direction = Direction::Left;
            }
            (Entry::Occupied(mut player), PlayerAction::Right) => {
                player.get_mut().desired_direction = Direction::Right;
            }
            (Entry::Occupied(_), PlayerAction::Attack) => {
                // Intentionally do nothing for now
            }
            (Entry::Vacant(_), _) => {
                return Err(Error {
                    msg: "can't do action on non-existing player",
                });
            }
        }

        Ok(switchers)
    }

    pub fn step(&mut self) -> Vec<String> {
        // Move all players, and create tail in their place
        for player in self.players.values_mut() {
            player.tail.push_front(player.position.clone());
            match player.desired_direction {
                // Coordinate space: (0, 0) is top right corner
                Direction::Up => {
                    player.position.y -= 1;
                }
                Direction::Down => {
                    player.position.y += 1;
                }
                Direction::Left => {
                    player.position.x -= 1;
                }
                Direction::Right => {
                    player.position.x += 1;
                }
            }
        }

        // Check player to player collisions
        let mut dead_players = Vec::new();
        for (name, player) in self.players.iter() {
            for other_player in self.players.values() {
                if player.position == other_player.position
                    || other_player.tail.contains(&player.position)
                {
                    dead_players.push(name.to_string());
                }
            }
        }

        // Remove collided players
        for dead_player in &dead_players {
            let _ = self.players.remove(dead_player);
        }

        // Shorten tails (if no collision with apple and if tail is longer than 3)
        let mut apple_eaten = false;
        for player in self.players.values_mut() {
            if Some(&player.position) == self.current_apple.as_ref() {
                apple_eaten = true;
                continue;
            }
            if player.tail.len() <= 3 {
                continue;
            }
            player.tail.pop_back();
        }

        if apple_eaten {
            self.current_apple = None;
        }

        dead_players
    }

    pub fn get_state(&self) -> ServerState {
        ServerState {
            theme: "snake",
            players: self
                .players
                .iter()
                .map(|(name, player)| {
                    [Player {
                        name: name.clone(),
                        suffx: "".into(),
                        invincible: false,
                        position: player.position.clone(),
                        health: 1.0,
                    }]
                    .into_iter()
                    .chain(player.tail.iter().enumerate().map(|(idx, tail)| Player {
                        name: name.clone(),
                        suffx: "".into(),
                        invincible: false,
                        position: tail.clone(),
                        health: (player.tail.len() as f32 / ((idx + 1) as f32)),
                    }))
                })
                .flatten()
                .collect(),
            projectiles: self.current_apple.iter().cloned().collect(),
        }
    }
}
