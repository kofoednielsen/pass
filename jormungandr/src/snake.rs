use rand::Rng;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};

use crate::api::{Player, PlayerAction, Position, ServerState};
use crate::Error;

const WIDTH: i32 = 20;
const HEIGHT: i32 = 20;

#[derive(Clone, Debug, PartialEq)]
struct PlayerData {
    position: Position,
    tail: VecDeque<Position>,
    /// (x, y)
    /// Coordinate space: (0, 0) is top right corner
    ///
    /// TODO: Maybe implement this as a queue of last keypresses, for better feel?
    velocity: (i32, i32),
    client_count: usize,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct GameState {
    players: HashMap<String, PlayerData>,
    current_apple: Option<Position>,
}

impl GameState {
    pub fn handle_request(&mut self, name: &str, action: PlayerAction) -> Result<(), Error> {
        let player = self.players.entry(name.to_string());
        match (player, action) {
            (Entry::Occupied(mut player), PlayerAction::Join) => {
                // Explicitly allow two clients to be logged in as the same
                // player, since it's fun!
                player.get_mut().client_count += 1;
            }
            (Entry::Occupied(mut player), PlayerAction::Leave) => {
                player.get_mut().client_count -= 1;
                // Do not add players to `switchers` here, they know it themselves...
            }
            (Entry::Vacant(player), PlayerAction::Join) => {
                // TODO: Find the least populated spot on the map
                let starting_position = Position { x: 3, y: 3 };
                player.insert(PlayerData {
                    position: starting_position,
                    tail: VecDeque::new(),
                    // Start by standing still?
                    velocity: (0, 0),
                    client_count: 1,
                });
            }
            (Entry::Vacant(_), PlayerAction::Leave) => {
                eprintln!("WARN: Somehow player {name} left without it being in the state");
            }
            (Entry::Occupied(mut player), PlayerAction::Up) => {
                player.get_mut().velocity = (0, -1);
            }
            (Entry::Occupied(mut player), PlayerAction::Down) => {
                player.get_mut().velocity = (0, 1);
            }
            (Entry::Occupied(mut player), PlayerAction::Left) => {
                player.get_mut().velocity = (-1, 0);
            }
            (Entry::Occupied(mut player), PlayerAction::Right) => {
                player.get_mut().velocity = (1, 0);
            }
            (Entry::Occupied(_), PlayerAction::Attack) => {
                // Intentionally do nothing for now
            }
            (Entry::Vacant(_), _) => {
                return Err(Error::PlayerNotFound);
            }
        }

        Ok(())
    }

    pub fn step(&mut self, rng: &mut impl Rng) -> Vec<String> {
        // Move all players, and create tail in their place
        for player in self.players.values_mut() {
            if player.velocity == (0, 0) {
                continue;
            }
            if let Some(tail_front) = player.tail.front() {
                let x = player.position.x + player.velocity.0;
                let y = player.position.y + player.velocity.1;
                // Walk the opposite direction if we were about to _directly_ eat ourselves
                if tail_front.x == x && tail_front.y == y {
                    player.velocity = (-player.velocity.0, -player.velocity.1);
                }
            }
            player.tail.push_front(player.position.clone());
            player.position.x += player.velocity.0;
            player.position.y += player.velocity.1;
        }

        // Check player to player collisions
        //
        // Explicitly made this way so that players colliding head on still die.
        let mut dead_players = Vec::new();
        for (name, player) in self.players.iter() {
            for (other_name, other_player) in self.players.iter() {
                let head_hit = player.position == other_player.position;
                let tail_hit = other_player.tail.contains(&player.position);
                if name == other_name {
                    if tail_hit {
                        dead_players.push(name.to_string());
                    }
                } else {
                    if head_hit || tail_hit {
                        dead_players.push(name.to_string());
                    }
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

        if self.current_apple.is_none() {
            let x = rng.gen_range(0..WIDTH);
            let y = rng.gen_range(0..HEIGHT);
            self.current_apple = Some(Position { x, y });
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
