use rand::Rng;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::time::Duration;

use crate::api::{Player, PlayerAction, Position, ServerState, HEIGHT, WIDTH};
use crate::Error;

pub const STEP_INTERVAL: Duration = Duration::from_millis(200);

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
    current_direction: Direction,
    queued_directions: VecDeque<Direction>,
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
        rng: &mut impl Rng,
        name: &str,
        action: PlayerAction,
    ) -> Result<(), Error> {
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
            (Entry::Vacant(_), PlayerAction::Join) => {
                let starting_position = self.find_free_spot(rng);
                let starting_direction = Direction::Right;
                // Work around lifetime issues
                self.players.insert(
                    name.to_string(),
                    PlayerData {
                        position: starting_position,
                        tail: VecDeque::new(),
                        current_direction: starting_direction,
                        queued_directions: VecDeque::new(),
                        client_count: 1,
                    },
                );
            }
            (Entry::Vacant(_), PlayerAction::Leave) => {
                eprintln!("WARN: Somehow player {name} left without it being in the state");
            }
            (Entry::Occupied(mut player), PlayerAction::Up) => {
                player.get_mut().queued_directions.push_back(Direction::Up);
            }
            (Entry::Occupied(mut player), PlayerAction::Down) => {
                player
                    .get_mut()
                    .queued_directions
                    .push_back(Direction::Down);
            }
            (Entry::Occupied(mut player), PlayerAction::Left) => {
                player
                    .get_mut()
                    .queued_directions
                    .push_back(Direction::Left);
            }
            (Entry::Occupied(mut player), PlayerAction::Right) => {
                player
                    .get_mut()
                    .queued_directions
                    .push_back(Direction::Right);
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

    fn find_free_spot(&self, rng: &mut impl Rng) -> Position {
        // Try to find a non-populated spot to start
        let mut position = Position { x: 0, y: 0 };
        for _ in 1..100 {
            let x = rng.gen_range(0..WIDTH);
            let y = rng.gen_range(0..HEIGHT);
            position = Position { x, y };
            'player: for player in self.players.values() {
                if position == player.position {
                    continue 'player;
                }
                for tail in player.tail.iter() {
                    if position == *tail {
                        continue 'player;
                    }
                }
            }
            break;
        }
        position
    }

    pub fn step(&mut self, rng: &mut impl Rng) -> Vec<String> {
        // Move all players, and create tail in their place
        for player in self.players.values_mut() {
            let prev_pos = player.position.clone();

            // Take next direction from the queue
            if let Some(new_direction) = player.queued_directions.pop_front() {
                player.current_direction = new_direction;
            }

            match player.current_direction {
                Direction::Up => {
                    player.position.add_y(-1);
                }
                Direction::Down => {
                    player.position.add_y(1);
                }
                Direction::Left => {
                    player.position.add_x(-1);
                }
                Direction::Right => {
                    player.position.add_x(1);
                }
            }

            // Walk the opposite direction if we were about to _directly_ eat ourselves
            if let Some(tail_front) = player.tail.front() {
                if tail_front == &player.position {
                    match player.current_direction {
                        Direction::Up => {
                            player.position.add_y(2);
                            player.current_direction = Direction::Down;
                        }
                        Direction::Down => {
                            player.position.add_y(-2);
                            player.current_direction = Direction::Up;
                        }
                        Direction::Left => {
                            player.position.add_x(2);
                            player.current_direction = Direction::Right;
                        }
                        Direction::Right => {
                            player.position.add_x(-2);
                            player.current_direction = Direction::Left;
                        }
                    }
                }
            }

            // Add new tail piece
            player.tail.push_front(prev_pos);
        }

        // Check player to player collisions
        //
        // Explicitly made this way so that players colliding head on still die.
        let mut dead_players = Vec::new();
        for (name, player) in self.players.iter() {
            if player.client_count == 0 {
                dead_players.push(name.to_string());
                continue;
            }

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
            self.current_apple = Some(self.find_free_spot(rng));
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
                    let health = player.tail.len().max(100) as i32;
                    [Player {
                        name: name.clone(),
                        suffx: "".into(),
                        invincible: false,
                        position: player.position.clone(),
                        health,
                    }]
                    .into_iter()
                    .chain(player.tail.iter().enumerate().map(move |(_, tail)| Player {
                        name: name.clone(),
                        suffx: "".into(),
                        invincible: false,
                        position: tail.clone(),
                        health,
                    }))
                })
                .flatten()
                .collect(),
            projectiles: self.current_apple.iter().cloned().collect(),
        }
    }
}
