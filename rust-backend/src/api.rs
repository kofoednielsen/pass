use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PlayerAction {
    Up,
    Down,
    Left,
    Right,
    Attack,
    Join,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct ClientRequest {
    name: String,
    action: PlayerAction,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "event")]
pub enum ServerResponse {
    Switch,
    State(GameState),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct GameState {
    theme: &'static str,
    players: Vec<Player>,
    projectiles: Vec<Position>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Player {
    name: String,
    suffx: String,
    invincible: bool,
    facing: Direction,
    position: Position,
    health: u32,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Position {
    x: u32,
    y: u32,
}

#[cfg(tests)]
mod tests {
    use serde_json::{from_value, to_value, json};
    use super::*;

    #[test]
    fn test_request() {
        let value = json!({
            "name": "abc",
            "action": "up",
        });
        let request = ClientRequest {
            name: "abc".into(),
            action: PlayerAction::Up,
        };

        assert_eq!(from_value(value), Ok(request));
    }

    #[test]
    fn test_response_switch() {
        let value = json!({
            "event": "switch",
        });
        let request = ServerResponse::Switch;

        assert_eq!(Ok(value), to_value(request));
    }

    #[test]
    fn test_response_state() {
        let value = json!({
            "event": "state",
            "theme": "snake",
            "players": [
                {
                    "name": "abc",
                    "suffx": "the destroyer",
                    "invincible": true,
                    "facing": "right",
                    "position": { "x": 10, "y": 20 },
                    "health": 10,
                },
            ],
            "projectiles": [
                { "x": 10, "y": 20 },
                { "x": 20, "y": 10 },
            ]
        });
        let request = ServerResponse::State(GameState {
            theme: "snake",
            players: vec![
                Player {
                    name: "abc".into(),
                    suffx: "the destroyer".into(),
                    invincible: true,
                    facing: Direction::Right,
                    position: Position { x: 10, y: 20 },
                    health: 10,
                }
            ],
            projectiles: vec![
                Position { x: 10, y: 20 },
                Position { x: 20, y: 10 },
            ],
        });

        assert_eq!(Ok(value), to_value(request));
    }
}
