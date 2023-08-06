use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PlayerAction {
    Up,
    Down,
    Left,
    Right,
    Attack,
    Join,
    #[serde(skip)]
    Leave,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct ClientRequest {
    pub name: String,
    pub action: PlayerAction,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "event")]
pub enum ServerResponse<'a> {
    Switch,
    State(State<'a>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct State<'a> {
    pub theme: &'a str,
    pub players: &'a [Player<'a>],
    pub projectiles: &'a [Position],
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Player<'a> {
    pub name: &'a str,
    pub suffx: &'a str,
    pub invincible: bool,
    pub facing: Direction,
    pub position: Position,
    pub health: u32,
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
    pub x: u32,
    pub y: u32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json, to_value};

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

        assert_eq!(from_value::<ClientRequest>(value).unwrap(), request);
    }

    #[test]
    fn test_response_switch() {
        let value = json!({
            "event": "switch",
        });
        let request = ServerResponse::Switch;

        assert_eq!(value, to_value(request).unwrap());
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
        let request = ServerResponse::State(State {
            theme: "snake",
            players: &[Player {
                name: "abc",
                suffx: "the destroyer",
                invincible: true,
                facing: Direction::Right,
                position: Position { x: 10, y: 20 },
                health: 10,
            }],
            projectiles: &[Position { x: 10, y: 20 }, Position { x: 20, y: 10 }],
        });

        assert_eq!(value, to_value(request).unwrap());
    }
}
