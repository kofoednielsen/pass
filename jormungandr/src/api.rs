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
pub enum ServerResponse {
    Switch { name: String },
    State(ServerState),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ServerState {
    pub theme: &'static str,
    pub players: Vec<Player>,
    pub projectiles: Vec<Position>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Player {
    pub name: String,
    pub suffx: String,
    pub invincible: bool,
    pub position: Position,
    pub health: f32,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub x: i32,
    pub y: i32,
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
            "name": "abc",
        });
        let request = ServerResponse::Switch { name: "abc".into() };

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
                    "position": { "x": 10, "y": 20 },
                    "health": 0.4,
                },
            ],
            "projectiles": [
                { "x": 10, "y": 20 },
                { "x": 20, "y": 10 },
            ]
        });
        let request = ServerResponse::State(ServerState {
            theme: "snake",
            players: vec![Player {
                name: "abc".into(),
                suffx: "the destroyer".into(),
                invincible: true,
                position: Position { x: 10, y: 20 },
                health: 0.4,
            }],
            projectiles: vec![Position { x: 10, y: 20 }, Position { x: 20, y: 10 }],
        });

        assert_eq!(value, to_value(request).unwrap());
    }
}
