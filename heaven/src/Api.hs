module Api
    ( width
    , height
    , Position(..)
    , PlayerAction(..)
    , Player(..)
    , ClientRequest(..)
    , ServerState(..)
    , ServerResponse(..)
    ) where


width, height :: Int
width = 20
height = 20

data Position = Position { x :: Int
                         , y :: Int
                         }
  deriving (Show, Eq)

data PlayerAction = Up
                  | Down
                  | Left
                  | Right
                  | Attack
                  | Join
                  | Leave
  deriving (Show, Eq)

data Player = Player { playerName :: String
                     , suffix :: String
                     , invincible :: Bool
                     , position :: Position
                     , health :: Double
                     }
  deriving (Show, Eq)

data ClientRequest = ClientRequest { requestName :: String
                                   , action :: PlayerAction
                                   }
  deriving (Show, Eq)

data ServerState = ServerState { theme :: String
                               , players :: [Player]
                               , projectiles :: [Position]
                               }
  deriving (Show, Eq)

data ServerResponse = Switch String
                    | State ServerState
  deriving (Show, Eq)
