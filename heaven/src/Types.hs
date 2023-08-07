module Types
  ( Position(..)
  , Player(..)
  , State(..)
  , Response(..)
  , PlayerAction(..)
  , Request(..)
  ) where

import Data.Text (Text)

import qualified Network.WebSockets as WS

data Position = Position { positionX :: Int
                         , positionY :: Int
                         }
  deriving (Show)

instance Show WS.Connection where
  show = const "<connection>"

data Player = Player { playerConnection :: WS.Connection
                     , playerName :: Text
                     , playerInvincible :: Bool
                     , playerPosition :: Position
                     , playerHealth :: Double
                     }
  deriving (Show)

data State = State { statePlayers :: [Player]
                   , stateProjectiles :: [Position]
                   }
  deriving (Show)

data Response = Switch Text
              | NewState State
  deriving (Show)

data PlayerAction = Join
                  | Leave
                  | GoUp
                  | GoDown
                  | GoLeft
                  | GoRight
                  | Attack
  deriving (Show)

data Request = Request { requestName :: Text
                       , requestAction :: PlayerAction
                       }
  deriving (Show)
