{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.ServerResponse.NewState
  ( encode
  ) where

import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL

import Types

data IntermediatePosition =
  IntermediatePosition { x :: Int
                       , y :: Int
                       }
  deriving (Show)

data IntermediatePlayer =
  IntermediatePlayer { name :: Text
                     , invincible :: Bool
                     , position :: IntermediatePosition
                     , health :: Double
                     }
  deriving (Show)

data IntermediateResponse =
  IntermediateResponse { event :: Text
                       , players :: [IntermediatePlayer]
                       , projectiles :: [IntermediatePosition]
                       }
  deriving (Show)

$(deriveJSON defaultOptions ''IntermediatePosition)
$(deriveJSON defaultOptions ''IntermediatePlayer)
$(deriveJSON defaultOptions ''IntermediateResponse)

toIntermediatePosition :: Position -> IntermediatePosition
toIntermediatePosition p =
  IntermediatePosition { x = positionX p
                       , y = positionY p
                       }

toIntermediatePlayer :: Player -> IntermediatePlayer
toIntermediatePlayer p =
  IntermediatePlayer { name = playerName p
                     , invincible = playerInvincible p
                     , position = toIntermediatePosition $ playerPosition p
                     , health = playerHealth p
                     }

encode :: State -> BL.ByteString
encode state =
  let intermediateResponse =
        IntermediateResponse
        { event = "state"
        , players = map toIntermediatePlayer $ statePlayers state
        , projectiles = map toIntermediatePosition $ stateProjectiles state
        }
  in A.encode intermediateResponse
