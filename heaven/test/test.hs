{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import Api

testClientRequest :: IO ()
testClientRequest =
  print $ decode $ BL.pack "{\"name\":\"niels\",\"action\":\"attack\"}"

testServerResponseSwitch :: IO ()
testServerResponseSwitch =
  BL.putStrLn $ encode $ Switch "playername"

testServerResponseNewState :: IO ()
testServerResponseNewState =
  let player = Player { playerConnection = undefined
                      , playerName = "niels"
                      , playerInvincible = False
                      , playerPosition = Position { positionX = 3
                                                  , positionY = 10
                                                  }
                      , playerHealth = 50
                      }
      projectile = Position { positionX = 1
                            , positionY = 0
                            }
      state = State { statePlayers = [ player ]
                    , stateProjectiles = [ projectile ]
                    }
  in BL.putStrLn $ encode $ NewState state

-- FIXME: Turn this into real tests and not just prints.
main :: IO ()
main = do
  testClientRequest
  testServerResponseSwitch
  testServerResponseNewState
