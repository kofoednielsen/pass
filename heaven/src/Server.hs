{-# LANGUAGE OverloadedStrings #-}
module Server (main) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_, void)
import qualified Control.Concurrent as C
import Control.Exception (handle)
import System.Random

import qualified Network.WebSockets as WS

import Types
import qualified Api

host :: String
host = "127.0.0.1"

port :: Int
port = 8080

width, height :: Int
width = 20
height = 20

dummyProjectile :: Position -- FIXME: Just to show something
dummyProjectile = Position { positionX = 5
                           , positionY = 15
                           }

newState :: State
newState = State { statePlayers = []
                 , stateProjectiles = [ dummyProjectile ]
                 }

newPlayer :: Text -> WS.Connection -> IO Player
newPlayer name conn = do
  x <- getStdRandom (randomR (0, width - 1))
  y <- getStdRandom (randomR (0, height - 1))
  return $ Player { playerConnection = conn
                  , playerName = name
                  , playerInvincible = True
                  , playerPosition = Position { positionX = x
                                              , positionY = y
                                              }
                  , playerHealth = 1.0
                  }

addPlayer :: Player -> State -> State
addPlayer player state = state { statePlayers = player : statePlayers state }

removePlayer :: Text -> State -> State
removePlayer name state = state { statePlayers = filter ((/= name) . playerName)
                                                 $ statePlayers state }

main :: IO ()
main = do
  state <- C.newMVar newState
  void $ C.forkIO $ gameLoop state
  WS.runServer host port $ server state

server :: C.MVar State -> WS.ServerApp
server state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ receiveLoop conn state

-- Attacking in heaven makes you lose health (temporary gameplay, not what was planned).
attack :: C.MVar State -> Text -> IO ()
attack stateMVar name =
  C.modifyMVar_ stateMVar
    $ pure . updatePlayer name (\player -> player { playerHealth = playerHealth player - 0.1 })

updatePlayers :: ([Player] -> [Player]) -> (State -> State)
updatePlayers f s = s { statePlayers = f $ statePlayers s }

updatePlayer :: Text -> (Player -> Player) -> (State -> State)
updatePlayer name f s =
  flip updatePlayers s
  $ \players -> map (\p -> if playerName p == name
                           then f p
                           else p) players

receiveLoop :: WS.Connection -> C.MVar State -> IO ()
receiveLoop conn stateMVar = do
  msg <- WS.receiveData conn
  let request = Api.decodeText msg
      name = requestName request
      changePosition :: Int -> Int -> IO ()
      changePosition xDiff yDiff =
        C.modifyMVar_ stateMVar
        $ pure . updatePlayer name (\player ->
                                      let position = playerPosition player
                                      in player { playerPosition = Position { positionX = positionX position + xDiff
                                                                            , positionY = positionY position + yDiff
                                                                            }
                                                })

  print request
  case requestAction request of
    Join -> do
      player <- newPlayer (requestName request) conn
      C.modifyMVar_ stateMVar $ pure . addPlayer player
    Leave -> C.modifyMVar_ stateMVar $ pure . removePlayer name
    GoUp -> changePosition 0 (-1)
    GoDown -> changePosition 0 1
    GoLeft -> changePosition (-1) 0
    GoRight -> changePosition 1 0
    Attack -> attack stateMVar name
  receiveLoop conn stateMVar

gameLoop :: C.MVar State -> IO ()
gameLoop stateMVar = do
  state <- C.readMVar stateMVar

  putStrLn "current players:"
  forM_ (statePlayers state) print

  let state' = state
      response = NewState state'
      responseText = Api.encodeText response
  putStrLn "Response:"
  print responseText
  broadcast stateMVar responseText
  putStrLn "----------------"
  putStrLn ""

  C.threadDelay 100000
  gameLoop stateMVar

send :: Player -> Text -> IO ()
send player message = WS.sendTextData (playerConnection player) message

broadcast :: C.MVar State -> Text -> IO ()
broadcast stateMVar message = do
  state <- C.readMVar stateMVar
  forM_ (statePlayers state) $ \player -> handle (handler player) $ send player message
  where handler :: Player -> WS.ConnectionException -> IO ()
        handler player _ = C.modifyMVar_ stateMVar $ pure . removePlayer (playerName player)
