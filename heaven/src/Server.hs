{-# LANGUAGE OverloadedStrings #-}
module Server (main) where

import Data.Text (Text)
import Control.Monad (forM_, void)
import qualified Control.Concurrent as C
import Control.Exception (handle)

import qualified Network.WebSockets as WS

import Types
import qualified Api

host :: String
host = "127.0.0.1"

port :: Int
port = 8080

dummyProjectile :: Position -- FIXME: Just to show something
dummyProjectile = Position { positionX = 5
                           , positionY = 15
                           }

newState :: State
newState = State { statePlayers = []
                 , stateProjectiles = [ dummyProjectile ]
                 }

newPlayer :: Text -> WS.Connection -> Player
newPlayer name conn =
  Player { playerConnection = conn
         , playerName = name
         , playerInvincible = False
         , playerPosition = Position { positionX = 3
                                     , positionY = 10
                                     }
         , playerHealth = 0.5
         }

addPlayer :: Player -> State -> State
addPlayer player state = state { statePlayers = player : statePlayers state }

removePlayer :: Player -> State -> State
removePlayer player state = state { statePlayers = filter ((/= playerName player) . playerName)
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

receiveLoop :: WS.Connection -> C.MVar State -> IO ()
receiveLoop conn state = do
  msg <- WS.receiveData conn
  let request = Api.decodeText msg
      player = newPlayer (requestName request) conn
  print request
  case requestAction request of
    Join -> C.modifyMVar_ state $ pure . addPlayer player
    Leave -> C.modifyMVar_ state $ pure . removePlayer player
    GoUp -> undefined
    GoDown -> undefined
    GoLeft -> undefined
    GoRight -> undefined
    Attack -> undefined
  receiveLoop conn state

gameLoop :: C.MVar State -> IO ()
gameLoop stateMVar = do
  state <- C.readMVar stateMVar

  putStrLn "current players:"
  forM_ (statePlayers state) print
  putStrLn "----------------"
  putStrLn ""

  let state' = state
      response = NewState state'
  broadcast stateMVar (Api.encodeText response)

  C.threadDelay 100000
  gameLoop stateMVar

send :: Player -> Text -> IO ()
send player message = WS.sendTextData (playerConnection player) message

broadcast :: C.MVar State -> Text -> IO ()
broadcast stateMVar message = do
  state <- C.readMVar stateMVar
  forM_ (statePlayers state) $ \player -> handle (handler player) $ send player message
  where handler :: Player -> WS.ConnectionException -> IO ()
        handler player _ = C.modifyMVar_ stateMVar $ pure . removePlayer player
