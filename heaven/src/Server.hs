{-# LANGUAGE OverloadedStrings #-}
module Server (main) where

import Data.Text (Text)
import Control.Monad (forM_)
import qualified Control.Concurrent as C
import Control.Exception (handle)

import qualified Network.WebSockets as WS

import Types
import qualified Api

host :: String
host = "127.0.0.1"

port :: Int
port = 8080

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> C.MVar ServerState -> IO ()
broadcast message state = do
  clients <- C.readMVar state
  forM_ clients $ \(client@(_, conn)) -> handle (handler client) $ WS.sendTextData conn message
  where handler :: Client -> WS.ConnectionException -> IO ()
        handler client _ = C.modifyMVar_ state $ pure . removeClient client

main :: IO ()
main = do
  state <- C.newMVar newServerState
  _threadId <- C.forkIO $ gameLoop state
  WS.runServer host port $ server state

server :: C.MVar ServerState -> WS.ServerApp
server state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ receiveLoop conn state

receiveLoop :: WS.Connection -> C.MVar ServerState -> IO ()
receiveLoop conn state = do
  msg <- WS.receiveData conn
  let request = Api.decodeText msg
  let client = (requestName request, conn)
  print request
  case requestAction request of
    Join -> do
      x <- C.modifyMVar_ state $ pure . addClient client
      -- WS.sendTextData conn (Api.encodeText dummyResponse)
      -- _threadId <- C.forkIO $ gameLoop state
      return x
    Leave -> C.modifyMVar_ state $ pure . removeClient client
    GoUp -> undefined
    GoDown -> undefined
    GoLeft -> undefined
    GoRight -> undefined
    Attack -> undefined
  receiveLoop conn state

dummyResponse :: Response
dummyResponse =
  let player = Player { playerName = "tester"
                      , playerInvincible = False
                      , playerPosition = Position { positionX = 3
                                                  , positionY = 10
                                                  }
                      , playerHealth = 0.5
                      }
      projectile = Position { positionX = 1
                            , positionY = 0
                            }
      state = State { statePlayers = [ player ]
                       , stateProjectiles = [ projectile ]
                       }
  in NewState state

gameLoop :: C.MVar ServerState -> IO ()
gameLoop state = do
  clients <- C.readMVar state
  putStrLn ("current clients: " ++ show (map fst clients))
  broadcast (Api.encodeText dummyResponse) state

  C.threadDelay 100000
  gameLoop state
