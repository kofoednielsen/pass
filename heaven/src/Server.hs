{-# LANGUAGE OverloadedStrings #-}
module Server (main) where

import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Control.Concurrent as C
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import Types
import Api

host :: String
host = "127.0.0.1"

port :: Int
port = 8080

type Client = (Text, WS.Connection)

-- The state kept on the server is simply a list of connected clients. We've
-- added an alias and some utility functions, so it will be easier to extend
-- this state later on.
type ServerState = [Client]

-- Create a new, initial state:
newServerState :: ServerState
newServerState = []

-- -- Get the number of active clients:
-- numClients :: ServerState -> Int
-- numClients = length

-- Check if a user already exists (based on username):
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- Remove a client:
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Send a message to all clients, and log it on stdout:
broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
  state <- C.newMVar newServerState
  _threadId <- C.forkIO $ gameLoop state
  WS.runServer host port $ server state

-- FIXME: Support sudden disconnects with e.g. 'finally disconnect'
server :: C.MVar ServerState -> WS.ServerApp
server state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
      msg <- WS.receiveData conn
      let request = decodeText msg
      let client = (requestName request, conn)
      print request
      clients <- C.readMVar state
      case requestAction request of
        Join -> C.modifyMVar_ state $ pure . addClient client
        Leave -> C.modifyMVar_ state $ pure . removeClient client
        GoUp -> undefined
        GoDown -> undefined
        GoLeft -> undefined
        GoRight -> undefined
        Attack -> undefined

gameLoop :: C.MVar ServerState -> IO ()
gameLoop state = do
  clients <- C.readMVar state
  putStrLn ("step; current clients: " ++ show (map fst clients))
  C.threadDelay 100000
  gameLoop state
