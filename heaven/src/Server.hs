{-# LANGUAGE OverloadedStrings #-}
module Server (main) where

-- FIXME: Copied entirely from
-- https://github.com/jaspervdj/websockets/blob/master/example/server.lhs
-- (license: BSD-3); rewrite to work with our context.

import System.IO.Unsafe

import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import Types
import Api

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

-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.
main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 8080 $ application state

-- Our main application has the type:
application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the
-- connection stays alive on some browsers.
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
      msg <- WS.receiveData conn
      let request = decodeText msg
      let client = (requestName request, conn)
      print request
      clients <- readMVar state
      case requestAction request of
        Join -> modifyMVar_ state $ pure . addClient client
        Leave -> modifyMVar_ state $ pure . removeClient client
        GoUp -> undefined
        GoDown -> undefined
        GoLeft -> undefined
        GoRight -> undefined
        Attack -> undefined

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.
talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast
    (user `mappend` ": " `mappend` msg)
