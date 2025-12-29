module Competences.Backend.State
  ( AppState (..)
  , ClientConnection (..)
  , initAppState
  , loadAppState
  , saveAppState
  , getDocument
  , updateDocument
  , registerClient
  , unregisterClient
  , getConnectedClients
  , broadcastToUsers
  )
where

import Competences.Backend.Database qualified as DB
import Competences.Command (Command, handleCommand)
import Competences.Command.Common (AffectedUsers (..))
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Protocol (ServerMessage (..))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forM_, when)
import Data.Aeson (eitherDecodeFileStrict, encodeFile, encode)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Network.WebSockets qualified as WS

-- | Client connection information
data ClientConnection = ClientConnection
  { userId :: !UserId
  , user :: !User
  , connection :: !WS.Connection
  }

-- | Application state containing the document and connected clients
data AppState = AppState
  { document :: !(TVar Document)
  -- ^ Current document state
  , clients :: !(TVar (Map UserId ClientConnection))
  -- ^ Connected WebSocket clients by user ID
  , dbPool :: !(Pool Connection)
  -- ^ Database connection pool for command/snapshot persistence
  }

-- | Initialize empty application state
initAppState :: Pool Connection -> IO AppState
initAppState pool = do
  doc <- newTVarIO emptyDocument
  conns <- newTVarIO Map.empty
  pure $ AppState doc conns pool

-- | Load application state from file (deprecated - use database loading instead)
-- Returns empty state if file doesn't exist
-- NOTE: This is kept for backward compatibility but database loading is preferred
loadAppState :: FilePath -> Pool Connection -> IO AppState
loadAppState path pool = do
  docResult <- eitherDecodeFileStrict path
  doc <- case docResult of
    Left err -> do
      putStrLn $ "Warning: Could not load document from " <> path <> ": " <> err
      putStrLn "Starting with empty document"
      pure emptyDocument
    Right d -> do
      putStrLn $ "Loaded document from " <> path
      pure d
  docVar <- newTVarIO doc
  conns <- newTVarIO Map.empty
  pure $ AppState docVar conns pool

-- | Save application state to file
saveAppState :: FilePath -> AppState -> IO ()
saveAppState path state = do
  doc <- readTVarIO state.document
  encodeFile path doc
  putStrLn $ "Saved document to " <> path

-- | Get current document (read-only)
getDocument :: AppState -> IO Document
getDocument = readTVarIO . (.document)

-- | Update document by applying a command
-- Returns the new document or an error
-- Also saves command to database and triggers snapshot if needed
updateDocument :: AppState -> UserId -> Command -> IO (Either Text (Document, AffectedUsers))
updateDocument state uid cmd = do
  -- Apply command to in-memory document atomically
  result <- atomically $ do
    doc <- readTVar state.document
    case handleCommand uid cmd doc of
      Left err -> pure $ Left err
      Right (doc', affected) -> do
        writeTVar state.document doc'
        pure $ Right (doc', affected)

  -- On success, persist to database
  case result of
    Left err -> pure $ Left err
    Right (doc', affected) -> do
      -- Save command to database
      generation <- DB.saveCommand state.dbPool uid cmd

      -- Check if we should take a snapshot
      shouldSnapshot <- DB.shouldTakeSnapshot state.dbPool generation
      when shouldSnapshot $ do
        putStrLn $ "Taking snapshot at generation " <> show generation
        DB.saveSnapshot state.dbPool doc' generation

      pure $ Right (doc', affected)

-- | Register a new client connection
registerClient :: AppState -> UserId -> User -> WS.Connection -> IO ()
registerClient state uid user conn = atomically $ do
  let client = ClientConnection uid user conn
  modifyTVar' state.clients $ Map.insert uid client

-- | Unregister a client connection
unregisterClient :: AppState -> UserId -> IO ()
unregisterClient state uid = atomically $
  modifyTVar' state.clients $ Map.delete uid

-- | Get all connected clients
getConnectedClients :: AppState -> IO [ClientConnection]
getConnectedClients state =
  Map.elems <$> readTVarIO state.clients

-- | Broadcast a message to specific users
broadcastToUsers :: AppState -> [UserId] -> ServerMessage -> IO ()
broadcastToUsers state userIds msg = do
  clients <- readTVarIO state.clients
  -- Deduplicate user IDs to avoid sending the same message multiple times
  let uniqueUserIds = Map.keys $ Map.fromList [(uid, ()) | uid <- userIds]
  forM_ uniqueUserIds $ \uid ->
    case Map.lookup uid clients of
      Nothing -> pure () -- User not connected
      Just client -> WS.sendTextData client.connection (encode msg)
