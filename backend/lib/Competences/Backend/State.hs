module Competences.Backend.State
  ( AppState
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

import Competences.Command (Command, handleCommand)
import Competences.Command.Common (AffectedUsers (..))
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Protocol (ServerMessage (..))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict, encodeFile, encode)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
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
  }

-- | Initialize empty application state
initAppState :: IO AppState
initAppState = do
  doc <- newTVarIO emptyDocument
  conns <- newTVarIO Map.empty
  pure $ AppState doc conns

-- | Load application state from file
-- Returns empty state if file doesn't exist
loadAppState :: FilePath -> IO AppState
loadAppState path = do
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
  pure $ AppState docVar conns

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
updateDocument :: AppState -> UserId -> Command -> IO (Either Text (Document, AffectedUsers))
updateDocument state uid cmd = atomically $ do
  doc <- readTVar state.document
  case handleCommand uid cmd doc of
    Left err -> pure $ Left err
    Right (doc', affected) -> do
      writeTVar state.document doc'
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
