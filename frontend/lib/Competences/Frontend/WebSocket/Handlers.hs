module Competences.Frontend.WebSocket.Handlers
  ( -- * Building Blocks
    sendAuth
  , waitForSnapshot
  , operationLoop
    -- * Composed Handlers
  , mkInitialHandler
  , mkReconnectHandler
    -- * Re-exports for handler state
  , CommandSender
  )
where

import Competences.Document (Document, User (..))
import Competences.Frontend.SyncDocument
  ( SyncContext
  , applyRemoteCommand
  , mkSyncDocument
  , mkSyncDocumentEnv
  , rejectCommand
  , setSyncDocument
  )
import Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , clearWebSocket
  , mkCommandSender
  , updateWebSocket
  )
import Competences.Frontend.WebSocket.Protocol
  ( AuthenticationException (..)
  , DisconnectedException (..)
  , WebSocket (..)
  )
import Competences.Protocol (ClientMessage (..), ServerMessage (..))
import Control.Exception (catch, throwIO)
import Control.Monad (forever)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M

-- ============================================================================
-- BUILDING BLOCKS
-- ============================================================================

-- | Send authentication message
sendAuth :: Text -> WebSocket -> IO ()
sendAuth token ws = ws.send (Authenticate token)

-- | Wait for InitialSnapshot, throws AuthenticationException on failure
waitForSnapshot :: WebSocket -> IO (Document, User)
waitForSnapshot ws = do
  msg <- ws.receive
  case msg of
    InitialSnapshot doc user -> pure (doc, user)
    AuthenticationFailed reason -> throwIO (AuthenticationException reason)
    other -> throwIO (AuthenticationException $ "Unexpected message during handshake: " <> T.pack (show other))

-- | Operation loop - runs until disconnect
-- Catches DisconnectedException internally and returns cleanly
-- Note: Connection state is updated by clearWebSocket in the handlers
operationLoop :: SyncContext -> WebSocket -> IO ()
operationLoop ref ws = loop `catch` handleDisconnect
  where
    handleDisconnect :: DisconnectedException -> IO ()
    handleDisconnect _ = pure ()  -- Just return cleanly, handlers will call clearWebSocket

    loop :: IO ()
    loop = forever $ do
      msg <- ws.receive
      case msg of
        ApplyCommand cmd -> applyRemoteCommand ref cmd
        CommandRejected cmd err -> do
          M.consoleLog $ M.ms $ "Command rejected: " <> show cmd <> " - " <> T.unpack err
          rejectCommand ref cmd
        KeepAliveResponse -> pure ()
        other -> M.consoleWarn $ M.ms $ "Unexpected message during operation: " <> show other

-- ============================================================================
-- COMPOSED HANDLERS
-- ============================================================================

-- | Initial handler: authenticate, create state, fork app, run operation
-- Returns (SyncContext, CommandSender) for reconnection
mkInitialHandler
  :: Text                         -- ^ JWT token
  -> (SyncContext -> IO ())   -- ^ Fork action (starts Miso app)
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkInitialHandler token forkApp ws = do
  -- Create CommandSender for safe command sending
  sender <- mkCommandSender

  -- Authenticate
  sendAuth token ws
  (doc, user) <- waitForSnapshot ws

  -- Update sender with new connection (this also notifies subscribers of Connected state)
  updateWebSocket sender ws

  -- Create SyncContext with CommandSender reference
  env <- mkSyncDocumentEnv user sender
  ref <- mkSyncDocument env
  setSyncDocument ref doc

  -- Fork the Miso application
  M.consoleLog $ M.ms $ "Starting app for user: " <> T.unpack user.name
  forkApp ref

  -- Run operation loop until disconnect
  operationLoop ref ws

  -- Disconnect happened, clear sender
  clearWebSocket sender

  pure (ref, sender)

-- | Reconnection handler: re-authenticate, update state, run operation
mkReconnectHandler
  :: Text                            -- ^ JWT token
  -> (SyncContext, CommandSender)  -- ^ Previous state and sender
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkReconnectHandler token (ref, sender) ws = do
  -- Re-authenticate
  sendAuth token ws
  (doc, _user) <- waitForSnapshot ws

  -- Update sender with new connection (resends pending, notifies subscribers)
  updateWebSocket sender ws

  -- Update SyncDocument with new document from server
  setSyncDocument ref doc

  M.consoleLog "Reconnected and synchronized"

  -- Run operation loop until disconnect
  operationLoop ref ws

  -- Disconnect happened, clear sender
  clearWebSocket sender

  pure (ref, sender)
