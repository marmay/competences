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

import Competences.Document (Document, User (..), UserId)
import Competences.Frontend.BuildInfo (frontendVersion)
import Competences.Frontend.Logging (logInfo, logWarn)
import Competences.Frontend.SyncContext
  ( SyncContext
  , applyRemoteCommand
  , mkSyncDocument
  , mkSyncDocumentEnv
  , rejectCommand
  , setServerInfo
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
import Competences.Protocol (ClientInfo (..), ClientMessage (..), ServerInfo (..), ServerMessage (..))
import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M

-- ============================================================================
-- BUILDING BLOCKS
-- ============================================================================

-- | Send authentication message with client version info
sendAuth :: Text -> Maybe UserId -> WebSocket -> IO ()
sendAuth token mImpersonate ws = ws.send (Authenticate token clientInfo mImpersonate)
  where
    clientInfo = ClientInfo frontendVersion

-- | Wait for InitialSnapshot, throws AuthenticationException on failure
waitForSnapshot :: WebSocket -> IO (Document, User, ServerInfo)
waitForSnapshot ws = do
  msg <- ws.receive
  case msg of
    InitialSnapshot doc user srvInfo -> pure (doc, user, srvInfo)
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
      handleMessage msg `catch` \(e :: SomeException) ->
        logWarn $ M.ms $ "Error handling message: " <> show e

    handleMessage :: ServerMessage -> IO ()
    handleMessage msg = case msg of
      ApplyCommand cmd -> applyRemoteCommand ref cmd
      CommandRejected cmd err -> do
        logWarn $ M.ms $ "Command rejected: " <> show cmd <> " - " <> T.unpack err
        rejectCommand ref cmd
      KeepAliveResponse -> pure ()
      other -> logWarn $ M.ms $ "Unexpected message during operation: " <> show other

-- ============================================================================
-- COMPOSED HANDLERS
-- ============================================================================

-- | Initial handler: authenticate, create state, fork app, run operation
-- Returns (SyncContext, CommandSender) for reconnection
mkInitialHandler
  :: Text                         -- ^ JWT token
  -> Maybe UserId                 -- ^ Impersonation target
  -> Bool                         -- ^ Whether impersonating
  -> (SyncContext -> IO ())       -- ^ Fork action (starts Miso app)
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkInitialHandler token mImpersonate impersonating forkApp ws = do
  -- Create CommandSender for safe command sending
  sender <- mkCommandSender

  -- Authenticate
  sendAuth token mImpersonate ws
  (doc, user, srvInfo) <- waitForSnapshot ws

  -- Update sender with new connection (this also notifies subscribers of Connected state)
  updateWebSocket sender ws

  -- Create SyncContext with CommandSender reference
  env <- mkSyncDocumentEnv user sender impersonating
  ref <- mkSyncDocument env
  setSyncDocument ref doc
  setServerInfo ref srvInfo

  -- Fork the Miso application
  logInfo $ M.ms $ "Starting app for user: " <> T.unpack user.name
  forkApp ref

  -- Run operation loop until disconnect, always clear sender on exit
  operationLoop ref ws `finally` clearWebSocket sender

  pure (ref, sender)

-- | Reconnection handler: re-authenticate, update state, run operation
mkReconnectHandler
  :: Text                            -- ^ JWT token
  -> Maybe UserId                    -- ^ Impersonation target
  -> (SyncContext, CommandSender)    -- ^ Previous state and sender
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkReconnectHandler token mImpersonate (ref, sender) ws = do
  -- Re-authenticate
  sendAuth token mImpersonate ws
  (doc, _user, srvInfo) <- waitForSnapshot ws

  -- Update sender with new connection (resends pending, notifies subscribers)
  updateWebSocket sender ws

  -- Update SyncDocument with new document from server
  setSyncDocument ref doc
  setServerInfo ref srvInfo

  logInfo "Reconnected and synchronized"

  -- Run operation loop until disconnect, always clear sender on exit
  operationLoop ref ws `finally` clearWebSocket sender

  pure (ref, sender)
