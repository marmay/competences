module Competences.Frontend.WebSocket.CommandSender
  ( -- * Types
    CommandSender
  , ConnectionState (..)
  , ConnectionChange (..)
    -- * Construction
  , mkCommandSender
    -- * Command Operations
  , pushCommand
  , acknowledgeCommand
  , getPending
  , pendingCount
    -- * Connection Operations
  , updateWebSocket
  , clearWebSocket
  , isConnected
    -- * Subscriptions
  , subscribeConnection
  )
where

import Competences.Command (Command)
import Competences.Frontend.WebSocket.Protocol (WebSocket (..))
import Competences.Protocol (ClientMessage (..))
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)

-- | Connection state for WebSocket
data ConnectionState
  = Connected
  | Disconnected
  deriving (Eq, Show, Generic)

-- | Connection change notification
data ConnectionChange = ConnectionChange
  { state :: !ConnectionState
  , pendingCount :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Handler for connection state changes
data ConnectionHandler where
  ConnectionHandler :: forall a. (ConnectionChange -> a) -> (M.Sink a) -> ConnectionHandler

-- | CommandSender owns the send queue, pending command, and connection state.
-- Fully self-contained - no dependency on SyncDocument.
data CommandSender = CommandSender
  { wsRef :: !(MVar (Maybe WebSocket))
  , pendingCommand :: !(MVar (Maybe Command))
  , commandQueue :: !(MVar [Command])
  , connectionHandlers :: !(MVar (Map.Map Int ConnectionHandler))
  , nextHandlerId :: !(MVar Int)
  }

-- | Create a new CommandSender (initially disconnected)
mkCommandSender :: IO CommandSender
mkCommandSender = do
  wsRef' <- newMVar Nothing
  pendingCommand' <- newMVar Nothing
  commandQueue' <- newMVar []
  connectionHandlers' <- newMVar Map.empty
  nextHandlerId' <- newMVar 0
  pure CommandSender
    { wsRef = wsRef'
    , pendingCommand = pendingCommand'
    , commandQueue = commandQueue'
    , connectionHandlers = connectionHandlers'
    , nextHandlerId = nextHandlerId'
    }

-- | Push a command to the queue and try to send
-- If nothing is pending and connected, sends immediately
pushCommand :: CommandSender -> Command -> IO ()
pushCommand sender cmd = do
  -- Add to queue
  modifyMVar_ sender.commandQueue $ \queue -> pure (queue ++ [cmd])
  -- Try to send
  trySendNext sender
  -- Notify pending count changed
  notifyConnectionChange sender

-- | Acknowledge that the pending command was confirmed (echo or reject)
-- Clears pending and tries to send next
acknowledgeCommand :: CommandSender -> IO ()
acknowledgeCommand sender = do
  modifyMVar_ sender.pendingCommand $ \_ -> pure Nothing
  -- Try to send next from queue
  trySendNext sender
  -- Notify pending count changed
  notifyConnectionChange sender

-- | Get the currently pending command (for echo detection)
getPending :: CommandSender -> IO (Maybe Command)
getPending sender = readMVar sender.pendingCommand

-- | Get total pending count (pending + queue length)
pendingCount :: CommandSender -> IO Int
pendingCount sender = do
  pending <- readMVar sender.pendingCommand
  queue <- readMVar sender.commandQueue
  let pendingN = case pending of
        Just _ -> 1
        Nothing -> 0
  pure (pendingN + length queue)

-- | Update the WebSocket connection (called after successful authentication)
-- Resends pending command if any
updateWebSocket :: CommandSender -> WebSocket -> IO ()
updateWebSocket sender ws = do
  modifyMVar_ sender.wsRef $ \_ -> pure (Just ws)
  -- Resend pending if any
  pending <- readMVar sender.pendingCommand
  case pending of
    Just cmd -> do
      M.consoleLog $ M.ms $ "Resending pending command: " <> show cmd
      ws.send (SendCommand cmd) `catch` \(_ :: SomeException) -> pure ()
    Nothing -> trySendNext sender
  -- Notify connection state changed
  notifyConnectionChange sender

-- | Clear the WebSocket connection (called on disconnect)
clearWebSocket :: CommandSender -> IO ()
clearWebSocket sender = do
  modifyMVar_ sender.wsRef $ \_ -> pure Nothing
  -- Notify connection state changed
  notifyConnectionChange sender

-- | Check if currently connected
isConnected :: CommandSender -> IO Bool
isConnected sender = do
  maybeWs <- readMVar sender.wsRef
  pure $ case maybeWs of
    Just _ -> True
    Nothing -> False

-- | Subscribe to connection state changes
subscribeConnection :: forall a. CommandSender -> (ConnectionChange -> a) -> M.Sink a -> IO ()
subscribeConnection sender f sink = createSub acquire release sink
  where
    acquire = do
      handlerId <- modifyMVar sender.nextHandlerId $ \hid -> pure (hid + 1, hid)
      modifyMVar_ sender.connectionHandlers $ \handlers ->
        pure $ Map.insert handlerId (ConnectionHandler f sink) handlers
      -- Send initial value
      change <- getCurrentChange sender
      sink $ f change
      pure handlerId
    release handlerId =
      modifyMVar_ sender.connectionHandlers $ \handlers ->
        pure $ Map.delete handlerId handlers

-- ============================================================================
-- INTERNAL HELPERS
-- ============================================================================

-- | Try to send the next command from queue if nothing is pending
trySendNext :: CommandSender -> IO ()
trySendNext sender = do
  maybeWs <- readMVar sender.wsRef
  case maybeWs of
    Nothing -> pure ()  -- Not connected
    Just ws -> do
      -- Try to pop from queue and send
      maybeCmd <- modifyMVar sender.pendingCommand $ \pending ->
        case pending of
          Just _ -> pure (pending, Nothing)  -- Already have pending
          Nothing -> do
            popped <- modifyMVar sender.commandQueue $ \queue ->
              case queue of
                [] -> pure ([], Nothing)
                (cmd : rest) -> pure (rest, Just cmd)
            pure (popped, popped)
      case maybeCmd of
        Just cmd -> do
          M.consoleLog $ M.ms $ "Sending command: " <> show cmd
          ws.send (SendCommand cmd) `catch` \(_ :: SomeException) -> pure ()
        Nothing -> pure ()

-- | Get current connection change state
getCurrentChange :: CommandSender -> IO ConnectionChange
getCurrentChange sender = do
  maybeWs <- readMVar sender.wsRef
  let connState = case maybeWs of
        Just _ -> Connected
        Nothing -> Disconnected
  count <- pendingCount sender
  pure $ ConnectionChange connState count

-- | Notify all subscribers about connection/pending change
notifyConnectionChange :: CommandSender -> IO ()
notifyConnectionChange sender = do
  change <- getCurrentChange sender
  handlers <- readMVar sender.connectionHandlers
  forM_ (Map.elems handlers) $ \(ConnectionHandler f sink) ->
    sink (f change)
