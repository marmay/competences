module Competences.Frontend.WebSocket.CommandSender
  ( -- * Types
    CommandSender
  , ConnectionState (..)
  , ConnectionChange (..)
    -- * Construction
  , mkCommandSender
    -- * Command Operations
  , enqueueCommand
  , acknowledgeCommand
  , getPending
  , getAllPending
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
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, forever, void)
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

-- | Internal state for pending and queued commands
-- Combined in single MVar for atomic operations
data SendState = SendState
  { pending :: !(Maybe Command)
  , queue :: ![Command]
  }
  deriving (Eq, Show, Generic)

-- | CommandSender owns the send queue, pending command, and connection state.
-- A background thread handles actual sending, triggered by signals.
-- Fully self-contained - no dependency on SyncDocument.
data CommandSender = CommandSender
  { wsRef :: !(MVar (Maybe WebSocket))
  , sendState :: !(MVar SendState)
  , sendSignal :: !(MVar ())  -- Empty = waiting, Full = wake up
  , connectionHandlers :: !(MVar (Map.Map Int ConnectionHandler))
  , nextHandlerId :: !(MVar Int)
  }

-- | Create a new CommandSender (initially disconnected)
-- Spawns a background thread that handles sending commands
mkCommandSender :: IO CommandSender
mkCommandSender = do
  wsRef' <- newMVar Nothing
  sendState' <- newMVar (SendState Nothing [])
  sendSignal' <- newEmptyMVar
  connectionHandlers' <- newMVar Map.empty
  nextHandlerId' <- newMVar 0
  let sender = CommandSender
        { wsRef = wsRef'
        , sendState = sendState'
        , sendSignal = sendSignal'
        , connectionHandlers = connectionHandlers'
        , nextHandlerId = nextHandlerId'
        }
  -- Start background sender thread
  void $ forkIO $ senderThread sender
  pure sender

-- | Enqueue a command and return the authoritative list of all pending commands.
-- The background thread will send it when ready.
enqueueCommand :: CommandSender -> Command -> IO [Command]
enqueueCommand sender cmd = do
  -- Add to queue atomically and return full pending list
  allPending <- modifyMVar sender.sendState $ \s ->
    let s' = s{queue = s.queue ++ [cmd]}
        all' = getAllPendingFromState s'
    in pure (s', all')
  -- Signal background thread to check for work
  signalSender sender
  -- Notify subscribers about pending count change
  notifyConnectionChange sender
  pure allPending

-- | Acknowledge that the pending command was confirmed (echo or reject).
-- Returns the remaining pending commands.
-- The background thread will send the next command.
acknowledgeCommand :: CommandSender -> IO [Command]
acknowledgeCommand sender = do
  -- Clear pending atomically and return remaining
  allPending <- modifyMVar sender.sendState $ \s ->
    let s' = s{pending = Nothing}
        all' = getAllPendingFromState s'
    in pure (s', all')
  -- Signal background thread to send next
  signalSender sender
  -- Notify subscribers about pending count change
  notifyConnectionChange sender
  pure allPending

-- | Get the currently pending command (for echo detection)
getPending :: CommandSender -> IO (Maybe Command)
getPending sender = (.pending) <$> readMVar sender.sendState

-- | Get all pending commands (pending + queue)
getAllPending :: CommandSender -> IO [Command]
getAllPending sender = getAllPendingFromState <$> readMVar sender.sendState

-- | Get total pending count (pending + queue length)
pendingCount :: CommandSender -> IO Int
pendingCount sender = length <$> getAllPending sender

-- | Update the WebSocket connection (called after successful authentication)
-- Resends pending command if any, then signals thread for queue
updateWebSocket :: CommandSender -> WebSocket -> IO ()
updateWebSocket sender ws = do
  modifyMVar_ sender.wsRef $ \_ -> pure (Just ws)
  -- Resend pending if any (reconnection case)
  s <- readMVar sender.sendState
  forM_ s.pending $ \cmd -> do
    M.consoleLog $ M.ms $ "Resending pending command: " <> show cmd
    ws.send (SendCommand cmd) `catch` \(_ :: SomeException) -> pure ()
  -- Signal thread to send from queue if no pending
  signalSender sender
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

-- | Get all pending commands from state (pending : queue)
getAllPendingFromState :: SendState -> [Command]
getAllPendingFromState s = case s.pending of
  Just cmd -> cmd : s.queue
  Nothing -> s.queue

-- | Signal the background thread to check for work
signalSender :: CommandSender -> IO ()
signalSender sender = void $ tryPutMVar sender.sendSignal ()

-- | Background thread that handles sending commands
-- Blocks on signal, then tries to send one command from queue
senderThread :: CommandSender -> IO ()
senderThread sender = forever $ do
  -- Block until signaled
  takeMVar sender.sendSignal
  -- Try to send from queue
  trySendFromQueue sender

-- | Try to send the next command from queue if nothing is pending
-- Atomically pops from queue and sets as pending in a single MVar operation
trySendFromQueue :: CommandSender -> IO ()
trySendFromQueue sender = do
  maybeWs <- readMVar sender.wsRef
  case maybeWs of
    Nothing -> pure ()  -- Not connected, wait for next signal
    Just ws -> do
      -- Atomically: if no pending, pop from queue and set as pending
      maybeCmd <- modifyMVar sender.sendState $ \s ->
        case s.pending of
          Just _ -> pure (s, Nothing)  -- Already have pending, wait for ack
          Nothing -> case s.queue of
            [] -> pure (s, Nothing)  -- Queue empty
            (cmd : rest) -> pure (SendState (Just cmd) rest, Just cmd)
      case maybeCmd of
        Just cmd -> do
          M.consoleLog $ M.ms $ "Sending command: " <> show cmd
          ws.send (SendCommand cmd) `catch` \(_ :: SomeException) -> pure ()
        Nothing -> pure ()

-- | Get current connection change state
getCurrentChange :: CommandSender -> IO ConnectionChange
getCurrentChange sender = do
  maybeWs <- readMVar sender.wsRef
  s <- readMVar sender.sendState
  let connState = case maybeWs of
        Just _ -> Connected
        Nothing -> Disconnected
      count = length (getAllPendingFromState s)
  pure $ ConnectionChange connState count

-- | Notify all subscribers about connection/pending change
notifyConnectionChange :: CommandSender -> IO ()
notifyConnectionChange sender = do
  change <- getCurrentChange sender
  handlers <- readMVar sender.connectionHandlers
  forM_ (Map.elems handlers) $ \(ConnectionHandler f sink) ->
    sink (f change)
