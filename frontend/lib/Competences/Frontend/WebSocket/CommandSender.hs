module Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , mkCommandSender
  , sendCommand
  , updateWebSocket
  , clearWebSocket
  )
where

import Competences.Command (Command)
import Competences.Frontend.WebSocket.Protocol (WebSocket (..))
import Competences.Protocol (ClientMessage (..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, catch)

-- | Thin wrapper around WebSocket for command sending
-- No internal queue - SyncDocument.localChanges is the single source of truth
data CommandSender = CommandSender
  { wsRef :: !(MVar (Maybe WebSocket))
  }

-- | Create a new CommandSender (initially disconnected)
mkCommandSender :: IO CommandSender
mkCommandSender = CommandSender <$> newMVar Nothing

-- | Send a command over the current WebSocket
-- If disconnected or send fails, the command stays in SyncDocument.pendingCommand
sendCommand :: CommandSender -> Command -> IO ()
sendCommand sender cmd = do
  maybeWs <- readMVar sender.wsRef
  case maybeWs of
    Just ws ->
      ws.send (SendCommand cmd) `catch` \(_ :: SomeException) ->
        pure ()  -- Send failed, command stays in SyncDocument.pendingCommand
    Nothing -> pure ()  -- Not connected, command stays in queue

-- | Update the WebSocket connection
-- Called after successful authentication
updateWebSocket :: CommandSender -> WebSocket -> IO ()
updateWebSocket sender ws =
  modifyMVar_ sender.wsRef $ \_ -> pure (Just ws)
  -- No retry logic needed - SyncDocument handles via trySendNextCommand

-- | Clear the WebSocket connection (called on disconnect)
clearWebSocket :: CommandSender -> IO ()
clearWebSocket sender = modifyMVar_ sender.wsRef $ \_ -> pure Nothing
