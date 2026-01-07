module Competences.Frontend.WebSocket.Protocol
  ( -- * Blocking WebSocket
    WebSocket (..)
  , connectBlocking
    -- * Exceptions
  , DisconnectedException (..)
  , AuthenticationException (..)
    -- * Main Entry Point
  , withWebSocket
  )
where

import Competences.Frontend.WebSocket
  ( WebSocketCallbacks (..)
  , connectWebSocketRaw
  , sendMessage
  )
import Competences.Protocol (ClientMessage, ServerMessage)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (when)
import Data.Text (Text)
import GHC.Conc (threadDelay)
import Miso qualified as M

-- | Blocking WebSocket interface
data WebSocket = WebSocket
  { send :: !(ClientMessage -> IO ())
  , receive :: !(IO ServerMessage)  -- Blocks until next message, throws on disconnect
  }

-- | Exception thrown when WebSocket disconnects (normal control flow)
data DisconnectedException = DisconnectedException
  deriving (Show)

instance Exception DisconnectedException

-- | Exception thrown on authentication failure (truly exceptional)
newtype AuthenticationException = AuthenticationException Text
  deriving (Show)

instance Exception AuthenticationException

-- | Connect to WebSocket and provide a blocking interface
-- Uses TQueue to convert callback-based WebSocket to blocking receive
-- Blocks until the connection is open before returning
-- Throws DisconnectedException if connection fails
connectBlocking :: Text -> IO WebSocket
connectBlocking url = do
  messageQueue <- newTQueueIO
  -- True = connected successfully, False = connection failed
  connectedSignal <- newEmptyMVar

  let callbacks = WebSocketCallbacks
        { onOpen = do
            _ <- tryPutMVar connectedSignal True
            pure ()
        , onClose = do
            -- Signal failure if connection never opened (tryPutMVar is idempotent)
            _ <- tryPutMVar connectedSignal False
            -- Also write to queue for receive to throw DisconnectedException
            atomically $ writeTQueue messageQueue Nothing
        , onError = do
            _ <- tryPutMVar connectedSignal False
            pure ()
        }

  let onMessage msg = atomically $ writeTQueue messageQueue (Just msg)

  rawWs <- connectWebSocketRaw url callbacks onMessage

  -- Wait for connection result
  connected <- takeMVar connectedSignal
  when (not connected) $ throwIO DisconnectedException

  pure WebSocket
    { send = sendMessage rawWs
    , receive = atomically (readTQueue messageQueue) >>= \case
        Just msg -> pure msg
        Nothing -> throwIO DisconnectedException
    }

-- | Main entry point for WebSocket protocol handling
-- Manages connection lifecycle with automatic reconnection
--
-- The initial handler runs first and should:
-- 1. Authenticate
-- 2. Create state
-- 3. Fork the application
-- 4. Run operation loop until disconnect
-- 5. Return state
--
-- The continuation handler runs on each reconnection and should:
-- 1. Re-authenticate
-- 2. Update state
-- 3. Run operation loop until disconnect
-- 4. Return state
--
-- AuthenticationException propagates up (handled by caller).
-- DisconnectedException is caught internally by handlers' operationLoop.
withWebSocket
  :: Text                           -- ^ WebSocket URL
  -> (WebSocket -> IO a)            -- ^ Initial handler
  -> (a -> WebSocket -> IO a)       -- ^ Reconnection handler
  -> IO ()
withWebSocket url initial continuation = do
  ws <- connectBlocking url
  a <- initial ws  -- May throw AuthenticationException
  reconnectLoop 0 a
  where
    reconnectLoop (attempt :: Int) a = do
      -- Exponential backoff: 1s, 2s, 4s, 8s, max 15s
      let delaySeconds = min 15 ((2 :: Int) ^ attempt)
      when (attempt > 0) $ do
        M.consoleLog $ M.ms $ "Reconnecting in " <> show delaySeconds <> "s..."
        threadDelay (delaySeconds * 1000000)

      -- Try to reconnect
      connectResult <- (Right <$> connectBlocking url) `catch` \(_ :: DisconnectedException) ->
        pure (Left ())

      case connectResult of
        Left () -> reconnectLoop (attempt + 1) a  -- Connection failed, retry
        Right ws' -> do
          -- Run continuation, may throw AuthenticationException
          a' <- continuation a ws'
          reconnectLoop 0 a'  -- Success, reset attempt counter
