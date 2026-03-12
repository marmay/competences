module Competences.Frontend.WebSocket
  ( WebSocketConnection
  , WebSocketCallbacks (..)
  , connectWebSocketRaw
  , sendMessage
  , getJWTToken
  )
where

import Competences.Frontend.BinaryFFI (arrayBufferToByteString, byteStringToUint8Array)
import Competences.Frontend.Logging (logDebug, logError, logInfo, logWarn)
import Competences.Protocol (ClientMessage (..), ServerMessage)
import Data.Binary (decodeOrFail)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Miso qualified as M
import Miso.DSL
  ( JSVal
  , fromJSVal
  , jsg
  , new
  , setField
  , (!)
  , (#)
  )
import Miso.FFI (addEventListener)

-- | Represents a WebSocket connection
newtype WebSocketConnection = WebSocketConnection JSVal

-- | Callbacks for WebSocket connection state changes
data WebSocketCallbacks = WebSocketCallbacks
  { onOpen :: IO ()
  , onClose :: IO ()
  , onError :: IO ()
  }

-- | Get JWT token from window.COMPETENCES_JWT
getJWTToken :: IO (Maybe Text)
getJWTToken = do
  jsg "window" ! "COMPETENCES_JWT" >>= fromJSVal @Text

-- | Connect to WebSocket server WITHOUT sending authentication
-- This is used by the blocking protocol layer which handles auth itself
connectWebSocketRaw
  :: Text
  -> WebSocketCallbacks
  -> (ServerMessage -> IO ())
  -> IO WebSocketConnection
connectWebSocketRaw wsUrl callbacks onMessage = do
  -- Create WebSocket connection using 'new' constructor
  webSocket <- jsg "WebSocket"
  logDebug $ "Establishing raw WebSocket connection with " <> M.ms wsUrl
  ws <- new webSocket [wsUrl]

  -- Set binaryType for ArrayBuffer reception
  setField ws ("binaryType" :: M.MisoString) ("arraybuffer" :: M.MisoString)

  -- Set up onmessage handler
  _ <- ws `addEventListener` "message" $ \msgEvent -> do
    msgData <- msgEvent ! "data"
    bytes <- arrayBufferToByteString msgData
    case decodeOrFail (BL.fromStrict bytes) of
      Left (_, _, err) -> do
        logWarn $ M.ms $ "Failed to decode binary message: " <> err
        _ <- ws # "close" $ ([] :: [JSVal])
        pure ()
      Right (_, _, serverMsg) -> onMessage serverMsg

  -- Set up onopen handler - NO authentication, just call callback
  _ <- ws `addEventListener` "open" $ \_ -> do
    logDebug "WebSocket connected (raw, no auto-auth)"
    callbacks.onOpen

  -- Set up onerror handler
  _ <- ws `addEventListener` "error" $ \_ -> do
    logError "WebSocket error"
    callbacks.onError

  -- Set up onclose handler
  _ <- ws `addEventListener` "close" $ \_ -> do
    logInfo "WebSocket closed"
    callbacks.onClose

  pure $ WebSocketConnection ws

-- | Send a ClientMessage over the WebSocket
sendMessage :: WebSocketConnection -> ClientMessage -> IO ()
sendMessage (WebSocketConnection ws) msg = do
  logDebug $ M.ms $ "Going to send " <> show msg
  let bytes = BL.toStrict $ Bin.encode msg
  jsBytes <- byteStringToUint8Array bytes
  _ <- ws # "send" $ [jsBytes]
  pure ()
