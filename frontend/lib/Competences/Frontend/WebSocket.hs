module Competences.Frontend.WebSocket
  ( WebSocketConnection
  , WebSocketCallbacks (..)
  , connectWebSocketRaw
  , sendMessage
  , getJWTToken
  )
where

import Competences.Protocol (ClientMessage (..), ServerMessage)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Miso qualified as M
import Miso.DSL
  ( JSVal
  , fromJSVal
  , jsg
  , new
  , toJSVal
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
  M.consoleLog $ "Establishing raw WebSocket connection with " <> M.ms wsUrl
  ws <- new webSocket [wsUrl]

  -- Set up onmessage handler
  _ <- ws `addEventListener` "message" $ \msgEvent -> do
    msgData <- msgEvent ! "data"
    (Just msgText) <- fromJSVal @Text msgData
    case decode (BL.fromStrict $ encodeUtf8 msgText) of
      Nothing -> M.consoleLog $ M.ms $ "Failed to decode message: " <> T.unpack msgText
      Just serverMsg -> onMessage serverMsg

  -- Set up onopen handler - NO authentication, just call callback
  _ <- ws `addEventListener` "open" $ \_ -> do
    M.consoleLog "WebSocket connected (raw, no auto-auth)"
    callbacks.onOpen

  -- Set up onerror handler
  _ <- ws `addEventListener` "error" $ \_ -> do
    M.consoleError "WebSocket error"
    callbacks.onError

  -- Set up onclose handler
  _ <- ws `addEventListener` "close" $ \_ -> do
    M.consoleLog "WebSocket closed"
    callbacks.onClose

  pure $ WebSocketConnection ws

-- | Send a ClientMessage over the WebSocket
sendMessage :: WebSocketConnection -> ClientMessage -> IO ()
sendMessage (WebSocketConnection ws) msg = do
  M.consoleLog $ M.ms $ "Going to send " <> show msg
  let jsonStr = decodeUtf8 $ BL.toStrict $ encode msg
  jsonVal <- toJSVal jsonStr
  _ <- ws # "send" $ [jsonVal]
  pure ()
