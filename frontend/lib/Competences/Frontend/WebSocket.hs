module Competences.Frontend.WebSocket
  ( WebSocketConnection
  , connectWebSocket
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

-- | Get JWT token from window.COMPETENCES_JWT
getJWTToken :: IO (Maybe Text)
getJWTToken = do
  jsg "window" ! "COMPETENCES_JWT" >>= fromJSVal @Text

-- | Connect to WebSocket server with JWT token
-- Security: Token is sent as first message after connection, NOT in URL
-- This prevents token from appearing in server logs, browser history, proxy logs
connectWebSocket
  :: Text
  -> Text
  -> (ServerMessage -> IO ())
  -> IO WebSocketConnection
connectWebSocket wsUrl jwtToken onMessage = do
  -- Connect WITHOUT token in URL (security improvement)
  -- Token will be sent as first message after connection established

  -- Create WebSocket connection using 'new' constructor
  webSocket <- jsg "WebSocket"
  M.consoleLog $ "Establishing WebSocket connection with " <> M.ms wsUrl
  ws <- new webSocket [wsUrl]

  -- Set up onmessage handler
  _ <- ws `addEventListener` "message" $ \msgEvent -> do
    msgData <- msgEvent ! "data"
    (Just msgText) <- fromJSVal @Text msgData
    case decode (BL.fromStrict $ encodeUtf8 msgText) of
      Nothing -> M.consoleLog $ M.ms $ "Failed to decode message: " <> T.unpack msgText
      Just serverMsg -> onMessage serverMsg

  -- Set up onopen handler - send authentication as first message
  _ <- ws `addEventListener` "open" $ \_ -> do
    M.consoleLog "WebSocket connected, sending authentication..."
    -- Send JWT token as first message (not in URL for security)
    let authMsg = Authenticate jwtToken
        jsonStr = decodeUtf8 $ BL.toStrict $ encode authMsg
    jsonVal <- toJSVal jsonStr
    _ <- ws # "send" $ [jsonVal]
    M.consoleLog "Authentication message sent"

  -- Set up onerror handler
  _ <- ws `addEventListener` "error" $ \_ -> do
    M.consoleError "WebSocket error"

  -- Set up onclose handler
  _ <- ws `addEventListener` "close" $ \_ -> do
    M.consoleLog "WebSocket closed"

  pure $ WebSocketConnection ws

-- | Send a ClientMessage over the WebSocket
sendMessage :: WebSocketConnection -> ClientMessage -> IO ()
sendMessage (WebSocketConnection ws) msg = do
  M.consoleLog $ M.ms $ "Going to send " <> show msg
  let jsonStr = decodeUtf8 $ BL.toStrict $ encode msg
  jsonVal <- toJSVal jsonStr
  _ <- ws # "send" $ [jsonVal]
  pure ()
