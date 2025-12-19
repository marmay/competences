module Competences.Frontend.WebSocket
  ( WebSocketConnection
  , connectWebSocket
  , sendMessage
  , getJWTToken
  )
where

import Competences.Protocol (ClientMessage, ServerMessage)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Language.Javascript.JSaddle
  ( JSM
  , JSVal
  , fun
  , jsg
  , new
  , toJSVal
  , valToText
  , (!), (#), (<#)
  )

-- | Represents a WebSocket connection
newtype WebSocketConnection = WebSocketConnection JSVal

-- | Get JWT token from window.COMPETENCES_JWT
getJWTToken :: JSM (Maybe Text)
getJWTToken = do
  maybeToken <- jsg ("window" :: Text) ! ("COMPETENCES_JWT" :: Text)
  valToText maybeToken >>= \case
    "" -> pure Nothing
    token -> pure (Just token)

-- | Connect to WebSocket server with JWT token
connectWebSocket ::
  Text ->
  Text ->
  (ServerMessage -> JSM ()) ->
  JSM WebSocketConnection
connectWebSocket wsUrl jwtToken onMessage = do
  -- Build WebSocket URL with token query parameter
  let url = wsUrl <> "?token=" <> jwtToken

  -- Create WebSocket connection using 'new' constructor
  ws <- new (jsg ("WebSocket" :: Text)) [toJSVal url]

  -- Set up onmessage handler
  _ <- ws <# ("onmessage" :: Text) $ fun $ \_ _ args -> do
    case args of
      (msgEvent : _) -> do
        msgData <- msgEvent ! ("data" :: Text)
        msgText <- valToText msgData
        case decode (BL.fromStrict $ encodeUtf8 msgText) of
          Nothing -> liftIO $ putStrLn $ "Failed to decode message: " <> T.unpack msgText
          Just serverMsg -> onMessage serverMsg
      _ -> pure ()

  -- Set up onopen handler
  _ <- ws <# ("onopen" :: Text) $ fun $ \_ _ _ -> do
    liftIO $ putStrLn "WebSocket connected"

  -- Set up onerror handler
  _ <- ws <# ("onerror" :: Text) $ fun $ \_ _ _ -> do
    liftIO $ putStrLn "WebSocket error"

  -- Set up onclose handler
  _ <- ws <# ("onclose" :: Text) $ fun $ \_ _ _ -> do
    liftIO $ putStrLn "WebSocket closed"

  pure $ WebSocketConnection ws

-- | Send a ClientMessage over the WebSocket
sendMessage :: WebSocketConnection -> ClientMessage -> JSM ()
sendMessage (WebSocketConnection ws) msg = do
  let jsonStr = decodeUtf8 $ BL.toStrict $ encode msg
  jsonVal <- toJSVal jsonStr
  _ <- ws # ("send" :: Text) $ [jsonVal]
  pure ()
