module Competences.Backend.WebSocket
  ( wsHandler
  , handleClient
  )
where

import Competences.Backend.State
  ( AppState
  , broadcastToUsers
  , getDocument
  , registerClient
  , saveAppState
  , unregisterClient
  , updateDocument
  )
import Competences.Command (Command)
import Competences.Command.Common (AffectedUsers (..))
import Competences.Document (User (..), UserId, UserRole (..))
import Competences.Document.Id (nilId)
import Competences.Protocol (ClientMessage (..), ServerMessage (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

-- | WebSocket application handler
-- Validates JWT and delegates to handleClient
wsHandler :: AppState -> FilePath -> WS.ServerApp
wsHandler state savePath pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    -- TODO: Extract and validate JWT from request headers
    -- For now, we'll use a test user
    user <- extractUserFromRequest pending
    let uid = user.id

    handleClient state savePath uid user conn

-- | Extract user from WebSocket request (stub for JWT validation)
-- TODO: Implement JWT token extraction and validation
extractUserFromRequest :: WS.PendingConnection -> IO User
extractUserFromRequest _pending = do
  -- Placeholder: return test user
  -- In production, this will:
  -- 1. Extract JWT from request headers
  -- 2. Validate JWT signature
  -- 3. Extract user claims from JWT
  -- 4. Return authenticated user
  pure $ User nilId "Test User" Teacher Nothing

-- | Handle a single client connection
handleClient :: AppState -> FilePath -> UserId -> User -> WS.Connection -> IO ()
handleClient state savePath uid user conn = do
  putStrLn $ "Client connected: " <> T.unpack user.name <> " (" <> show uid <> ")"

  -- Register client
  registerClient state uid user conn

  -- Send initial snapshot
  doc <- getDocument state
  WS.sendTextData conn (encode $ InitialSnapshot doc)

  -- Handle messages and cleanup on disconnect
  flip finally (cleanup uid) $ do
    forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Nothing -> do
          putStrLn $ "Invalid message format from " <> show uid <> ", ignoring"
          -- Ignore invalid messages rather than disconnecting
        Just clientMsg -> handleClientMessage state savePath uid clientMsg conn
  where
    cleanup userId = do
      putStrLn $ "Client disconnected: " <> show userId
      unregisterClient state userId

-- | Handle individual client messages
handleClientMessage :: AppState -> FilePath -> UserId -> ClientMessage -> WS.Connection -> IO ()
handleClientMessage state savePath uid clientMsg conn = case clientMsg of
  SendCommand cmd -> do
    putStrLn $ "Received command from " <> show uid <> ": " <> show cmd
    result <- updateDocument state uid cmd
    case result of
      Left err -> do
        putStrLn $ "Command rejected: " <> T.unpack err
        WS.sendTextData conn (encode $ CommandRejected cmd err)
      Right (doc, AffectedUsers affected) -> do
        putStrLn $ "Command applied, broadcasting to " <> show (length affected) <> " users"
        -- Broadcast to all affected users (including sender)
        broadcastToUsers state affected (ApplyCommand cmd)
        -- Save document after successful command
        -- TODO: Make this async or batched for better performance
        void $ forkIO $ saveAppState savePath state

  KeepAlive -> do
    -- Respond to keep-alive
    WS.sendTextData conn (encode KeepAliveResponse)
