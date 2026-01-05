module Competences.Backend.WebSocket
  ( wsHandler
  , handleClient
  , extractUserFromJWT'
  )
where

import Competences.Backend.Auth (JWTSecret, extractUserFromJWT, validateJWT)
import Competences.Backend.State
  ( AppState
  , broadcastToUsers
  , getDocument
  , registerClient
  , unregisterClient
  , updateDocument
  )
import Competences.Command.Common (AffectedUsers (..))
import Competences.Document (User (..), UserId, UserRole (..), projectDocument)
import Competences.Document.User (Office365Id)
import Competences.Protocol (ClientMessage (..), ServerMessage (..))
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

-- | WebSocket application handler
-- Accepts connection first, then waits for authentication message
-- This prevents JWT from appearing in URL (server logs, browser history, proxy logs)
wsHandler :: AppState -> JWTSecret -> WS.ServerApp
wsHandler state jwtSecret pending = do
  -- Accept connection without authentication (auth will happen via first message)
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    -- Wait for authentication message as first message
    putStrLn "Waiting for authentication message..."
    authMsg <- WS.receiveData conn
    case decode authMsg of
      Just (Authenticate token) -> do
        case extractUserFromJWT' jwtSecret token of
          Left err -> do
            putStrLn $ "Authentication failed: " <> err
            WS.sendTextData conn (encode $ AuthenticationFailed $ T.pack err)
            -- Close connection after failed auth
          Right (userId, userName, userRole, o365Id) -> do
            let user = User userId userName userRole o365Id
            putStrLn $ "Authentication successful for: " <> T.unpack userName
            handleClient state userId user conn
      Just _otherMsg -> do
        putStrLn "First message must be Authenticate"
        WS.sendTextData conn (encode $ AuthenticationFailed "First message must be authentication")
      Nothing -> do
        putStrLn "Invalid message format for authentication"
        WS.sendTextData conn (encode $ AuthenticationFailed "Invalid message format")

-- | Validate JWT and extract user information
extractUserFromJWT' :: JWTSecret -> Text -> Either String (UserId, Text, UserRole, Office365Id)
extractUserFromJWT' jwtSecret token = do
  -- Validate JWT signature
  claims <- validateJWT jwtSecret token
  -- Extract user info from claims
  extractUserFromJWT claims

-- | Handle a single client connection
handleClient :: AppState -> UserId -> User -> WS.Connection -> IO ()
handleClient state uid user conn = do
  putStrLn $ "Client connected: " <> T.unpack user.name <> " (" <> show uid <> ")"

  -- Register client
  registerClient state uid user conn

  -- Send initial snapshot with authenticated user (projected based on user identity)
  doc <- getDocument state
  let projectedDoc = projectDocument user doc
  WS.sendTextData conn (encode $ InitialSnapshot projectedDoc user)

  -- Handle messages and cleanup on disconnect
  flip finally (cleanup uid) $ do
    forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Nothing -> do
          putStrLn $ "Invalid message format from " <> show uid <> ", ignoring"
          -- Ignore invalid messages rather than disconnecting
        Just clientMsg -> handleClientMessage state uid user clientMsg conn
  where
    cleanup userId = do
      putStrLn $ "Client disconnected: " <> show userId
      unregisterClient state userId

-- | Handle individual client messages
handleClientMessage :: AppState -> UserId -> User -> ClientMessage -> WS.Connection -> IO ()
handleClientMessage state uid user clientMsg conn = case clientMsg of
  Authenticate _ -> do
    -- Authentication should only happen as the first message before handleClient is called
    putStrLn $ "Unexpected Authenticate message from " <> show uid <> " (already authenticated)"
    -- Ignore - user is already authenticated

  SendCommand cmd -> do
    putStrLn $ "Received command from " <> show uid <> ": " <> show cmd
    -- Authorization check: currently all commands require Teacher role
    if user.role /= Teacher
      then do
        putStrLn $ "Command rejected: user " <> show uid <> " is not a teacher"
        WS.sendTextData conn (encode $ CommandRejected cmd "Only teachers can execute commands")
      else do
        result <- updateDocument state uid cmd
        case result of
          Left err -> do
            putStrLn $ "Command rejected: " <> T.unpack err
            WS.sendTextData conn (encode $ CommandRejected cmd err)
          Right (_, AffectedUsers affected) -> do
            putStrLn $ "Command applied, broadcasting to " <> show (length affected) <> " users"
            -- Broadcast to all affected users (including sender)
            -- Note: Command is already persisted to database in updateDocument
            broadcastToUsers state affected (ApplyCommand cmd)

  KeepAlive -> do
    -- Respond to keep-alive
    WS.sendTextData conn (encode KeepAliveResponse)
