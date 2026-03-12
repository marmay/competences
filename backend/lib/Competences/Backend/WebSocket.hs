module Competences.Backend.WebSocket
  ( wsHandler
  , handleClient
  , extractUserFromJWT'
  )
where

import Competences.Backend.Auth (JWTSecret, extractUserFromJWT, validateJWT)
import Competences.Backend.BuildInfo (backendVersion)
import Competences.Backend.CAS qualified as CAS
import Competences.Backend.Checkpoint (computeDocumentChecksum)
import Competences.Backend.CommandLog qualified as CL
import Competences.Backend.Database qualified as DB
import Competences.Backend.State
  ( AppState (..)
  , ConnectionId
  , getDocument
  , registerClient
  , unregisterClient
  , updateDocument
  )
import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserId, UserRole (..), projectDocument)
import Competences.Document.FileRef (FileData (..), FileRef (..))
import Competences.Document.User (Office365Id)
import Competences.Protocol (CommandId, ClientMessage (..), ServerInfo (..), ServerMessage (..))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (atomically, readTVarIO, TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, unless)
import Data.Binary (decodeOrFail)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

-- | Number of commands at which we send a checksum for checkpoint storage.
checksumInterval :: Int
checksumInterval = 50

-- | Maximum commands for incremental sync before falling back to snapshot.
maxIncrementalCommands :: Int
maxIncrementalCommands = 500

-- | Maximum file upload size in bytes (10 MB).
maxUploadSize :: Int64
maxUploadSize = 10 * 1024 * 1024

-- | WebSocket application handler.
-- Accepts connection first, then waits for authentication message.
wsHandler :: AppState -> JWTSecret -> WS.ServerApp
wsHandler state jwtSecret pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    putStrLn "Waiting for authentication message..."
    authMsg <- WS.receiveData conn
    case decodeOrFail authMsg of
      Right (_, _, Authenticate token _clientInfo mImpersonate) ->
        authenticateAndHandle state jwtSecret conn token mImpersonate
      Right (_, _, _otherMsg) -> do
        putStrLn "First message must be Authenticate"
        WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "First message must be authentication")
      Left (_, _, err) -> do
        putStrLn $ "Invalid message format for authentication: " <> err
        WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "Invalid message format")

-- | Shared authentication logic.
authenticateAndHandle
  :: AppState -> JWTSecret -> WS.Connection -> Text -> Maybe UserId -> IO ()
authenticateAndHandle state jwtSecret conn token mImpersonate =
  case extractUserFromJWT' jwtSecret token of
    Left err -> do
      putStrLn $ "Authentication failed: " <> err
      WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed $ T.pack err)
    Right (userId, userName, userRole, o365Id) -> do
      let user = User userId userName userRole o365Id
      case mImpersonate of
        Nothing -> do
          putStrLn $ "Authentication successful for: " <> T.unpack userName
          -- Send Authenticated
          WS.sendBinaryData conn (Bin.encode $ Authenticated user (ServerInfo backendVersion))
          handleClient state userId user conn
        Just targetUserId -> do
          if userRole /= Teacher
            then do
              putStrLn $ "Impersonation rejected: user " <> T.unpack userName <> " is not a teacher"
              WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "Only teachers can impersonate")
            else do
              doc <- getDocument state
              case Ix.getOne (doc.users Ix.@= targetUserId) of
                Nothing -> do
                  putStrLn $ "Impersonation rejected: target user not found: " <> show targetUserId
                  WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "Target user not found")
                Just targetUser -> do
                  putStrLn $ "Impersonation: " <> T.unpack userName <> " viewing as " <> T.unpack targetUser.name
                  WS.sendBinaryData conn (Bin.encode $ Authenticated targetUser (ServerInfo backendVersion))
                  handleClient state targetUser.id targetUser conn

-- | Validate JWT and extract user information
extractUserFromJWT' :: JWTSecret -> Text -> Either String (UserId, Text, UserRole, Office365Id)
extractUserFromJWT' jwtSecret token = do
  claims <- validateJWT jwtSecret token
  extractUserFromJWT claims

-- | Handle a single client connection after authentication.
-- Protocol: wait for SubscribeFrom, sync, then enter operation loop with sender thread.
handleClient :: AppState -> UserId -> User -> WS.Connection -> IO ()
handleClient state uid user conn = do
  putStrLn $ "Client connected: " <> T.unpack user.name <> " (" <> show uid <> ")"

  -- Wait for SubscribeFrom (first message after auth)
  mSubscribe <- waitForSubscribe conn
  case mSubscribe of
    Nothing -> putStrLn $ "Client " <> show uid <> " did not send SubscribeFrom, disconnecting"
    Just mCommandId -> do
      -- Perform initial sync
      syncResult <- performSync state user conn mCommandId
      case syncResult of
        Nothing -> putStrLn $ "Sync failed for " <> show uid <> ", disconnecting"
        Just syncedGen -> do
          -- Wait for ACK from client
          mAck <- waitForSubscribe conn
          case mAck of
            Nothing -> putStrLn $ "Client " <> show uid <> " did not ACK sync, disconnecting"
            Just _ackCmdId -> do
              -- Register client AFTER handshake
              connId <- registerClient state uid user conn

              -- Create sender thread state
              ackSignal <- newEmptyTMVarIO
              positionRef <- newIORef syncedGen

              -- Signal sender to start (initial ACK)
              atomically $ putTMVar ackSignal ()

              -- Fork sender thread
              senderTid <- forkIO $
                senderThread state.commandLog ackSignal user positionRef conn state

              -- Enter receive loop, cleanup on disconnect
              flip finally (cleanup uid connId senderTid) $
                receiveLoop state uid user conn ackSignal positionRef
  where
    cleanup :: UserId -> ConnectionId -> ThreadId -> IO ()
    cleanup userId cid senderTid = do
      putStrLn $ "Client disconnected: " <> show userId <> " (" <> show cid <> ")"
      killThread senderTid
      unregisterClient state userId cid

-- | Wait for a SubscribeFrom message from the client.
-- Returns Nothing if client sends something unexpected or disconnects.
waitForSubscribe :: WS.Connection -> IO (Maybe (Maybe CommandId))
waitForSubscribe conn = do
  result <- try $ WS.receiveData conn
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right msg -> case decodeOrFail msg of
      Right (_, _, SubscribeFrom mCmdId) -> pure (Just mCmdId)
      _ -> pure Nothing

-- | Perform initial sync: send SnapshotUpdate or CommandUpdate.
-- Returns the generation we synced up to, or Nothing on failure.
performSync :: AppState -> User -> WS.Connection -> Maybe CommandId -> IO (Maybe Int64)
performSync state user conn mCommandId = case mCommandId of
  Nothing -> do
    -- Fresh client: send full snapshot
    sendSnapshot state user conn
  Just cmdId -> do
    -- Returning client: try incremental
    mGen <- CL.lookupCommandGeneration state.commandLog cmdId
    case mGen of
      Nothing -> do
        putStrLn $ "CommandId not found, sending full snapshot for " <> T.unpack user.name
        sendSnapshot state user conn
      Just gen -> do
        -- Count commands
        count <- DB.countCommandsForUser state.dbPool user.role user.id gen
        if count > maxIncrementalCommands
          then do
            putStrLn $ "Too many commands (" <> show count <> "), sending snapshot for " <> T.unpack user.name
            sendSnapshot state user conn
          else do
            -- Load commands for this user since the given generation
            cmdsWithId <- DB.loadCommandsForUser state.dbPool user.role user.id gen
            let cmds = [(cid, cmd) | (cid, _gen, cmd) <- cmdsWithId]
            case cmds of
              [] -> do
                -- No new commands, send snapshot (edge case: client is up to date)
                -- We still need to send something. Use the same cmdId.
                currentGen <- readTVarIO state.currentGeneration
                doc <- getDocument state
                let projectedDoc = projectDocument user doc
                    mChecksum = computeChecksum count projectedDoc
                WS.sendBinaryData conn (Bin.encode $ CommandUpdate cmdId [] mChecksum)
                pure (Just currentGen)
              _ -> do
                let lastCmdId = fst (last cmds)
                    commands = map snd cmds
                -- Compute checksum if enough commands
                doc <- getDocument state
                let projectedDoc = projectDocument user doc
                    mChecksum = computeChecksum count projectedDoc
                putStrLn $ "Incremental sync: sending " <> show (length commands) <> " commands to " <> T.unpack user.name
                WS.sendBinaryData conn (Bin.encode $ CommandUpdate lastCmdId commands mChecksum)
                currentGen <- readTVarIO state.currentGeneration
                pure (Just currentGen)

-- | Send a full projected document snapshot.
sendSnapshot :: AppState -> User -> WS.Connection -> IO (Maybe Int64)
sendSnapshot state user conn = do
  doc <- getDocument state
  let projectedDoc = projectDocument user doc
      checksum = computeDocumentChecksum projectedDoc
  currentGen <- readTVarIO state.currentGeneration
  mLatestCmdId <- CL.getLatestCommandId state.commandLog
  case mLatestCmdId of
    Nothing -> do
      -- Empty database case - we need a CommandId.
      -- This should not happen in practice since startup always creates commands.
      putStrLn $ "Warning: no commands in database, cannot send snapshot to " <> T.unpack user.name
      pure Nothing
    Just latestCmdId -> do
      putStrLn $ "Sending full snapshot to " <> T.unpack user.name
      WS.sendBinaryData conn (Bin.encode $ SnapshotUpdate latestCmdId projectedDoc (Just checksum))
      pure (Just currentGen)

-- | Compute a checksum if command count is at a checkpoint interval boundary.
computeChecksum :: Int -> Document -> Maybe Text
computeChecksum count doc
  | count > 0 && count `mod` checksumInterval == 0 = Just (computeDocumentChecksum doc)
  | otherwise = Nothing

-- | Sender thread: reads from CommandLog, sends CommandUpdate, waits for ACK.
senderThread :: CL.CommandLog -> TMVar () -> User -> IORef Int64 -> WS.Connection -> AppState -> IO ()
senderThread cl ackSignal user positionRef conn state = do
  -- Wait for initial ACK before starting
  atomically $ takeTMVar ackSignal
  go
  where
    go = do
      pos <- readIORef positionRef

      -- Block until new commands exist past our position
      atomically $ CL.waitForNewCommands cl pos

      -- Read and filter commands for this user
      (newPos, cmds) <- CL.readCommandsSince cl user.role user.id pos
      writeIORef positionRef newPos

      unless (null cmds) $ do
        let lastCmdId = fst (last cmds)
            commands = map snd cmds
        -- Optionally compute checksum
        doc <- getDocument state
        let projectedDoc = projectDocument user doc
            cmdCount = length commands
            mChecksum = computeChecksum cmdCount projectedDoc
        result <- try $ WS.sendBinaryData conn (Bin.encode $ CommandUpdate lastCmdId commands mChecksum)
        case result of
          Left (_ :: SomeException) -> pure ()  -- Connection dead, thread will be killed
          Right () -> do
            -- Wait for client ACK
            atomically $ takeTMVar ackSignal

      go

-- | Main receive loop after sync is complete.
receiveLoop :: AppState -> UserId -> User -> WS.Connection -> TMVar () -> IORef Int64 -> IO ()
receiveLoop state uid user conn ackSignal positionRef = forever $ do
  msg <- WS.receiveData conn
  case decodeOrFail msg of
    Left (_, _, err) ->
      putStrLn $ "Invalid message format from " <> show uid <> ": " <> err <> ", ignoring"
    Right (_, _, clientMsg) ->
      handleClientMessage state uid user clientMsg conn ackSignal positionRef

-- | Handle individual client messages during operation.
handleClientMessage :: AppState -> UserId -> User -> ClientMessage -> WS.Connection -> TMVar () -> IORef Int64 -> IO ()
handleClientMessage state uid user clientMsg conn ackSignal positionRef = case clientMsg of
  Authenticate {} ->
    putStrLn $ "Unexpected Authenticate message from " <> show uid <> " (already authenticated)"

  SubscribeFrom mCmdId -> do
    -- This is an ACK from the client
    case mCmdId of
      Just cmdId -> do
        -- Look up generation and update position
        mGen <- CL.lookupCommandGeneration state.commandLog cmdId
        case mGen of
          Just gen -> writeIORef positionRef gen
          Nothing -> pure ()  -- Unknown cmdId, keep current position
      Nothing -> pure ()  -- Full resync requested - handled elsewhere
    -- Signal sender thread to proceed
    _ <- try @SomeException $ atomically $ putTMVar ackSignal ()
    pure ()

  SendCommand cmd -> do
    putStrLn $ "Received command from " <> show uid <> ": " <> show cmd
    if not (isAuthorized user.role cmd)
      then do
        putStrLn $ "Command rejected: user " <> show uid <> " is not authorized"
        WS.sendBinaryData conn (Bin.encode $ CommandRejected cmd "Not authorized for this command")
      else do
        result <- updateDocument state uid cmd
        case result of
          Left err -> do
            putStrLn $ "Command rejected: " <> T.unpack err
            WS.sendBinaryData conn (Bin.encode $ CommandRejected cmd err)
          Right (_, _, _cmdId, _gen) ->
            -- Command applied and appended to CommandLog.
            -- Sender threads will pick it up and deliver to clients.
            putStrLn "Command applied successfully"

  KeepAlive ->
    WS.sendBinaryData conn (Bin.encode KeepAliveResponse)

  RequestFile hash -> do
    putStrLn $ "File requested: " <> show hash
    mContents <- CAS.fetchFile state.cas hash
    case mContents of
      Nothing -> WS.sendBinaryData conn (Bin.encode $ FileNotFound hash)
      Just contents ->
        WS.sendBinaryData conn (Bin.encode $ FileContents hash (FileData contents))

  UploadFile fileName mimeType (FileData contents) -> do
    let contentSize = BL.length contents
    putStrLn $ "File upload: " <> T.unpack fileName <> " (" <> T.unpack mimeType <> ", " <> show contentSize <> " bytes)"
    if contentSize > maxUploadSize
      then WS.sendBinaryData conn (Bin.encode $ FileUploadFailed $
             "File too large (" <> T.pack (show contentSize) <> " bytes, max " <> T.pack (show maxUploadSize) <> ")")
      else do
        (sha, fileSize) <- CAS.storeAndRegister state.cas state.instanceId contents
        let fileRef = FileRef
              { hash = sha
              , fileName = fileName
              , mimeType = mimeType
              , fileSize = fileSize
              }
        WS.sendBinaryData conn (Bin.encode $ FileUploaded fileRef)

  RequestUploadPermission _fileName _mimeType fileSize -> do
    if fileSize > maxUploadSize
      then WS.sendBinaryData conn (Bin.encode $ UploadDenied $
             "File too large (" <> T.pack (show fileSize) <> " bytes, max " <> T.pack (show maxUploadSize) <> ")")
      else WS.sendBinaryData conn (Bin.encode UploadPermitted)

-- | Check if a user role is authorized to execute a command.
isAuthorized :: UserRole -> Command -> Bool
isAuthorized Teacher _ = True
isAuthorized Student (Submissions _) = True
isAuthorized Student _ = False
