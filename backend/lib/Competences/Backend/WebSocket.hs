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
import Competences.Backend.CommandProcessor (ConnectionId (..))
import Competences.Backend.CommandProcessor qualified as CP
import Competences.Backend.Database qualified as DB
import Competences.Backend.Exchange qualified as Exchange
import Competences.Backend.SessionRegistry qualified as SR
import Competences.Backend.State
  ( AppState (..)
  , getDocument
  )
import Competences.Command (Command (..), CommandContext (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserId, UserRole (..), projectDocument)
import Competences.Document.FileRef (FileData (..), FileRef (..))
import Competences.Document.Lock (Lock, LockHolder (..))
import Competences.Document.Session (SessionId)
import Competences.Document.User (Office365Id)
import Data.Map.Strict qualified as Map
import Competences.Protocol (CommandId, ClientMessage (..), ServerInfo (..), ServerMessage (..))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( TQueue
  , TVar
  , TMVar
  , atomically
  , flushTQueue
  , newEmptyTMVarIO
  , newTQueueIO
  , newTVarIO
  , modifyTVar'
  , putTMVar
  , readTQueue
  , readTVar
  , readTVarIO
  , takeTMVar
  , writeTVar
  )
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, void, when)
import Data.Binary (decodeOrFail)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

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
      Right (_, _, Authenticate token _clientInfo sessionId mImpersonate) ->
        authenticateAndHandle state jwtSecret conn token sessionId mImpersonate
      Right (_, _, _otherMsg) -> do
        putStrLn "First message must be Authenticate"
        WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "First message must be authentication")
      Left (_, _, err) -> do
        putStrLn $ "Invalid message format for authentication: " <> err
        WS.sendBinaryData conn (Bin.encode $ AuthenticationFailed "Invalid message format")

-- | Shared authentication logic.
authenticateAndHandle
  :: AppState -> JWTSecret -> WS.Connection -> Text -> SessionId -> Maybe UserId -> IO ()
authenticateAndHandle state jwtSecret conn token sessionId mImpersonate =
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
          handleClient state userId sessionId user conn
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
                  handleClient state targetUser.id sessionId targetUser conn

-- | Validate JWT and extract user information
extractUserFromJWT' :: JWTSecret -> Text -> Either String (UserId, Text, UserRole, Office365Id)
extractUserFromJWT' jwtSecret token = do
  claims <- validateJWT jwtSecret token
  extractUserFromJWT claims

-- | Handle a single client connection after authentication.
-- Protocol: wait for SubscribeFrom, sync, then enter operation loop with sender thread.
handleClient :: AppState -> UserId -> SessionId -> User -> WS.Connection -> IO ()
handleClient state uid sessionId user conn = do
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
        Just _syncedGen -> do
          -- Wait for ACK, handling resync requests (max 3 attempts)
          let waitForConfirmedAck (0 :: Int) = do
                putStrLn $ "Client " <> show uid <> " exceeded max resync attempts, disconnecting"
                pure False
              waitForConfirmedAck attemptsLeft = do
                mAck <- waitForSubscribe conn
                case mAck of
                  Nothing -> do
                    putStrLn $ "Client " <> show uid <> " did not ACK sync, disconnecting"
                    pure False
                  Just Nothing -> do
                    -- Client requested resync (checksum mismatch)
                    putStrLn $ "Client " <> show uid <> " requested resync after initial sync"
                    resyncResult <- sendSnapshot state user conn
                    case resyncResult of
                      Nothing -> do
                        putStrLn $ "Resync failed for " <> show uid
                        pure False
                      Just _ -> waitForConfirmedAck (attemptsLeft - 1)
                  Just (Just _cmdId) ->
                    pure True -- Normal ACK, proceed
          confirmed <- waitForConfirmedAck (3 :: Int)
          when confirmed $ do
              -- Allocate a unique connection ID
              connId <- atomically $ do
                cid <- readTVar state.nextConnectionId
                modifyTVar' state.nextConnectionId (+ 1)
                pure (ConnectionId cid)

              -- Register session and create per-client queue
              SR.registerSession state.sessionRegistry sessionId uid connId
              clientQueue <- newTQueueIO
              CP.registerClient state.processor connId user.role uid clientQueue

              -- Create sender thread state
              ackSignal <- newEmptyTMVarIO
              resyncFlag <- newTVarIO False

              -- Signal sender to start (initial ACK)
              atomically $ putTMVar ackSignal ()

              -- Fork sender thread
              senderTid <- forkIO $
                senderThread clientQueue ackSignal resyncFlag user state conn

              -- Enter receive loop, cleanup on disconnect
              flip finally (cleanup uid connId senderTid) $
                receiveLoop state uid sessionId user conn ackSignal resyncFlag
  where
    cleanup :: UserId -> ConnectionId -> ThreadId -> IO ()
    cleanup userId cid senderTid = do
      putStrLn $ "Client disconnected: " <> show userId <> " (" <> show cid <> ")"
      killThread senderTid
      CP.unregisterClient state.processor cid
      SR.unregisterConnection state.sessionRegistry sessionId cid

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
-- Always includes a checksum for checkpoint storage.
-- Returns the generation we synced up to, or Nothing on failure.
performSync :: AppState -> User -> WS.Connection -> Maybe CommandId -> IO (Maybe Int64)
performSync state user conn mCommandId = case mCommandId of
  Nothing -> do
    -- Fresh client: send full snapshot
    sendSnapshot state user conn
  Just cmdId -> do
    -- Returning client: try incremental
    mGen <- DB.lookupCommandGeneration state.dbPool cmdId
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
            let cmds = [(cid, ctx, cmd) | (cid, _gen, ctx, cmd) <- cmdsWithId]
            case cmds of
              [] -> do
                -- No new commands, send current state with checksum
                currentGen <- readTVarIO state.currentGeneration
                doc <- getDocument state
                let projectedDoc = projectDocument user doc
                    checksum = Just (computeDocumentChecksum projectedDoc)
                WS.sendBinaryData conn (Bin.encode $ CommandUpdate cmdId [] checksum)
                pure (Just currentGen)
              _ -> do
                let lastCmdId = (\(c, _, _) -> c) (last cmds)
                    userCommands = [(ctx, cmd) | (_cid, ctx, cmd) <- cmds]
                -- Always compute checksum for sync
                doc <- getDocument state
                let projectedDoc = projectDocument user doc
                    checksum = Just (computeDocumentChecksum projectedDoc)
                putStrLn $ "Incremental sync: sending " <> show (length userCommands) <> " commands to " <> T.unpack user.name
                WS.sendBinaryData conn (Bin.encode $ CommandUpdate lastCmdId userCommands checksum)
                currentGen <- readTVarIO state.currentGeneration
                pure (Just currentGen)

-- | Send a full projected document snapshot.
sendSnapshot :: AppState -> User -> WS.Connection -> IO (Maybe Int64)
sendSnapshot state user conn = do
  doc <- getDocument state
  let projectedDoc = projectDocument user doc
      checksum = computeDocumentChecksum projectedDoc
  currentGen <- readTVarIO state.currentGeneration
  mLatestCmdId <- DB.getLatestCommandId state.dbPool
  case mLatestCmdId of
    Nothing -> do
      -- Empty database case
      putStrLn $ "Warning: no commands in database, cannot send snapshot to " <> T.unpack user.name
      pure Nothing
    Just latestCmdId -> do
      putStrLn $ "Sending full snapshot to " <> T.unpack user.name
      WS.sendBinaryData conn (Bin.encode $ SnapshotUpdate latestCmdId projectedDoc (Just checksum))
      pure (Just currentGen)

-- | Sender thread: reads from per-client TQueue, sends CommandUpdate, waits for ACK.
--
-- Key STM property: reads queue items AND document TVar in a single atomically block,
-- so the document state is always consistent with the commands sent.
--
-- When the client requests a resync (SubscribeFrom Nothing), the resync flag is set.
-- After receiving the ACK, the sender flushes the queue (items are superseded by the
-- snapshot), sends a fresh SnapshotUpdate, and waits for the client to ACK it.
senderThread :: TQueue CP.ClientQueueItem -> TMVar () -> TVar Bool -> User -> AppState -> WS.Connection -> IO ()
senderThread queue ackSignal resyncFlag user state conn = do
  -- Wait for initial ACK before starting
  atomically $ takeTMVar ackSignal
  go
  where
    go = do
      -- Atomic read: batch of commands + consistent document snapshot
      (items, doc) <- atomically $ do
        first <- readTQueue queue -- blocks until item available
        rest <- flushTQueue queue -- grab any additional items
        d <- readTVar state.document
        pure (first : rest, d)

      let lastCmdId = (last items).commandId
          userCommands = [(item.context, item.command) | item <- items]
          mChecksum =
            if any (.checkpoint) items
              then Just (computeDocumentChecksum (projectDocument user doc))
              else Nothing

      result <- try $ WS.sendBinaryData conn (Bin.encode $ CommandUpdate lastCmdId userCommands mChecksum)
      case result of
        Left (_ :: SomeException) -> pure () -- Connection dead, thread will be killed
        Right () -> do
          -- Wait for client ACK and atomically check resync flag
          needsResync <- atomically $ do
            takeTMVar ackSignal
            r <- readTVar resyncFlag
            when r $ do
              writeTVar resyncFlag False
              -- Discard queued items — they're superseded by the snapshot
              void $ flushTQueue queue
            pure r
          if needsResync
            then do
              putStrLn $ "Resync requested for " <> T.unpack user.name <> ", sending snapshot"
              mGen <- sendSnapshot state user conn
              case mGen of
                Nothing -> pure () -- Failed to send snapshot, connection likely dead
                Just _ -> do
                  -- Wait for client to ACK the snapshot
                  atomically $ takeTMVar ackSignal
                  go
            else go

-- | Main receive loop after sync is complete.
receiveLoop :: AppState -> UserId -> SessionId -> User -> WS.Connection -> TMVar () -> TVar Bool -> IO ()
receiveLoop state uid sessionId user conn ackSignal resyncFlag = forever $ do
  msg <- WS.receiveData conn
  case decodeOrFail msg of
    Left (_, _, err) ->
      putStrLn $ "Invalid message format from " <> show uid <> ": " <> err <> ", ignoring"
    Right (_, _, clientMsg) ->
      handleClientMessage state uid sessionId user clientMsg conn ackSignal resyncFlag

-- | Handle individual client messages during operation.
handleClientMessage :: AppState -> UserId -> SessionId -> User -> ClientMessage -> WS.Connection -> TMVar () -> TVar Bool -> IO ()
handleClientMessage state uid sessionId user clientMsg conn ackSignal resyncFlag = case clientMsg of
  Authenticate {} ->
    putStrLn $ "Unexpected Authenticate message from " <> show uid <> " (already authenticated)"

  SubscribeFrom mCmdId -> do
    -- Distinguish resync (Nothing) from normal ACK (Just cmdId)
    case mCmdId of
      Nothing -> do
        putStrLn $ "Client " <> show uid <> " requested resync"
        atomically $ writeTVar resyncFlag True
      Just _ -> pure ()
    -- Signal sender thread to proceed in both cases
    _ <- try @SomeException $ atomically $ putTMVar ackSignal ()
    pure ()

  SendCommand cmd -> do
    putStrLn $ "Received command from " <> show uid <> ": " <> show cmd
    if not (isAuthorized user.role cmd)
      then do
        putStrLn $ "Command rejected: user " <> show uid <> " is not authorized"
        WS.sendBinaryData conn (Bin.encode $ CommandRejected cmd "Not authorized for this command")
      else do
        -- Server-side lock validation for Unlock:
        -- Unlock requires the lock holder's session to be dead.
        allowed <- case cmd of
          Unlock lock -> validateUnlock state uid lock
          _ -> pure (Right ())
        case allowed of
          Left err -> do
            putStrLn $ "Command rejected (lock validation): " <> T.unpack err
            WS.sendBinaryData conn (Bin.encode $ CommandRejected cmd err)
          Right () -> do
            result <- CP.submitCommand state.processor (CommandContext uid sessionId) cmd
            case result of
              Left err -> do
                putStrLn $ "Command rejected: " <> T.unpack err
                WS.sendBinaryData conn (Bin.encode $ CommandRejected cmd err)
              Right _cmdId ->
                -- Command applied. Processor pushes to client queues.
                -- SenderThread will deliver to this and all other affected clients.
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

  RequestExport target -> do
    doc <- getDocument state
    Exchange.handleRequestExport conn doc target

-- | Validate an Unlock command against the session registry.
-- Checks that the lock exists and the holder's session is dead.
validateUnlock :: AppState -> UserId -> Lock -> IO (Either Text ())
validateUnlock state requestingUid lock = do
  doc <- getDocument state
  case Map.lookup lock doc.locks of
    Nothing -> pure (Left "lock not held")
    Just holder -> do
      alive <- SR.isSessionAlive state.sessionRegistry holder.sessionId
      pure $ if alive
        then Left $ if holder.userId == requestingUid
          then "entity is locked in another active session"
          else "entity is locked by another user who is currently active"
        else Right ()

-- | Check if a user role is authorized to execute a command.
isAuthorized :: UserRole -> Command -> Bool
isAuthorized Teacher _ = True
isAuthorized Student (Submissions _) = True
isAuthorized Student _ = False
