module Competences.Frontend.WebSocket.Handlers
  ( -- * Building Blocks
    sendAuth
  , waitForAuth
  , operationLoop
    -- * Composed Handlers
  , mkInitialHandler
  , mkReconnectHandler
    -- * Re-exports for handler state
  , CommandSender
  )
where

import Competences.Command (Command)
import Competences.Document (Document, User (..), UserId)
import Competences.Document.FileRef (FileData (..), FileRef (..))
import Competences.Document.Id (idToText)
import Competences.Frontend.BuildInfo (frontendVersion)
import Competences.Frontend.IndexedDB (CheckpointData (..), IndexedDB, computeDocumentChecksum, loadCheckpoint, storeCheckpoint)
import Competences.Frontend.Logging (logInfo, logWarn)
import Competences.Frontend.FileCache qualified as FC
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , applyRemoteCommand
  , completeFileDownload
  , completeFileUpload
  , completeUploadPermission
  , mkSyncDocument
  , mkSyncDocumentEnv
  , readSyncDocument
  , rejectCommand
  , setServerInfo
  , setSyncDocument
  )
import Competences.Frontend.SyncContext.SyncDocument (SyncDocument (..), SyncDocumentEnv (..))
import Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , clearWebSocket
  , mkCommandSender
  , updateWebSocket
  )
import Competences.Frontend.WebSocket.Protocol
  ( AuthenticationException (..)
  , DisconnectedException (..)
  , WebSocket (..)
  )
import Competences.Protocol
  ( CommandId
  , ClientInfo (..)
  , ClientMessage (..)
  , ServerInfo (..)
  , ServerMessage (..)
  )
import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forever, forM_, unless, void, when)
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import UnliftIO (readIORef, writeIORef)

-- ============================================================================
-- BUILDING BLOCKS
-- ============================================================================

-- | Send authentication message with client version info.
sendAuth :: Text -> Maybe UserId -> WebSocket -> IO ()
sendAuth token mImpersonate ws = ws.send (Authenticate token clientInfo mImpersonate)
  where
    clientInfo = ClientInfo frontendVersion

-- | Wait for Authenticated response, throws AuthenticationException on failure.
waitForAuth :: WebSocket -> IO (User, ServerInfo)
waitForAuth ws = do
  msg <- ws.receive
  case msg of
    Authenticated user srvInfo -> pure (user, srvInfo)
    AuthenticationFailed reason -> throwIO (AuthenticationException reason)
    other -> throwIO (AuthenticationException $ "Unexpected message during auth: " <> T.pack (show other))

-- | Send SubscribeFrom and wait for sync response.
-- Returns (document, commandId, maybeChecksum).
data SyncResult
  = SyncSnapshot !CommandId !Document !(Maybe Text)
  | SyncIncremental !CommandId ![(UserId, Command)] !(Maybe Text)

waitForSync :: WebSocket -> IO SyncResult
waitForSync ws = do
  msg <- ws.receive
  case msg of
    SnapshotUpdate cmdId doc mChecksum -> pure (SyncSnapshot cmdId doc mChecksum)
    CommandUpdate cmdId cmds mChecksum -> pure (SyncIncremental cmdId cmds mChecksum)
    AuthenticationFailed reason -> throwIO (AuthenticationException reason)
    other -> throwIO (AuthenticationException $ "Unexpected message during sync: " <> T.pack (show other))

-- | Send SubscribeFrom as ACK.
sendAck :: Maybe CommandId -> WebSocket -> IO ()
sendAck mCmdId ws = ws.send (SubscribeFrom mCmdId)

-- | Number of commands between periodic client-side checkpoints.
checkpointInterval :: Int
checkpointInterval = 50

-- | Operation loop - runs until disconnect.
-- Handles CommandUpdate, CommandRejected, file messages.
-- Tracks command count for periodic checkpoint storage.
operationLoop :: SyncContext -> Maybe IndexedDB -> WebSocket -> IO ()
operationLoop ref mIdb ws = do
  counterRef <- newIORef (0 :: Int)
  loop counterRef `catch` handleDisconnect
  where
    handleDisconnect :: DisconnectedException -> IO ()
    handleDisconnect _ = pure ()

    loop :: IORef Int -> IO ()
    loop counterRef = forever $ do
      msg <- ws.receive
      handleMessage counterRef msg `catch` \(e :: SomeException) ->
        logWarn $ M.ms $ "Error handling message: " <> show e

    handleMessage :: IORef Int -> ServerMessage -> IO ()
    handleMessage counterRef msg = case msg of
      CommandUpdate cmdId cmds mChecksum -> do
        -- Apply each command with its original userId
        mapM_ (uncurry (applyRemoteCommand ref)) cmds
        -- Update currentCommandId
        writeIORef ref.currentCommandId (Just cmdId)
        checksumOk <- case mChecksum of
          Just checksum -> case mIdb of
            Nothing -> pure True
            Just idb -> do
              let key = checkpointKey ref.env.connectedUser.id
              ok <- validateAndStoreCheckpoint idb key checksum cmdId ref
              when ok $ writeIORef counterRef 0
              pure ok
          Nothing -> do
            -- Live update: track for periodic client-side checkpoint
            unless (null cmds) $ do
              count <- readIORef counterRef
              let newCount = count + length cmds
              writeIORef counterRef newCount
              when (newCount >= checkpointInterval) $ forM_ mIdb $ \idb -> do
                storeLocalCheckpoint idb cmdId ref
                writeIORef counterRef 0
            pure True
        -- Single ACK: resync request on mismatch, normal ACK otherwise
        sendAck (if checksumOk then Just cmdId else Nothing) ws

      CommandRejected cmd err -> do
        logWarn $ M.ms $ "Command rejected: " <> show cmd <> " - " <> T.unpack err
        rejectCommand ref cmd

      KeepAliveResponse -> pure ()

      FileContents hash fileData -> do
        logInfo $ M.ms $ "Received file: " <> show hash
        FC.insertFile ref.fileCache hash fileData
        completeFileDownload ref hash (Just ((.unFileData) fileData))

      FileNotFound hash -> do
        logWarn $ M.ms $ "File not found: " <> show hash
        completeFileDownload ref hash Nothing

      FileUploaded fileRef -> do
        logInfo $ M.ms $ "File uploaded: " <> T.unpack fileRef.fileName <> " (" <> show fileRef.hash <> ")"
        completeFileUpload ref (Right fileRef)

      FileUploadFailed reason -> do
        logWarn $ M.ms $ "File upload failed: " <> T.unpack reason
        completeFileUpload ref (Left reason)

      UploadPermitted -> do
        logInfo $ M.ms ("Upload permission granted" :: String)
        completeUploadPermission ref (Right ())

      UploadDenied reason -> do
        logWarn $ M.ms $ "Upload denied: " <> T.unpack reason
        completeUploadPermission ref (Left reason)

      SnapshotUpdate cmdId doc mChecksum -> do
        -- Full snapshot (resync or mismatch recovery)
        logInfo $ M.ms ("Received SnapshotUpdate, resyncing" :: String)
        setSyncDocument ref doc
        writeIORef ref.currentCommandId (Just cmdId)
        -- Snapshot comes directly from server — store checkpoint without validation
        forM_ mChecksum $ \checksum ->
          forM_ mIdb $ \idb -> do
            let key = checkpointKey ref.env.connectedUser.id
            storeCheckpoint idb key CheckpointData
              { document = doc
              , commandId = cmdId
              , checksum = checksum
              }
        sendAck (Just cmdId) ws

      other -> logWarn $ M.ms $ "Unexpected message during operation: " <> show other

-- | Store a local checkpoint without server checksum validation.
-- Used for periodic client-side checkpointing between server checkpoints.
storeLocalCheckpoint :: IndexedDB -> CommandId -> SyncContext -> IO ()
storeLocalCheckpoint idb cmdId ref = do
  syncDoc <- readSyncDocument ref
  checksum <- computeDocumentChecksum syncDoc.remoteDocument
  let key = checkpointKey ref.env.connectedUser.id
  storeCheckpoint idb key CheckpointData
    { document = syncDoc.remoteDocument
    , commandId = cmdId
    , checksum = checksum
    }
  logInfo $ M.ms ("Periodic checkpoint stored" :: String)

-- | Validate checksum and store checkpoint only if it matches.
-- Returns True if stored, False on mismatch.
-- On mismatch, the caller is responsible for requesting a resync.
validateAndStoreCheckpoint :: IndexedDB -> Text -> Text -> CommandId -> SyncContext -> IO Bool
validateAndStoreCheckpoint idb key serverChecksum cmdId ref = do
  syncDoc <- readSyncDocument ref
  localChecksum <- computeDocumentChecksum syncDoc.remoteDocument
  if localChecksum == serverChecksum
    then do
      storeCheckpoint idb key CheckpointData
        { document = syncDoc.remoteDocument
        , commandId = cmdId
        , checksum = serverChecksum
        }
      logInfo $ M.ms ("Checkpoint stored (checksum verified)" :: String)
      pure True
    else do
      logWarn $ M.ms ("Checksum mismatch — NOT storing checkpoint" :: String)
      pure False

-- | Compute the IndexedDB key for a user's checkpoint.
checkpointKey :: UserId -> Text
checkpointKey uid = "checkpoint:" <> idToText uid

-- | Validate sync checksum and send ACK. Returns True if checksum matched (or absent).
-- Sends a resync request (SubscribeFrom Nothing) on mismatch.
validateSyncAndAck :: Maybe IndexedDB -> Text -> Maybe Text -> CommandId -> SyncContext -> WebSocket -> IO Bool
validateSyncAndAck mIdb key mChecksum cmdId ref ws = do
  checksumOk <- case mChecksum of
    Just checksum -> case mIdb of
      Nothing -> pure True
      Just idb -> validateAndStoreCheckpoint idb key checksum cmdId ref
    Nothing -> pure True
  unless checksumOk $
    logWarn $ M.ms ("Sync checksum mismatch — requesting resync" :: String)
  sendAck (if checksumOk then Just cmdId else Nothing) ws
  pure checksumOk

-- ============================================================================
-- COMPOSED HANDLERS
-- ============================================================================

-- | Initial handler: authenticate, subscribe, create state, fork app, run operation.
mkInitialHandler
  :: Text                         -- ^ JWT token
  -> Maybe UserId                 -- ^ Impersonation target
  -> Bool                         -- ^ Whether impersonating
  -> Maybe IndexedDB              -- ^ IndexedDB handle (Nothing outside WASM)
  -> (SyncContext -> IO ())       -- ^ Fork action (starts Miso app)
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkInitialHandler token mImpersonate impersonating mIdb forkApp ws = do
  -- Create CommandSender
  sender <- mkCommandSender

  -- Authenticate
  sendAuth token mImpersonate ws
  (user, srvInfo) <- waitForAuth ws

  -- Update sender with connection
  updateWebSocket sender ws

  -- Load checkpoint from IndexedDB (if available)
  let key = checkpointKey user.id
  mCpData <- case mIdb of
    Nothing -> pure Nothing
    Just idb -> loadCheckpoint idb key

  -- Send SubscribeFrom
  let mSavedCmdId = (.commandId) <$> mCpData
  sendAck mSavedCmdId ws

  -- Wait for sync
  syncResult <- waitForSync ws

  -- Create SyncContext
  env <- mkSyncDocumentEnv user sender impersonating
  ref <- mkSyncDocument env
  setServerInfo ref srvInfo

  -- Process sync result and send ACK
  case syncResult of
    SyncSnapshot cmdId doc mChecksum -> do
      setSyncDocument ref doc
      writeIORef ref.currentCommandId (Just cmdId)
      -- Store checkpoint if checksum present
      forM_ mChecksum $ \checksum ->
        forM_ mIdb $ \idb ->
          storeCheckpoint idb key CheckpointData
            { document = doc
            , commandId = cmdId
            , checksum = checksum
            }
      sendAck (Just cmdId) ws

    SyncIncremental cmdId cmds mChecksum -> do
      -- Apply commands on top of checkpoint document
      case mCpData of
        Just cpData -> do
          setSyncDocument ref cpData.document
          mapM_ (uncurry (applyRemoteCommand ref)) cmds
        Nothing -> do
          -- Should not happen: got incremental without checkpoint
          logWarn $ M.ms ("Got incremental sync without checkpoint, commands may be lost" :: String)
          mapM_ (uncurry (applyRemoteCommand ref)) cmds
      writeIORef ref.currentCommandId (Just cmdId)
      -- Validate checksum and send ACK — request resync on mismatch
      void $ validateSyncAndAck mIdb key mChecksum cmdId ref ws

  -- Fork the Miso application
  logInfo $ M.ms $ "Starting app for user: " <> T.unpack user.name
  forkApp ref

  -- Run operation loop until disconnect
  operationLoop ref mIdb ws `finally` clearWebSocket sender

  pure (ref, sender)

-- | Reconnection handler: re-authenticate, subscribe, update state, run operation.
mkReconnectHandler
  :: Text                            -- ^ JWT token
  -> Maybe UserId                    -- ^ Impersonation target
  -> Maybe IndexedDB                 -- ^ IndexedDB handle
  -> (SyncContext, CommandSender)    -- ^ Previous state and sender
  -> WebSocket
  -> IO (SyncContext, CommandSender)
mkReconnectHandler token mImpersonate mIdb (ref, sender) ws = do
  -- Authenticate
  sendAuth token mImpersonate ws
  (_user, srvInfo) <- waitForAuth ws

  -- Update sender with new connection
  updateWebSocket sender ws

  -- Get last known command ID
  lastCmdId <- readIORef ref.currentCommandId

  -- Send SubscribeFrom
  sendAck lastCmdId ws

  -- Wait for sync
  syncResult <- waitForSync ws

  let key = checkpointKey ref.env.connectedUser.id

  -- Process sync result and send ACK
  case syncResult of
    SyncSnapshot cmdId doc mChecksum -> do
      setSyncDocument ref doc
      setServerInfo ref srvInfo
      writeIORef ref.currentCommandId (Just cmdId)
      forM_ mChecksum $ \checksum ->
        forM_ mIdb $ \idb ->
          storeCheckpoint idb key CheckpointData
            { document = doc
            , commandId = cmdId
            , checksum = checksum
            }
      sendAck (Just cmdId) ws

    SyncIncremental cmdId cmds mChecksum -> do
      setServerInfo ref srvInfo
      mapM_ (uncurry (applyRemoteCommand ref)) cmds
      writeIORef ref.currentCommandId (Just cmdId)
      -- Validate checksum and send ACK — request resync on mismatch
      void $ validateSyncAndAck mIdb key mChecksum cmdId ref ws

  logInfo "Reconnected and synchronized"

  -- Run operation loop until disconnect
  operationLoop ref mIdb ws `finally` clearWebSocket sender

  pure (ref, sender)
