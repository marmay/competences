module Competences.Frontend.SyncContext.SyncDocument
  ( -- * SyncDocument Reference
    SyncContext (..)
  , SyncDocumentEnv (..)
  , SyncDocument (..)
  , DocumentChange (..)
  , DocumentChangeInfo (..)
  , mkSyncDocument
  , mkSyncDocument'
  , subscribeDocument
  , subscribeDocumentIO
  , registerDocumentHandler
  , unregisterDocumentHandler
  , modifySyncDocument
  , setSyncDocument
  , applyRemoteCommand
  , rejectCommand
  , emptySyncDocument
  , readSyncDocument
  , setSyncDocument'
  , issueInitialUpdate
  , mkSyncDocumentEnv
  , syncDocumentEnv
  , isTeacher
  , getCommandSender
    -- * Viewer Pin Requests
  , PinViewerRequest (..)
  , requestViewerPin
  , getFocusedUserRef
  , nextId
  , isInitialUpdate
    -- * Non-optimistic Commands
  , sendCommandOnly
    -- * Rejection Notifications
  , subscribeRejections
  , notifyRejection
    -- * Upload Permission
  , requestUploadPermission
  , completeUploadPermission
    -- * File Upload
  , uploadFile
  , completeFileUpload
    -- * File Download
  , downloadFile
  , completeFileDownload
    -- * Exchange (export)
  , requestExport
  , completeExport
    -- * File Cache
  , FileCache
    -- * Server Info
  , setServerInfo
  , readServerInfo
    -- * Focused User (re-exported from UIState)
  , FocusedUserRef
  , FocusedUserState (..)
  , FocusedUserChange (..)
  , subscribeFocusedUser
  , registerFocusedUserHandler
  , unregisterFocusedUserHandler
  , setFocusedUser
  , readFocusedUser
  )
where

import Competences.Command (Command, CommandContext (..), handleCommand)
import Competences.Document (Assignment, Document, LessonNotes, Resource, Task, User (..), UserRole (..), emptyDocument)
import Competences.Document.Session (SessionId)
import Competences.Document.FileRef (FileData (..), FileRef, SHA256Hash)
import Competences.Document.Id (Id (..))
import Competences.Protocol (CommandId, ExportTarget, ServerInfo (..))
import Competences.Frontend.FileCache (FileCache, newFileCache)
import Competences.Frontend.Logging (logDebug, logError, logWarn)
import Competences.Frontend.SvgEmbed.Manager (FormulaCache, newFormulaCache)
import Competences.Frontend.SyncContext.WindowManager
  ( PinId
  , WindowEventSink
  , WindowEventSinkInstaller
  , mkWindowEventSink
  )
import Competences.Frontend.SyncContext.UIState
  ( FocusedUserChange (..)
  , FocusedUserRef
  , FocusedUserState (..)
  , mkFocusedUserRef
  , readFocusedUser
  , registerFocusedUserHandler
  , setFocusedUser
  , subscribeFocusedUser
  , unregisterFocusedUserHandler
  )
import Competences.Frontend.FileCache qualified as FC
import Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , acknowledgeCommand
  , enqueueCommand
  , getAllPending
  , getPending
  , sendCommandDirect
  , sendRequestFile
  , sendRequestExport
  , sendRequestUploadPermission
  , sendUploadFile
  )
import Control.Monad (forM_, when)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time (Day, UTCTime (..), getCurrentTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)
import Optics.Core ((&), (.~))
import System.Random (StdGen, newStdGen, random)
import UnliftIO (IORef, MVar, MonadIO, MonadUnliftIO, atomicModifyIORef', liftIO, modifyMVar, modifyMVar_, newEmptyMVar, newIORef, newMVar, readIORef, readMVar, takeMVar, tryPutMVar, writeIORef)

-- | The SyncDocument is, what is at the heart of the application. It contains the
-- entire server state regarding the competence grid model, as far as it is
-- available to the session user. It also contains the local state of the application,
-- as far as it shall be replicated to the server, i.e. everything that shall be
-- persisted.
data SyncDocument = SyncDocument
  { localDocument :: !Document
  , localChanges :: ![Command]  -- All unconfirmed commands (including in-flight)
  , remoteDocument :: !Document
  , onChanged :: !(Map.Map Int ChangedHandler)
  , nextChangedHandlerId :: !Int
  }
  deriving (Generic)

data DocumentChange = DocumentChange
  { document :: !Document
  , change :: !DocumentChangeInfo
  }
  deriving (Eq, Show, Generic)

data DocumentChangeInfo
  = InitialUpdate
  | DocumentReloaded
  | DocumentChanged Document Command
  deriving (Eq, Show, Generic)

isInitialUpdate :: DocumentChangeInfo -> Bool
isInitialUpdate InitialUpdate = True
isInitialUpdate _ = False

data ChangedHandler where
  ChangedHandler :: forall a. (DocumentChange -> a) -> (M.Sink a) -> ChangedHandler

-- | Handler for command rejection notifications.
data RejectionHandler = RejectionHandler (Command -> Text -> IO ())

data SyncContext = SyncContext
  { syncDocument :: MVar SyncDocument
  , randomGen :: MVar StdGen
  , env :: !SyncDocumentEnv
  , focusedUserRef :: !FocusedUserRef
  , windowManager :: !WindowEventSink
  , windowEventSinkInstaller :: !WindowEventSinkInstaller
  , onPinClosedRef :: !(IORef (PinId -> IO ()))
  , onPinViewerRequestRef :: !(IORef (PinViewerRequest -> IO ()))
  , serverInfoRef :: !(IORef ServerInfo)
  , currentCommandId :: !(IORef (Maybe CommandId))
  , formulaCache :: !FormulaCache
  , fileCache :: !FileCache
  , uploadPermissionCallback :: !(IORef (Maybe (Either Text () -> IO ())))
  , fileUploadCallback :: !(IORef (Maybe (Either Text FileRef -> IO ())))
  , fileDownloadResults :: !(IORef (Map.Map SHA256Hash (MVar (Maybe BL.ByteString))))
  , exportCallback :: !(IORef (Maybe (Either Text Text -> IO ())))
  , rejectionHandlers :: !(MVar (Map.Map Int RejectionHandler))
  , nextRejectionHandlerId :: !(IORef Int)
  }

-- | Get the environment from a SyncContext
syncDocumentEnv :: SyncContext -> SyncDocumentEnv
syncDocumentEnv r = r.env

-- | Whether the connected user has the 'Teacher' role.
isTeacher :: SyncContext -> Bool
isTeacher r = (syncDocumentEnv r).connectedUser.role == Teacher

data PinViewerRequest
  = PinTaskViewer !Task
  | PinResourceViewer !Resource
  | PinLessonNotesViewer !LessonNotes
  | PinAssignmentViewer !Assignment
  deriving (Eq, Show)

requestViewerPin :: SyncContext -> PinViewerRequest -> IO ()
requestViewerPin r req = do
  handler <- readIORef r.onPinViewerRequestRef
  handler req

-- | Get the CommandSender from a SyncContext
getCommandSender :: SyncContext -> CommandSender
getCommandSender r = r.env.commandSender

-- | Get the FocusedUserRef from a SyncContext
getFocusedUserRef :: SyncContext -> FocusedUserRef
getFocusedUserRef r = r.focusedUserRef

data SyncDocumentEnv = SyncDocumentEnv
  { currentDay :: !Day
  , connectedUser :: !User
  , sessionId :: !SessionId
  , commandSender :: !CommandSender  -- Reference to CommandSender for network operations
  , impersonating :: !Bool  -- Whether the teacher is impersonating a student
  }
  deriving (Generic)

mkSyncDocument :: (MonadIO m) => SyncDocumentEnv -> m SyncContext
mkSyncDocument env = do
  syncDocument <- newMVar emptySyncDocument
  randomGen <- newStdGen >>= newMVar
  focusedUser <- mkFocusedUserRef env.connectedUser
  (winMgr, installer) <- liftIO mkWindowEventSink
  onPinClosed <- newIORef (\_ -> pure ())
  onPinViewer <- newIORef (\_ -> pure ())
  srvInfo <- newIORef defaultServerInfo
  cmdIdRef <- newIORef Nothing
  fc <- liftIO newFormulaCache
  filec <- liftIO newFileCache
  upc <- newIORef Nothing
  fuc <- newIORef Nothing
  fdr <- newIORef Map.empty
  ec <- newIORef Nothing
  rh <- newMVar Map.empty
  rhId <- newIORef 0
  pure $ SyncContext syncDocument randomGen env focusedUser winMgr installer onPinClosed onPinViewer srvInfo cmdIdRef fc filec upc fuc fdr ec rh rhId

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncContext
mkSyncDocument' env rgen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar rgen
  focusedUser <- mkFocusedUserRef env.connectedUser
  (winMgr, installer) <- liftIO mkWindowEventSink
  onPinClosed <- newIORef (\_ -> pure ())
  onPinViewer <- newIORef (\_ -> pure ())
  srvInfo <- newIORef defaultServerInfo
  cmdIdRef <- newIORef Nothing
  fc <- liftIO newFormulaCache
  filec <- liftIO newFileCache
  upc <- newIORef Nothing
  fuc <- newIORef Nothing
  fdr <- newIORef Map.empty
  ec <- newIORef Nothing
  rh <- newMVar Map.empty
  rhId <- newIORef 0
  pure $ SyncContext syncDocument randomGen' env focusedUser winMgr installer onPinClosed onPinViewer srvInfo cmdIdRef fc filec upc fuc fdr ec rh rhId

-- | Request permission to upload a file. The callback is invoked with
-- Right () on UploadPermitted, or Left reason on UploadDenied.
requestUploadPermission :: SyncContext -> Text -> Text -> Int64 -> (Either Text () -> IO ()) -> IO ()
requestUploadPermission r fileName mimeType fileSize callback = do
  writeIORef r.uploadPermissionCallback (Just callback)
  sendRequestUploadPermission r.env.commandSender fileName mimeType fileSize

-- | Complete a pending upload permission request by invoking the stored callback.
-- Called from the WebSocket handler when UploadPermitted or UploadDenied is received.
completeUploadPermission :: SyncContext -> Either Text () -> IO ()
completeUploadPermission r result = do
  mCb <- readIORef r.uploadPermissionCallback
  writeIORef r.uploadPermissionCallback Nothing
  case mCb of
    Just cb -> cb result
    Nothing -> pure ()

-- | Upload a file to the server's CAS via callback.
-- The callback is invoked with the result when FileUploaded or FileUploadFailed is received.
uploadFile :: SyncContext -> Text -> Text -> BL.ByteString -> (Either Text FileRef -> IO ()) -> IO ()
uploadFile r fileName mimeType contents callback = do
  writeIORef r.fileUploadCallback (Just callback)
  sendUploadFile r.env.commandSender fileName mimeType (FileData contents)

-- | Complete a pending file upload by invoking the stored callback.
-- Called from the WebSocket handler when FileUploaded or FileUploadFailed is received.
completeFileUpload :: SyncContext -> Either Text FileRef -> IO ()
completeFileUpload r result = do
  mCb <- readIORef r.fileUploadCallback
  writeIORef r.fileUploadCallback Nothing
  case mCb of
    Just cb -> cb result
    Nothing -> pure ()

-- | Download a file from the server's CAS, using the local cache first.
-- Blocks until the file is received or not found.
downloadFile :: SyncContext -> SHA256Hash -> IO (Maybe BL.ByteString)
downloadFile r hash = do
  cached <- FC.lookupFile r.fileCache hash
  case cached of
    Just bs -> pure (Just bs)
    Nothing -> do
      resultVar <- newEmptyMVar
      pending <- readIORef r.fileDownloadResults
      writeIORef r.fileDownloadResults (Map.insert hash resultVar pending)
      sendRequestFile r.env.commandSender hash
      result <- takeMVar resultVar
      pending' <- readIORef r.fileDownloadResults
      writeIORef r.fileDownloadResults (Map.delete hash pending')
      pure result

-- | Complete a pending file download by filling the MVar with the result.
-- Called from the WebSocket handler when FileContents or FileNotFound is received.
completeFileDownload :: SyncContext -> SHA256Hash -> Maybe BL.ByteString -> IO ()
completeFileDownload r hash mData = do
  pending <- readIORef r.fileDownloadResults
  case Map.lookup hash pending of
    Just var -> do
      _ <- tryPutMVar var mData
      pure ()
    Nothing -> pure ()

-- | Request a YAML export from the server. The callback fires when
-- 'ExportText' (Right) or 'ExportFailed' (Left) arrives.
requestExport :: SyncContext -> ExportTarget -> (Either Text Text -> IO ()) -> IO ()
requestExport r target callback = do
  writeIORef r.exportCallback (Just callback)
  sendRequestExport r.env.commandSender target

-- | Complete a pending export by invoking the stored callback.
-- Called from the WebSocket handler when ExportText or ExportFailed is received.
completeExport :: SyncContext -> Either Text Text -> IO ()
completeExport r result = do
  mCb <- readIORef r.exportCallback
  writeIORef r.exportCallback Nothing
  case mCb of
    Just cb -> cb result
    Nothing -> pure ()

readSyncDocument :: (MonadIO m) => SyncContext -> m SyncDocument
readSyncDocument d = readMVar d.syncDocument

subscribeDocument :: forall a. SyncContext -> (DocumentChange -> a) -> M.Sink a -> IO ()
subscribeDocument d f s = createSub acquire release s
  where
    acquire = do
      (handlerId, initialDoc) <- registerDocumentHandler d f s
      -- Send initial notification (outside MVar lock)
      s $ f (DocumentChange initialDoc InitialUpdate)
      pure handlerId
    release = unregisterDocumentHandler d

-- | Register a document handler directly without using createSub.
-- This is for use within other subscriptions that need to compose handlers.
-- Returns (handler ID, initial document) - caller should send initial notification
-- outside this call to avoid deadlock.
registerDocumentHandler :: forall a. SyncContext -> (DocumentChange -> a) -> M.Sink a -> IO (Int, Document)
registerDocumentHandler d f s = do
  modifyMVar d.syncDocument $ \d' ->
    pure (d'{ onChanged = Map.insert d'.nextChangedHandlerId (ChangedHandler f s) d'.onChanged
            , nextChangedHandlerId = d'.nextChangedHandlerId + 1
            }
          , (d'.nextChangedHandlerId, d'.localDocument)
          )

-- | Unregister a document handler by ID.
unregisterDocumentHandler :: SyncContext -> Int -> IO ()
unregisterDocumentHandler d handlerId =
  modifyMVar_ d.syncDocument $ \d' ->
    pure d'{ onChanged = Map.delete handlerId d'.onChanged }

-- | Subscribe to document changes via IO callback (for use outside Miso components).
-- Returns an unsubscribe action. Sends the initial document immediately.
subscribeDocumentIO :: SyncContext -> (DocumentChange -> IO ()) -> IO (IO ())
subscribeDocumentIO r handler = do
  (handlerId, initialDoc) <- registerDocumentHandler r id handler
  handler (DocumentChange initialDoc InitialUpdate)
  pure (unregisterDocumentHandler r handlerId)

modifySyncDocument :: SyncContext -> Command -> IO ()
modifySyncDocument r c = do
  logDebug $ "[SyncDoc] modifySyncDocument: " <> M.ms (show c)
  -- Enqueue command and get authoritative list from CommandSender
  allPending <- enqueueCommand r.env.commandSender c
  -- Update SyncDocument with the authoritative list
  modifyMVar_ r.syncDocument $ \d -> do
    -- Replay all pending commands on remoteDocument to get localDocument
    let localCtx = CommandContext r.env.connectedUser.id r.env.sessionId
        (localDoc', validChanges) = replayLocalChanges localCtx d.remoteDocument allPending
    let d' = d
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)
    -- Log handler count for leak detection
    let handlerCount = Map.size d.onChanged
    logDebug $ "[SyncDoc] Notifying " <> M.ms handlerCount <> " handlers"
    -- Notify subscribers
    forM_ d.onChanged $
      issueDocumentChange (DocumentChange d'.localDocument (DocumentChanged d.localDocument c))
    pure d'

setSyncDocument :: SyncContext -> Document -> IO ()
setSyncDocument r m = do
  -- Get all pending commands from CommandSender (authoritative source)
  allPending <- getAllPending r.env.commandSender
  let ctx = CommandContext r.env.connectedUser.id r.env.sessionId
  modifyMVar_ r.syncDocument $ setSyncDocument' ctx allPending m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument Map.empty 0

-- | Set the sync document from server state, replaying local changes
-- Takes the authoritative list of pending commands from CommandSender
setSyncDocument' :: CommandContext -> [Command] -> Document -> SyncDocument -> IO SyncDocument
setSyncDocument' ctx allPending remoteDoc d = do
  -- Replay all pending commands on the new remoteDocument
  let (localDoc', validChanges) = replayLocalChanges ctx remoteDoc allPending

  let d' =
        d
          & (#remoteDocument .~ remoteDoc)
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

  -- Log handler count for leak detection
  let handlerCount = Map.size d.onChanged
  logDebug $ "[SyncDoc] setSyncDocument' notifying " <> M.ms handlerCount <> " handlers"
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> IO ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncContext -> IO ()
issueInitialUpdate r = do
  d <- readMVar r.syncDocument
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument InitialUpdate)

mkSyncDocumentEnv :: (MonadIO m) => User -> SessionId -> CommandSender -> Bool -> m SyncDocumentEnv
mkSyncDocumentEnv u sid sender imp = do
  d <- (.utctDay) <$> liftIO getCurrentTime
  pure $ SyncDocumentEnv d u sid sender imp

nextId :: (MonadUnliftIO m) => SyncContext -> m (Id a)
nextId r = modifyMVar r.randomGen (pure . swap . random)

-- | Apply a command from the server (echo or broadcast)
-- Updates remoteDocument and replays localChanges on top of it.
applyRemoteCommand :: SyncContext -> CommandContext -> Command -> IO ()
applyRemoteCommand d cmdCtx cmd = do
  -- Check if this is an echo of our pending command
  pending <- getPending d.env.commandSender
  let isEcho = pending == Just cmd

  -- If echo, acknowledge and get remaining pending list from CommandSender
  remainingPending <- if isEcho
    then do
      logDebug $ M.ms $ "Received echo of our command: " <> show cmd
      acknowledgeCommand d.env.commandSender
    else getAllPending d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Apply command to remoteDocument using the original issuer's userId
    remoteDoc' <- case handleCommand cmdCtx cmd syncDoc.remoteDocument of
      Left err -> do
        -- This shouldn't happen - server validated the command
        logError $ M.ms $ "Server sent invalid command: " <> show err
        pure syncDoc.remoteDocument
      Right (doc, _) -> pure doc

    -- Replay remaining pending commands on top of the new remote document
    let localCtx = CommandContext d.env.connectedUser.id d.env.sessionId
        (localDoc', validChanges) = replayLocalChanges
          localCtx
          remoteDoc'
          remainingPending

    when (length validChanges < length remainingPending) $ do
      logWarn $ M.ms $ "Conflict detected - "
        <> show (length remainingPending - length validChanges) <> " local commands were dropped"

    let syncDoc' = syncDoc
          & (#remoteDocument .~ remoteDoc')
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

    -- Log handler count for leak detection
    let handlerCount = Map.size syncDoc.onChanged
    logDebug $ "[SyncDoc] applyRemoteCommand notifying " <> M.ms handlerCount <> " handlers"
    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' (DocumentChanged syncDoc.localDocument cmd))

    pure syncDoc'

-- | Default ServerInfo used before the first handshake completes.
defaultServerInfo :: ServerInfo
defaultServerInfo = ServerInfo ""

-- | Write a new ServerInfo received from the backend.
setServerInfo :: (MonadIO m) => SyncContext -> ServerInfo -> m ()
setServerInfo r si = liftIO $ writeIORef r.serverInfoRef si

-- | Read the current ServerInfo.
readServerInfo :: (MonadIO m) => SyncContext -> m ServerInfo
readServerInfo r = liftIO $ readIORef r.serverInfoRef

-- | Replay local changes on top of a document, filtering out invalid ones
-- Returns (resulting document, valid localChanges)
replayLocalChanges :: CommandContext -> Document -> [Command] -> (Document, [Command])
replayLocalChanges ctx doc localCmds =
  foldr applyOne (doc, []) (reverse localCmds)
  where
    applyOne cmd (currentDoc, validCmds) =
      case handleCommand ctx cmd currentDoc of
        Left _err -> (currentDoc, validCmds)  -- Drop invalid command
        Right (newDoc, _) -> (newDoc, cmd : validCmds)

-- | Handle a command rejection from the server
-- Removes the rejected command from localChanges
rejectCommand :: SyncContext -> Command -> IO ()
rejectCommand d cmd = do
  -- Check if this is our pending command
  pending <- getPending d.env.commandSender
  let isOurPending = pending == Just cmd

  -- If our pending, acknowledge and get remaining; otherwise get current list
  remainingPending <- if isOurPending
    then do
      logWarn $ M.ms $ "Our command was rejected by server: " <> show cmd
      acknowledgeCommand d.env.commandSender
    else getAllPending d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Replay remaining pending commands on remote document
    let localCtx = CommandContext d.env.connectedUser.id d.env.sessionId
        (localDoc', validChanges) = replayLocalChanges
          localCtx
          syncDoc.remoteDocument
          remainingPending

    let syncDoc' = syncDoc
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' DocumentReloaded)

    pure syncDoc'

-- | Send a command to the server without applying locally.
-- Used for non-optimistic operations where the server must validate
-- before local application (e.g., lock stealing).
sendCommandOnly :: SyncContext -> Command -> IO ()
sendCommandOnly r cmd = do
  logDebug $ "[SyncDoc] sendCommandOnly: " <> M.ms (show cmd)
  sendCommandDirect r.env.commandSender cmd

-- | Subscribe to command rejection notifications.
-- Returns an unsubscribe action.
subscribeRejections :: SyncContext -> (Command -> Text -> IO ()) -> IO (IO ())
subscribeRejections r handler = do
  handlerId <- atomicModifyIORef' r.nextRejectionHandlerId $ \n -> (n + 1, n)
  modifyMVar_ r.rejectionHandlers $ pure . Map.insert handlerId (RejectionHandler handler)
  pure $ modifyMVar_ r.rejectionHandlers $ pure . Map.delete handlerId

-- | Notify all rejection subscribers about a rejected command.
-- Called from the WebSocket operation loop on CommandRejected.
notifyRejection :: SyncContext -> Command -> Text -> IO ()
notifyRejection r cmd err = do
  handlers <- readMVar r.rejectionHandlers
  forM_ handlers $ \(RejectionHandler handler) -> handler cmd err

