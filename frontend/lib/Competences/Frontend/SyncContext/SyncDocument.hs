module Competences.Frontend.SyncContext.SyncDocument
  ( -- * SyncDocument Reference
    SyncContext (..)
  , SyncDocumentEnv (..)
  , SyncDocument (..)
  , DocumentChange (..)
  , mkSyncDocument
  , mkSyncDocument'
  , subscribeDocument
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
  , getCommandSender
  , getFocusedUserRef
  , nextId
  , isInitialUpdate
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

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Document.Id (Id (..))
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
import Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , acknowledgeCommand
  , enqueueCommand
  , getAllPending
  , getPending
  )
import Control.Monad (forM_, when)
import Data.Map qualified as Map
import Data.Time (Day, UTCTime (..), getCurrentTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)
import Optics.Core ((&), (.~))
import System.Random (StdGen, newStdGen, random)
import UnliftIO (MVar, MonadIO, MonadUnliftIO, liftIO, modifyMVar, modifyMVar_, newMVar, readMVar)

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

data SyncContext = SyncContext
  { syncDocument :: MVar SyncDocument
  , randomGen :: MVar StdGen
  , env :: !SyncDocumentEnv
  , focusedUserRef :: !FocusedUserRef
  }

-- | Get the environment from a SyncContext
syncDocumentEnv :: SyncContext -> SyncDocumentEnv
syncDocumentEnv r = r.env

-- | Get the CommandSender from a SyncContext
getCommandSender :: SyncContext -> CommandSender
getCommandSender r = r.env.commandSender

-- | Get the FocusedUserRef from a SyncContext
getFocusedUserRef :: SyncContext -> FocusedUserRef
getFocusedUserRef r = r.focusedUserRef

data SyncDocumentEnv = SyncDocumentEnv
  { currentDay :: !Day
  , connectedUser :: !User
  , commandSender :: !CommandSender  -- Reference to CommandSender for network operations
  }
  deriving (Generic)

mkSyncDocument :: (MonadIO m) => SyncDocumentEnv -> m SyncContext
mkSyncDocument env = do
  syncDocument <- newMVar emptySyncDocument
  randomGen <- newStdGen >>= newMVar
  focusedUser <- mkFocusedUserRef env.connectedUser
  pure $ SyncContext syncDocument randomGen env focusedUser

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncContext
mkSyncDocument' env rgen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar rgen
  focusedUser <- mkFocusedUserRef env.connectedUser
  pure $ SyncContext syncDocument randomGen' env focusedUser

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

modifySyncDocument :: SyncContext -> Command -> IO ()
modifySyncDocument r c = do
  M.consoleLog $ "modifySyncDocument: " <> M.ms (show c)
  -- Enqueue command and get authoritative list from CommandSender
  allPending <- enqueueCommand r.env.commandSender c
  -- Update SyncDocument with the authoritative list
  modifyMVar_ r.syncDocument $ \d -> do
    -- Replay all pending commands on remoteDocument to get localDocument
    let (localDoc', validChanges) = replayLocalChanges r.env.connectedUser.id d.remoteDocument allPending
    let d' = d
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)
    -- Notify subscribers
    forM_ d.onChanged $
      issueDocumentChange (DocumentChange d'.localDocument (DocumentChanged d.localDocument c))
    pure d'

setSyncDocument :: SyncContext -> Document -> IO ()
setSyncDocument r m = do
  -- Get all pending commands from CommandSender (authoritative source)
  allPending <- getAllPending r.env.commandSender
  modifyMVar_ r.syncDocument $ setSyncDocument' r.env.connectedUser.id allPending m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument Map.empty 0

-- | Set the sync document from server state, replaying local changes
-- Takes the authoritative list of pending commands from CommandSender
setSyncDocument' :: UserId -> [Command] -> Document -> SyncDocument -> IO SyncDocument
setSyncDocument' userId allPending remoteDoc d = do
  -- Replay all pending commands on the new remoteDocument
  let (localDoc', validChanges) = replayLocalChanges userId remoteDoc allPending

  let d' =
        d
          & (#remoteDocument .~ remoteDoc)
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> IO ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncContext -> IO ()
issueInitialUpdate r = do
  d <- readMVar r.syncDocument
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument InitialUpdate)

mkSyncDocumentEnv :: (MonadIO m) => User -> CommandSender -> m SyncDocumentEnv
mkSyncDocumentEnv u sender = do
  d <- (.utctDay) <$> liftIO getCurrentTime
  pure $ SyncDocumentEnv d u sender

nextId :: (MonadUnliftIO m) => SyncContext -> m (Id a)
nextId r = modifyMVar r.randomGen (pure . swap . random)

-- | Apply a command from the server (echo or broadcast)
-- Updates remoteDocument and replays localChanges on top of it
applyRemoteCommand :: SyncContext -> Command -> IO ()
applyRemoteCommand d cmd = do
  -- Check if this is an echo of our pending command
  pending <- getPending d.env.commandSender
  let isEcho = pending == Just cmd

  -- If echo, acknowledge and get remaining pending list from CommandSender
  remainingPending <- if isEcho
    then do
      M.consoleLog $ M.ms $ "Received echo of our command: " <> show cmd
      acknowledgeCommand d.env.commandSender
    else getAllPending d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Apply command to remoteDocument
    remoteDoc' <- case handleCommand d.env.connectedUser.id cmd syncDoc.remoteDocument of
      Left err -> do
        -- This shouldn't happen - server validated the command
        M.consoleLog $ M.ms $ "ERROR: Server sent invalid command: " <> show err
        pure syncDoc.remoteDocument
      Right (doc, _) -> pure doc

    -- Replay remaining pending commands on top of the new remote document
    let (localDoc', validChanges) = replayLocalChanges
          d.env.connectedUser.id
          remoteDoc'
          remainingPending

    when (length validChanges < length remainingPending) $ do
      M.consoleLog $ M.ms $ "WARNING: Conflict detected - "
        <> show (length remainingPending - length validChanges) <> " local commands were dropped"

    let syncDoc' = syncDoc
          & (#remoteDocument .~ remoteDoc')
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' (DocumentChanged syncDoc.localDocument cmd))

    pure syncDoc'

-- | Replay local changes on top of a document, filtering out invalid ones
-- Returns (resulting document, valid localChanges)
replayLocalChanges :: UserId -> Document -> [Command] -> (Document, [Command])
replayLocalChanges userId doc localCmds =
  foldr applyOne (doc, []) (reverse localCmds)
  where
    applyOne cmd (currentDoc, validCmds) =
      case handleCommand userId cmd currentDoc of
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
      M.consoleLog $ M.ms $ "Our command was rejected by server: " <> show cmd
      acknowledgeCommand d.env.commandSender
    else getAllPending d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Replay remaining pending commands on remote document
    let (localDoc', validChanges) = replayLocalChanges
          d.env.connectedUser.id
          syncDoc.remoteDocument
          remainingPending

    let syncDoc' = syncDoc
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' DocumentReloaded)

    pure syncDoc'
