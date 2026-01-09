module Competences.Frontend.SyncDocument
  ( -- * SyncDocument Reference
    SyncDocumentRef (..)
  , SyncDocumentEnv (..)
  , SyncDocument (..)
  , DocumentChange (..)
  , mkSyncDocument
  , mkSyncDocument'
  , subscribeDocument
  , modifySyncDocument
  , setSyncDocument
  , applyRemoteCommand
  , rejectCommand
  , emptySyncDocument
  , modifySyncDocument'
  , readSyncDocument
  , setSyncDocument'
  , issueInitialUpdate
  , mkSyncDocumentEnv
  , syncDocumentEnv
  , getCommandSender
  , nextId
  , isInitialUpdate
    -- * Focused User
  , FocusedUserState (..)
  , FocusedUserChange (..)
  , subscribeFocusedUser
  , setFocusedUser
  , readFocusedUser
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Document.Id (Id (..))
import Competences.Document.User (isStudent)
import Competences.Frontend.WebSocket.CommandSender
  ( CommandSender
  , acknowledgeCommand
  , getPending
  , pushCommand
  )
import Control.Monad (forM_, when)
import Data.Map qualified as Map
import Data.Time (Day, UTCTime (..), getCurrentTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)
import Optics.Core ((%~), (&), (.~))
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

-- | State for the focused user feature, bundled in a single MVar
-- to avoid race conditions between handler registration and notifications
data FocusedUserState = FocusedUserState
  { focusedUser :: !(Maybe User)
  , onFocusedUserChanged :: !(Map.Map Int FocusedUserHandler)
  , nextFocusedUserHandlerId :: !Int
  }
  deriving (Generic)

data FocusedUserHandler where
  FocusedUserHandler :: forall a. (FocusedUserChange -> a) -> (M.Sink a) -> FocusedUserHandler

data FocusedUserChange = FocusedUserChange
  { user :: !(Maybe User)
  , isInitial :: !Bool
  }
  deriving (Eq, Show, Generic)

data SyncDocumentRef = SyncDocumentRef
  { syncDocument :: MVar SyncDocument
  , randomGen :: MVar StdGen
  , env :: !SyncDocumentEnv
  , focusedUserState :: MVar FocusedUserState
  }

-- | Get the environment from a SyncDocumentRef
syncDocumentEnv :: SyncDocumentRef -> SyncDocumentEnv
syncDocumentEnv r = r.env

-- | Get the CommandSender from a SyncDocumentRef
getCommandSender :: SyncDocumentRef -> CommandSender
getCommandSender r = r.env.commandSender

data SyncDocumentEnv = SyncDocumentEnv
  { currentDay :: !Day
  , connectedUser :: !User
  , commandSender :: !CommandSender  -- Reference to CommandSender for network operations
  }
  deriving (Generic)

mkSyncDocument :: (MonadIO m) => SyncDocumentEnv -> m SyncDocumentRef
mkSyncDocument env = do
  syncDocument <- newMVar emptySyncDocument
  randomGen <- newStdGen >>= newMVar
  focusedUserState <- newMVar $ mkFocusedUserState env.connectedUser
  pure $ SyncDocumentRef syncDocument randomGen env focusedUserState

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncDocumentRef
mkSyncDocument' env rgen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar rgen
  focusedUserState <- newMVar $ mkFocusedUserState env.connectedUser
  pure $ SyncDocumentRef syncDocument randomGen' env focusedUserState

readSyncDocument :: (MonadIO m) => SyncDocumentRef -> m SyncDocument
readSyncDocument d = readMVar d.syncDocument

subscribeDocument :: forall a. SyncDocumentRef -> (DocumentChange -> a) -> M.Sink a -> IO ()
subscribeDocument d f s = createSub acquire release s
  where
    acquire = do
      modifyMVar d.syncDocument $ \d' -> do
        s $ f (DocumentChange d'.localDocument InitialUpdate)
        pure (d'{ onChanged = Map.insert d'.nextChangedHandlerId (ChangedHandler f s) d'.onChanged
                 , nextChangedHandlerId = d'.nextChangedHandlerId + 1
                 }
               , d'.nextChangedHandlerId
               )
    release changedHandlerId =
      modifyMVar_ d.syncDocument $ \d' -> do
        pure d'{ onChanged = Map.delete changedHandlerId d'.onChanged }

modifySyncDocument :: SyncDocumentRef -> Command -> IO ()
modifySyncDocument r c = do
  M.consoleLog $ "modifySyncDocument: " <> M.ms (show c)
  modifyMVar_ r.syncDocument $ modifySyncDocument' r.env.connectedUser.id c
  -- Push command to CommandSender (it handles at-most-one-in-flight)
  pushCommand r.env.commandSender c

setSyncDocument :: SyncDocumentRef -> Document -> IO ()
setSyncDocument r m = do
  -- Get pending from CommandSender to include in replay
  pending <- getPending r.env.commandSender
  modifyMVar_ r.syncDocument $ setSyncDocument' r.env.connectedUser.id pending m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument Map.empty 0

modifySyncDocument' :: UserId -> Command -> SyncDocument -> IO SyncDocument
modifySyncDocument' uId c d = do
  case handleCommand uId c d.localDocument of
    Left err -> do
      M.consoleLog $ M.ms $ "Handling command '" <> show c <> "' failed: " <> show err
      pure d
    Right m' -> do
      let d' =
            d
              & (#localChanges %~ (++ [c]))  -- Add at end (FIFO queue)
              & (#localDocument .~ fst m')
      forM_ d.onChanged $
        issueDocumentChange (DocumentChange d'.localDocument (DocumentChanged d.localDocument c))
      pure d'

-- | Set the sync document from server state, replaying local changes
-- The pending command (from CommandSender) is passed in to include in replay
setSyncDocument' :: UserId -> Maybe Command -> Document -> SyncDocument -> IO SyncDocument
setSyncDocument' userId maybePending remoteDoc d = do
  -- Replay local changes on the new remoteDocument
  -- Include pending command at the front (it was the first to be sent)
  let allLocalChanges = case maybePending of
        Nothing -> d.localChanges
        Just pending -> pending : d.localChanges
  let (localDoc', validChanges) = replayLocalChanges userId remoteDoc allLocalChanges

  let d' =
        d
          & (#remoteDocument .~ remoteDoc)
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> IO ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncDocumentRef -> IO ()
issueInitialUpdate r = do
  d <- readMVar r.syncDocument
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument InitialUpdate)

mkSyncDocumentEnv :: (MonadIO m) => User -> CommandSender -> m SyncDocumentEnv
mkSyncDocumentEnv u sender = do
  d <- (.utctDay) <$> liftIO getCurrentTime
  pure $ SyncDocumentEnv d u sender

nextId :: (MonadUnliftIO m) => SyncDocumentRef -> m (Id a)
nextId r = modifyMVar r.randomGen (pure . swap . random)

-- | Apply a command from the server (echo or broadcast)
-- Updates remoteDocument and replays localChanges on top of it
applyRemoteCommand :: SyncDocumentRef -> Command -> IO ()
applyRemoteCommand d cmd = do
  -- Check if this is an echo of our pending command
  pending <- getPending d.env.commandSender
  let isEcho = pending == Just cmd

  when isEcho $ do
    M.consoleLog $ M.ms $ "Received echo of our command: " <> show cmd
    -- Acknowledge the command (clears pending, sends next)
    acknowledgeCommand d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Apply command to remoteDocument
    remoteDoc' <- case handleCommand d.env.connectedUser.id cmd syncDoc.remoteDocument of
      Left err -> do
        -- This shouldn't happen - server validated the command
        M.consoleLog $ M.ms $ "ERROR: Server sent invalid command: " <> show err
        pure syncDoc.remoteDocument
      Right (doc, _) -> pure doc

    -- If echo, remove from head of localChanges
    let localChanges' = if isEcho
          then drop 1 syncDoc.localChanges
          else syncDoc.localChanges

    -- Replay remaining localChanges on top of the new remote document
    let (localDoc', validChanges) = replayLocalChanges
          d.env.connectedUser.id
          remoteDoc'
          localChanges'

    when (length validChanges < length localChanges') $ do
      M.consoleLog $ M.ms $ "WARNING: Conflict detected - "
        <> show (length localChanges' - length validChanges) <> " local commands were dropped"

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
rejectCommand :: SyncDocumentRef -> Command -> IO ()
rejectCommand d cmd = do
  -- Check if this is our pending command
  pending <- getPending d.env.commandSender
  let isOurPending = pending == Just cmd

  when isOurPending $ do
    M.consoleLog $ M.ms $ "Our command was rejected by server: " <> show cmd
    -- Acknowledge the command (clears pending, sends next)
    acknowledgeCommand d.env.commandSender

  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Remove the rejected command from localChanges
    let localChanges' = if isOurPending
          then drop 1 syncDoc.localChanges  -- Remove from head
          else filter (/= cmd) syncDoc.localChanges  -- Remove if found elsewhere

    -- Replay remaining local changes on remote document
    let (localDoc', validChanges) = replayLocalChanges
          d.env.connectedUser.id
          syncDoc.remoteDocument
          localChanges'

    let syncDoc' = syncDoc
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)

    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' DocumentReloaded)

    pure syncDoc'

-- ============================================================================
-- FOCUSED USER
-- ============================================================================

-- | Get initial focused user based on role
initialFocusedUser :: User -> Maybe User
initialFocusedUser u
  | isStudent u = Just u  -- Students always focus themselves
  | otherwise = Nothing   -- Teachers start with no focus

-- | Create initial focused user state
mkFocusedUserState :: User -> FocusedUserState
mkFocusedUserState u = FocusedUserState
  { focusedUser = initialFocusedUser u
  , onFocusedUserChanged = Map.empty
  , nextFocusedUserHandlerId = 0
  }

-- | Subscribe to focused user changes
subscribeFocusedUser :: forall a. SyncDocumentRef -> (FocusedUserChange -> a) -> M.Sink a -> IO ()
subscribeFocusedUser r f sink = createSub acquire release sink
  where
    acquire = do
      modifyMVar r.focusedUserState $ \s -> do
        let handlerId = s.nextFocusedUserHandlerId
            handler = FocusedUserHandler f sink
            newState = s
              { onFocusedUserChanged = Map.insert handlerId handler s.onFocusedUserChanged
              , nextFocusedUserHandlerId = handlerId + 1
              }
        -- Send initial value
        sink $ f $ FocusedUserChange s.focusedUser True
        pure (newState, handlerId)
    release handlerId =
      modifyMVar_ r.focusedUserState $ \s ->
        pure s { onFocusedUserChanged = Map.delete handlerId s.onFocusedUserChanged }

-- | Set the focused user (only works for teachers; no-op for students)
setFocusedUser :: SyncDocumentRef -> Maybe User -> IO ()
setFocusedUser r newUser = do
  -- Check if connected user is a student (cannot change focus)
  if isStudent r.env.connectedUser
    then pure ()  -- No-op for students
    else modifyMVar_ r.focusedUserState $ \s -> do
      let change = FocusedUserChange newUser False
      -- Notify all handlers
      forM_ (Map.elems s.onFocusedUserChanged) $ \(FocusedUserHandler f sink) ->
        sink (f change)
      pure s { focusedUser = newUser }

-- | Read current focused user
readFocusedUser :: (MonadIO m) => SyncDocumentRef -> m (Maybe User)
readFocusedUser r = liftIO $ (.focusedUser) <$> readMVar r.focusedUserState
