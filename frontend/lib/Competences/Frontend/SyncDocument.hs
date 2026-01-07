module Competences.Frontend.SyncDocument
  ( -- * SyncDocument Reference
    SyncDocumentRef
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
  , nextId
  , isInitialUpdate
    -- * Focused User
  , FocusedUserState (..)
  , FocusedUserChange (..)
  , subscribeFocusedUser
  , setFocusedUser
  , readFocusedUser
    -- * Connection State
  , ConnectionState (..)
  , ConnectionChange (..)
  , subscribeConnection
  , setConnectionState
  , notifyPendingChange
  , trySendNextCommand
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Document.User (isStudent)
import Competences.Document.Id (Id (..))
import Control.Monad (forM_, when)
import Data.Time (Day, UTCTime (..), getCurrentTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((%~), (&), (.~))
import System.Random (StdGen, newStdGen, random)
import UnliftIO (MVar, MonadIO, MonadUnliftIO, liftIO, modifyMVar, modifyMVar_, newMVar, readMVar)
import Miso.Subscription.Util (createSub)
import qualified Data.Map as Map

-- | The SyncDocument is, what is at the heart of the application. It contains the
-- entire server state regarding the competence grid model, as far as it is
-- available to the session user. It also contains the local state of the application,
-- as far as it shall be replicated to the server, i.e. everything that shall be
-- persisted.
data SyncDocument = SyncDocument
  { localDocument :: !Document
  , localChanges :: ![Command]
  , pendingCommand :: !(Maybe Command)
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

-- | Connection state for WebSocket
data ConnectionState
  = Connected
  | Disconnected
  | Reconnecting !Int  -- Attempt number
  deriving (Eq, Show, Generic)

-- | Connection change notification
data ConnectionChange = ConnectionChange
  { state :: !ConnectionState
  , pendingCount :: !Int
  }
  deriving (Eq, Show, Generic)

-- | State for connection status tracking
data ConnectionStateData = ConnectionStateData
  { connectionState :: !ConnectionState
  , onConnectionChanged :: !(Map.Map Int ConnectionHandler)
  , nextConnectionHandlerId :: !Int
  }
  deriving (Generic)

data ConnectionHandler where
  ConnectionHandler :: forall a. (ConnectionChange -> a) -> (M.Sink a) -> ConnectionHandler

data SyncDocumentRef = SyncDocumentRef
  { syncDocument :: MVar SyncDocument
  , randomGen :: MVar StdGen
  , env :: !SyncDocumentEnv
  , focusedUserState :: MVar FocusedUserState
  , connectionStateData :: MVar ConnectionStateData
  }

-- | Get the environment from a SyncDocumentRef
syncDocumentEnv :: SyncDocumentRef -> SyncDocumentEnv
syncDocumentEnv r = r.env

data SyncDocumentEnv = SyncDocumentEnv
  { currentDay :: !Day
  , connectedUser :: !User
  , sendCommand :: !(Command -> IO ())  -- Injected by connection layer
  }
  deriving (Generic)

mkSyncDocument :: (MonadIO m) => SyncDocumentEnv -> m SyncDocumentRef
mkSyncDocument env = do
  syncDocument <- newMVar emptySyncDocument
  randomGen <- newStdGen >>= newMVar
  focusedUserState <- newMVar $ mkFocusedUserState env.connectedUser
  connectionStateData <- newMVar mkConnectionStateData
  pure $ SyncDocumentRef syncDocument randomGen env focusedUserState connectionStateData

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncDocumentRef
mkSyncDocument' env randomGen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar randomGen
  focusedUserState <- newMVar $ mkFocusedUserState env.connectedUser
  connectionStateData <- newMVar mkConnectionStateData
  pure $ SyncDocumentRef syncDocument randomGen' env focusedUserState connectionStateData

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
  -- Notify pending count changed
  notifyPendingChange r
  -- Try to send next command (respects at-most-one-in-flight)
  trySendNextCommand r

setSyncDocument :: SyncDocumentRef -> Document -> IO ()
setSyncDocument r m = do
  modifyMVar_ r.syncDocument $ setSyncDocument' r.env.connectedUser.id m
  -- Notify pending count may have changed
  notifyPendingChange r
  -- Note: trySendNextCommand is NOT called here because connection state
  -- is not yet Connected. The handler will call it after setConnectionState.

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] Nothing emptyDocument Map.empty 0

modifySyncDocument' :: UserId -> Command -> SyncDocument -> IO SyncDocument
modifySyncDocument' uId c d = do
  case handleCommand uId c d.localDocument of
    Left err -> do
      M.consoleLog $ M.ms $ "Handling command '" <> show c <> "' failed: " <> show err
      pure d
    Right m' -> do
      let d' =
            d
              & (#localChanges %~ (c :))
              & (#localDocument .~ fst m')
      forM_ d.onChanged $
        issueDocumentChange (DocumentChange d'.localDocument (DocumentChanged d.localDocument c))
      pure d'

setSyncDocument' :: UserId -> Document -> SyncDocument -> IO SyncDocument
setSyncDocument' userId remoteDoc d = do
  -- Put pendingCommand back at front of localChanges for replay
  -- This preserves commands that were in-flight when connection dropped
  let allLocalChanges = maybe d.localChanges (: d.localChanges) d.pendingCommand

  -- Replay all local changes on the new remoteDocument
  -- This handles both initial connection (empty localChanges) and reconnection
  let (localDoc', validChanges, _) = replayLocalChanges userId remoteDoc Nothing allLocalChanges

  let d' =
        d
          & (#remoteDocument .~ remoteDoc)
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)
          & (#pendingCommand .~ Nothing)  -- Clear pending - commands are now in localChanges

  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> IO ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncDocumentRef -> IO ()
issueInitialUpdate r = do
  d <- readMVar r.syncDocument
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument InitialUpdate)

mkSyncDocumentEnv :: (MonadIO m) => User -> (Command -> IO ()) -> m SyncDocumentEnv
mkSyncDocumentEnv u sendCmd = do
  d <- (.utctDay) <$> liftIO getCurrentTime
  pure $ SyncDocumentEnv d u sendCmd

nextId :: (MonadUnliftIO m) => SyncDocumentRef -> m (Id a)
nextId r = modifyMVar r.randomGen (pure . swap . random)

-- | Try to send the next command from localChanges if connected and no command is pending
-- This maintains the at-most-one-command-in-flight invariant
trySendNextCommand :: SyncDocumentRef -> IO ()
trySendNextCommand r = do
  connState <- (.connectionState) <$> readMVar r.connectionStateData
  case connState of
    Connected -> do
      maybeCmd <- modifyMVar r.syncDocument $ \syncDoc ->
        case (syncDoc.pendingCommand, syncDoc.localChanges) of
          (Nothing, cmd : rest) ->
            let syncDoc' = syncDoc
                  & (#pendingCommand .~ Just cmd)
                  & (#localChanges .~ rest)
            in pure (syncDoc', Just cmd)
          _ -> pure (syncDoc, Nothing)
      case maybeCmd of
        Just cmd -> do
          M.consoleLog $ M.ms $ "Sending command: " <> show cmd
          r.env.sendCommand cmd
        Nothing -> pure ()
    _ -> pure ()  -- Not connected, don't send

-- | Apply a command from the server (echo or broadcast)
-- Updates remoteDocument and replays pendingCommand + localChanges on top of it
applyRemoteCommand :: SyncDocumentRef -> Command -> IO ()
applyRemoteCommand d cmd = do
  modifyMVar_ d.syncDocument $ \syncDoc -> do
    -- Apply command to remoteDocument
    remoteDoc' <- case handleCommand d.env.connectedUser.id cmd syncDoc.remoteDocument of
      Left err -> do
        -- This shouldn't happen - server validated the command
        M.consoleLog $ M.ms $ "ERROR: Server sent invalid command: " <> show err
        pure syncDoc.remoteDocument
      Right (doc, _) -> pure doc

    -- Check if this is an echo of OUR pending command
    pendingCommand' <- case syncDoc.pendingCommand of
      Just pending | pending == cmd -> do
        M.consoleLog $ M.ms $ "Received echo of our command: " <> show cmd
        pure Nothing  -- Clear pending, this is our echo
      _ -> pure syncDoc.pendingCommand  -- Different command, keep pending

    -- Replay pendingCommand and localChanges on top of the new remote document
    let (localDoc', validChanges, validPending) = replayLocalChanges
          d.env.connectedUser.id
          remoteDoc'
          pendingCommand'
          syncDoc.localChanges

    when (length validChanges < length syncDoc.localChanges) $ do
      M.consoleLog $ M.ms $ "WARNING: Conflict detected - "
        <> show (length syncDoc.localChanges - length validChanges) <> " local commands were dropped"

    let syncDoc' = syncDoc
          & (#remoteDocument .~ remoteDoc')
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)
          & (#pendingCommand .~ validPending)

    -- Notify subscribers
    forM_ syncDoc.onChanged $
      issueDocumentChange (DocumentChange localDoc' (DocumentChanged syncDoc.localDocument cmd))

    pure syncDoc'

  -- Notify that pending count may have changed
  notifyPendingChange d
  -- Try to send next command (may have been unblocked by echo clearing pendingCommand)
  trySendNextCommand d

-- | Replay pending command and local changes on top of a document, filtering out invalid ones
-- Returns (resulting document, valid localChanges, valid pendingCommand)
replayLocalChanges :: UserId -> Document -> Maybe Command -> [Command] -> (Document, [Command], Maybe Command)
replayLocalChanges userId doc maybePending localCmds =
  -- First apply pending command (if exists)
  let (docAfterPending, validPending) = case maybePending of
        Nothing -> (doc, Nothing)
        Just pending -> case handleCommand userId pending doc of
          Left _err -> (doc, Nothing)  -- Pending command no longer valid
          Right (doc', _) -> (doc', Just pending)

  -- Then apply local changes on top
      (finalDoc, validLocalCmds) = foldr applyOne (docAfterPending, []) (reverse localCmds)

   in (finalDoc, validLocalCmds, validPending)
  where
    applyOne cmd (currentDoc, validCmds) =
      case handleCommand userId cmd currentDoc of
        Left _err -> (currentDoc, validCmds)  -- Drop invalid command
        Right (newDoc, _) -> (newDoc, cmd : validCmds)

-- | Handle a command rejection from the server
-- Removes the rejected command from pendingCommand and replays remaining changes
rejectCommand :: SyncDocumentRef -> Command -> IO ()
rejectCommand d cmd = do
  modifyMVar_ d.syncDocument $ \syncDoc -> do
    case syncDoc.pendingCommand of
      Just pending | pending == cmd -> do
        -- Expected: our pending command was rejected
        M.consoleLog $ M.ms $ "Our command was rejected by server: " <> show cmd

        -- Remove from localChanges if it's there
        let localChanges' = filter (/= cmd) syncDoc.localChanges

        -- Replay remaining local changes on remote document (no pending now)
        let (localDoc', validChanges, _) = replayLocalChanges
              d.env.connectedUser.id
              syncDoc.remoteDocument
              Nothing  -- pendingCommand cleared
              localChanges'

        let syncDoc' = syncDoc
              & (#pendingCommand .~ Nothing)  -- Clear pending
              & (#localDocument .~ localDoc')
              & (#localChanges .~ validChanges)

        -- Notify subscribers
        forM_ syncDoc.onChanged $
          issueDocumentChange (DocumentChange localDoc' DocumentReloaded)

        pure syncDoc'

      _ -> do
        -- Unexpected: rejection for command we didn't send
        M.consoleLog $ M.ms $ "WARNING: Received rejection for non-pending command: " <> show cmd
        pure syncDoc  -- Keep state

  -- Notify that pending count may have changed
  notifyPendingChange d
  -- Try to send next command (pendingCommand was cleared)
  trySendNextCommand d

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

-- ============================================================================
-- CONNECTION STATE
-- ============================================================================

-- | Create initial connection state data
mkConnectionStateData :: ConnectionStateData
mkConnectionStateData = ConnectionStateData
  { connectionState = Disconnected
  , onConnectionChanged = Map.empty
  , nextConnectionHandlerId = 0
  }

-- | Subscribe to connection state changes
subscribeConnection :: forall a. SyncDocumentRef -> (ConnectionChange -> a) -> M.Sink a -> IO ()
subscribeConnection r f sink = createSub acquire release sink
  where
    acquire = do
      modifyMVar r.connectionStateData $ \s -> do
        let handlerId = s.nextConnectionHandlerId
            handler = ConnectionHandler f sink
            newState = s
              { onConnectionChanged = Map.insert handlerId handler s.onConnectionChanged
              , nextConnectionHandlerId = handlerId + 1
              }
        -- Get pending count for initial notification
        syncDoc <- readMVar r.syncDocument
        let pendingCount = length syncDoc.localChanges + (if syncDoc.pendingCommand == Nothing then 0 else 1)
        -- Send initial value
        sink $ f $ ConnectionChange s.connectionState pendingCount
        pure (newState, handlerId)
    release handlerId =
      modifyMVar_ r.connectionStateData $ \s ->
        pure s { onConnectionChanged = Map.delete handlerId s.onConnectionChanged }

-- | Set the connection state and notify subscribers
setConnectionState :: SyncDocumentRef -> ConnectionState -> IO ()
setConnectionState r newState = do
  modifyMVar_ r.connectionStateData $ \s -> do
    -- Get pending count for notification
    syncDoc <- readMVar r.syncDocument
    let pendingCount = length syncDoc.localChanges + (if syncDoc.pendingCommand == Nothing then 0 else 1)
    let change = ConnectionChange newState pendingCount
    -- Notify all handlers
    forM_ (Map.elems s.onConnectionChanged) $ \(ConnectionHandler f sink) ->
      sink (f change)
    pure s { connectionState = newState }

-- | Notify subscribers about pending count change (without changing connection state)
notifyPendingChange :: SyncDocumentRef -> IO ()
notifyPendingChange r = do
  modifyMVar_ r.connectionStateData $ \s -> do
    -- Get pending count for notification
    syncDoc <- readMVar r.syncDocument
    let pendingCount = length syncDoc.localChanges + (if syncDoc.pendingCommand == Nothing then 0 else 1)
    let change = ConnectionChange s.connectionState pendingCount
    -- Notify all handlers
    forM_ (Map.elems s.onConnectionChanged) $ \(ConnectionHandler f sink) ->
      sink (f change)
    pure s  -- State unchanged
