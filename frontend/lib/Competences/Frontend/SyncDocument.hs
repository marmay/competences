module Competences.Frontend.SyncDocument
  ( SyncDocumentRef
  , SyncDocumentEnv (..)
  , SyncDocument (..)
  , DocumentChange (..)
  , mkSyncDocument
  , mkSyncDocument'
  , subscribeDocument
  , modifySyncDocument
  , setSyncDocument
  , setWebSocket
  , applyRemoteCommand
  , rejectCommand
  , emptySyncDocument
  , modifySyncDocument'
  , readSyncDocument
  , setSyncDocument'
  , issueInitialUpdate
  , syncDocumentEnv
  , mkSyncDocumentEnv
  , nextId
  , isInitialUpdate
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, User (..), UserId, emptyDocument)
import Competences.Document.Id (Id (..))
import Competences.Frontend.WebSocket (WebSocketConnection, sendMessage)
import Competences.Protocol (ClientMessage (..))
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

data SyncDocumentRef = SyncDocumentRef
  { syncDocument :: MVar SyncDocument
  , randomGen :: MVar StdGen
  , env :: !SyncDocumentEnv
  , webSocket :: MVar (Maybe WebSocketConnection)
  }

data SyncDocumentEnv = SyncDocumentEnv
  { currentDay :: !Day
  , connectedUser :: !User
  }
  deriving (Eq, Generic, Show)

mkSyncDocument :: (MonadIO m) => SyncDocumentEnv -> m SyncDocumentRef
mkSyncDocument env = do
  syncDocument <- newMVar emptySyncDocument
  randomGen <- newStdGen >>= newMVar
  webSocket <- newMVar Nothing
  pure $ SyncDocumentRef syncDocument randomGen env webSocket

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncDocumentRef
mkSyncDocument' env randomGen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar randomGen
  webSocket <- newMVar Nothing
  pure $ SyncDocumentRef syncDocument randomGen' env webSocket

readSyncDocument :: (MonadIO m) => SyncDocumentRef -> m SyncDocument
readSyncDocument = readMVar . (.syncDocument)

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
modifySyncDocument d c = do
  M.consoleLog $ "modifySyncDocument: " <> M.ms (show c)
  modifyMVar_ d.syncDocument $ modifySyncDocument' d.env.connectedUser.id c
  -- Try to send next command after adding to localChanges
  trySendNextCommand d

setSyncDocument :: SyncDocumentRef -> Document -> IO ()
setSyncDocument d m = modifyMVar_ d.syncDocument $ setSyncDocument' d.env.connectedUser.id m

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
  -- Replay existing localChanges on the new remoteDocument
  -- This handles both initial connection (empty localChanges) and reconnection
  -- Note: pendingCommand is cleared on reconnect, so we pass Nothing
  let (localDoc', validChanges, _) = replayLocalChanges userId remoteDoc Nothing d.localChanges

  let d' =
        d
          & (#remoteDocument .~ remoteDoc)
          & (#localDocument .~ localDoc')
          & (#localChanges .~ validChanges)
          & (#pendingCommand .~ Nothing)  -- Clear pending on reconnect

  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> IO ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncDocumentRef -> IO ()
issueInitialUpdate r = do
  d <- readMVar r.syncDocument
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument InitialUpdate)

syncDocumentEnv :: SyncDocumentRef -> SyncDocumentEnv
syncDocumentEnv = (.env)

mkSyncDocumentEnv :: (MonadIO m) => User -> m SyncDocumentEnv
mkSyncDocumentEnv u = do
  d <- (.utctDay) <$> liftIO getCurrentTime
  pure $ SyncDocumentEnv d u

nextId :: (MonadUnliftIO m) => SyncDocumentRef -> m (Id a)
nextId r = do
  modifyMVar r.randomGen (pure . swap . random)

-- | Set the WebSocket connection for sending commands
setWebSocket :: SyncDocumentRef -> WebSocketConnection -> IO ()
setWebSocket r ws = do
  modifyMVar_ r.webSocket $ \_ -> pure (Just ws)
  -- Try to send pending commands after connection is established
  trySendNextCommand r

-- | Try to send the next command from localChanges if no command is pending
trySendNextCommand :: SyncDocumentRef -> IO ()
trySendNextCommand d = do
  maybeWs <- readMVar d.webSocket
  case maybeWs of
    Nothing -> pure ()  -- No connection
    Just ws -> do
      maybeCmd <- modifyMVar d.syncDocument $ \syncDoc ->
        case (syncDoc.pendingCommand, syncDoc.localChanges) of
          (Nothing, cmd : rest) -> do
            -- No pending command and have local changes - send next one
            let syncDoc' = syncDoc
                  & (#pendingCommand .~ Just cmd)
                  & (#localChanges .~ rest)
            pure (syncDoc', Just cmd)
          _ -> pure (syncDoc, Nothing)  -- Already pending or no changes

      case maybeCmd of
        Just cmd -> do
          M.consoleLog $ M.ms $ "Sending command to server: " <> show cmd
          sendMessage ws (SendCommand cmd)
        Nothing -> pure ()

-- | Apply a command from the server (echo or broadcast)
-- Updates remoteDocument and replays pendingCommand + localChanges on top of it
applyRemoteCommand :: SyncDocumentRef -> Command -> IO ()
applyRemoteCommand d cmd = do
  shouldSendNext <- modifyMVar d.syncDocument $ \syncDoc -> do
    -- Apply command to remoteDocument
    remoteDoc' <- case handleCommand d.env.connectedUser.id cmd syncDoc.remoteDocument of
      Left err -> do
        -- This shouldn't happen - server validated the command
        M.consoleLog $ M.ms $ "ERROR: Server sent invalid command: " <> show err
        pure syncDoc.remoteDocument
      Right (doc, _) -> pure doc

    -- Check if this is an echo of OUR pending command
    (pendingCommand', isEcho) <- case syncDoc.pendingCommand of
      Just pending | pending == cmd -> do
        M.consoleLog $ M.ms $ "Received echo of our command: " <> show cmd
        pure (Nothing, True)  -- Clear pending, this is our echo
      _ -> pure (syncDoc.pendingCommand, False)  -- Different command, keep pending

    -- Replay pendingCommand and localChanges on top of the new remote document
    let (localDoc', validChanges, validPending) = replayLocalChanges
          d.env.connectedUser.id
          remoteDoc'
          pendingCommand'
          syncDoc.localChanges

    when (not isEcho && length validChanges < length syncDoc.localChanges) $ do
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

    pure (syncDoc', isEcho)

  -- If we cleared pendingCommand, try to send next
  when shouldSendNext $ trySendNextCommand d

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
  shouldSendNext <- modifyMVar d.syncDocument $ \syncDoc -> do
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

        pure (syncDoc', True)  -- Should send next

      _ -> do
        -- Unexpected: rejection for command we didn't send
        M.consoleLog $ M.ms $ "WARNING: Received rejection for non-pending command: " <> show cmd
        pure (syncDoc, False)  -- Keep state, don't send next

  -- If we cleared pendingCommand, try to send next
  when shouldSendNext $ trySendNextCommand d
