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
import Control.Monad (forM_)
import Data.Time (Day, UTCTime (..), getCurrentTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
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
  , localChanges :: ![Command]
  , remoteDocument :: !Document
  , onChanged :: ![ChangedHandler]
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
  pure $ SyncDocumentRef syncDocument randomGen env

mkSyncDocument' :: (MonadIO m) => SyncDocumentEnv -> StdGen -> Document -> m SyncDocumentRef
mkSyncDocument' env randomGen m = do
  syncDocument <- newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)
  randomGen' <- newMVar randomGen
  pure $ SyncDocumentRef syncDocument randomGen' env

readSyncDocument :: (MonadIO m) => SyncDocumentRef -> m SyncDocument
readSyncDocument = readMVar . (.syncDocument)

subscribeDocument :: forall a. SyncDocumentRef -> (DocumentChange -> a) -> M.Sink a -> JSM ()
subscribeDocument d f s = do
  let h = ChangedHandler f s
  modifyMVar_ d.syncDocument $ \d' -> do
    s $ f (DocumentChange d'.localDocument InitialUpdate)
    pure $ d' & (#onChanged %~ (h :))

modifySyncDocument :: SyncDocumentRef -> Command -> JSM ()
modifySyncDocument d c = do
  M.consoleLog $ "modifySyncDocument: " <> M.ms (show c)
  modifyMVar_ d.syncDocument $ modifySyncDocument' d.env.connectedUser.id c

setSyncDocument :: SyncDocumentRef -> Document -> JSM ()
setSyncDocument d m = modifyMVar_ d.syncDocument $ setSyncDocument' m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument []

modifySyncDocument' :: UserId -> Command -> SyncDocument -> JSM SyncDocument
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

setSyncDocument' :: Document -> SyncDocument -> JSM SyncDocument
setSyncDocument' m d = do
  let d' =
        d
          & (#localChanges .~ [])
          & (#localDocument .~ m)
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument DocumentReloaded)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> JSM ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncDocumentRef -> JSM ()
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
