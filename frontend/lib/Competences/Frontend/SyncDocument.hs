module Competences.Frontend.SyncDocument
  ( SyncDocumentRef
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
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, emptyDocument)
import Control.Concurrent (MVar, newMVar)
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
import Optics.Core ((%~), (&), (.~))
import UnliftIO (modifyMVar_, readMVar, MonadIO)

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
  { model :: !Document
  , change :: !(Maybe (Document, Command))
  }
  deriving (Eq, Show, Generic)

data ChangedHandler where
  ChangedHandler :: forall a. (DocumentChange -> a) -> (M.Sink a) -> ChangedHandler

type SyncDocumentRef = MVar SyncDocument

mkSyncDocument :: IO SyncDocumentRef
mkSyncDocument = newMVar emptySyncDocument

mkSyncDocument' :: Document -> IO SyncDocumentRef
mkSyncDocument' m = newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)

readSyncDocument :: MonadIO m => SyncDocumentRef -> m SyncDocument
readSyncDocument = readMVar

subscribeDocument :: forall a. SyncDocumentRef -> (DocumentChange -> a) -> M.Sink a -> JSM ()
subscribeDocument d f s = do
  let h = ChangedHandler f s
  modifyMVar_ d $ \d' -> do
    s $ f (DocumentChange d'.localDocument Nothing)
    pure $ d' & (#onChanged %~ (h :))

modifySyncDocument :: SyncDocumentRef -> Command -> JSM ()
modifySyncDocument d c = do
  M.consoleLog $ "modifySyncDocument: " <> M.ms (show c)
  modifyMVar_ d $ modifySyncDocument' c

setSyncDocument :: SyncDocumentRef -> Document -> JSM ()
setSyncDocument d m = modifyMVar_ d $ setSyncDocument' m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument []

modifySyncDocument' :: Command -> SyncDocument -> JSM SyncDocument
modifySyncDocument' c d = do
  case handleCommand c d.localDocument of
    Left err -> do
      M.consoleLog $ M.ms $ "Handling command '" <> show c <> "' failed: " <> show err
      pure d
    Right m' -> do
      let d' =
            d
              & (#localChanges %~ (c :))
              & (#localDocument .~ fst m')
      forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument (Just (d.localDocument, c)))
      pure d'

setSyncDocument' :: Document -> SyncDocument -> JSM SyncDocument
setSyncDocument' m d = do
  let d' =
        d
          & (#localChanges .~ [])
          & (#localDocument .~ m)
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d'.localDocument Nothing)
  pure d'

issueDocumentChange :: DocumentChange -> ChangedHandler -> JSM ()
issueDocumentChange c (ChangedHandler f sink) = sink $ f c

issueInitialUpdate :: SyncDocumentRef -> JSM ()
issueInitialUpdate r = do
  d <- readMVar r
  forM_ d.onChanged $ issueDocumentChange (DocumentChange d.localDocument Nothing)
