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
  , setSyncDocument'
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document (Document, emptyDocument)
import Control.Concurrent (MVar, newMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified
import Optics.Core ((%~), (&), (.~))
import UnliftIO (modifyMVar_)

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
  ChangedHandler :: forall a. (DocumentChange -> a) -> (Miso.Sink a) -> ChangedHandler

type SyncDocumentRef = MVar SyncDocument

mkSyncDocument :: JSM SyncDocumentRef
mkSyncDocument = liftIO $ newMVar $ emptySyncDocument

mkSyncDocument' :: Document -> JSM SyncDocumentRef
mkSyncDocument' m = liftIO $ newMVar $ emptySyncDocument & (#remoteDocument .~ m) & (#localDocument .~ m)

subscribeDocument :: forall a. SyncDocumentRef -> (DocumentChange -> a) -> Miso.Sink a -> JSM ()
subscribeDocument d f s = do
  let h = ChangedHandler f s
  modifyMVar_ d $ \d' -> pure $ d' & (#onChanged %~ (h :))

modifySyncDocument :: SyncDocumentRef -> Command -> JSM ()
modifySyncDocument d c = modifyMVar_ d $ modifySyncDocument' c

setSyncDocument :: SyncDocumentRef -> Document -> JSM ()
setSyncDocument d m = modifyMVar_ d $ setSyncDocument' m

emptySyncDocument :: SyncDocument
emptySyncDocument = SyncDocument emptyDocument [] emptyDocument []

modifySyncDocument' :: Command -> SyncDocument -> JSM SyncDocument
modifySyncDocument' c d = do
  case handleCommand c d.localDocument of
    Left _ -> pure d
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
