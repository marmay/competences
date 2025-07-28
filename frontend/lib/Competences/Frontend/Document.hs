module Competences.Frontend.Document
  ( DocumentRef
  , Document (..)
  , ModelChange (..)
  , mkDocument
  , mkDocument'
  , subscribeDocument
  , modifyDocumentModel
  , setDocumentModel
  , emptyDocument
  , modifyDocumentModel'
  , setDocumentModel'
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Model (Model, emptyModel)
import Control.Concurrent (MVar, newMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified
import Optics.Core ((%~), (&), (.~))
import UnliftIO (modifyMVar_)

-- | The Document is, what is at the heart of the application. It contains the
-- entire server state regarding the competence grid model, as far as it is
-- available to the session user. It also contains the local state of the application,
-- as far as it shall be replicated to the server, i.e. everything that shall be
-- persisted.
data Document = Document
  { localModel :: !Model
  , localChanges :: ![Command]
  , remoteModel :: !Model
  , onChanged :: ![ChangedHandler]
  }
  deriving (Generic)

data ModelChange = ModelChange
  { model :: !Model
  , change :: !(Maybe (Model, Command))
  }
  deriving (Eq, Show, Generic)

data ChangedHandler where
  ChangedHandler :: forall a. (ModelChange -> a) -> (Miso.Sink a) -> ChangedHandler

type DocumentRef = MVar Document

mkDocument :: JSM DocumentRef
mkDocument = liftIO $ newMVar $ emptyDocument

mkDocument' :: Model -> JSM DocumentRef
mkDocument' m = liftIO $ newMVar $ emptyDocument & (#remoteModel .~ m) & (#localModel .~ m)

subscribeDocument :: forall a. DocumentRef -> (ModelChange -> a) -> Miso.Sink a -> JSM ()
subscribeDocument d f s = do
  let h = ChangedHandler f s
  modifyMVar_ d $ \d' -> pure $ d' & (#onChanged %~ (h :))

modifyDocumentModel :: DocumentRef -> Command -> JSM ()
modifyDocumentModel d c = modifyMVar_ d $ modifyDocumentModel' c

setDocumentModel :: DocumentRef -> Model -> JSM ()
setDocumentModel d m = modifyMVar_ d $ setDocumentModel' m

emptyDocument :: Document
emptyDocument = Document emptyModel [] emptyModel []

modifyDocumentModel' :: Command -> Document -> JSM Document
modifyDocumentModel' c d = do
  case handleCommand c d.localModel of
    Left _ -> pure d
    Right m' -> do
      let d' =
            d
              & (#localChanges %~ (c :))
              & (#localModel .~ fst m')
      forM_ d.onChanged $ issueModelChange (ModelChange d'.localModel (Just (d.localModel, c)))
      pure d'

setDocumentModel' :: Model -> Document -> JSM Document
setDocumentModel' m d = do
  let d' =
        d
          & (#localChanges .~ [])
          & (#localModel .~ m)
  forM_ d.onChanged $ issueModelChange (ModelChange d'.localModel Nothing)
  pure d'

issueModelChange :: ModelChange -> ChangedHandler -> JSM ()
issueModelChange c (ChangedHandler f sink) = sink $ f c
