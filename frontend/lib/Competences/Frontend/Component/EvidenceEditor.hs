module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  )
where

import Competences.Document (User)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M

data Model = Model
  {
  }
  deriving (Eq, Generic, Show)

data Action

evidenceEditorComponent :: SyncDocumentRef -> User -> M.Component p Model Action
evidenceEditorComponent r u = M.component model update view
  where
    model = Model

    update _ = pure ()

    view m = M.div_ [] []
