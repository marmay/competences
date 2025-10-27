module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..))
import Competences.Document
  ( Evidence (..)
  )
import Competences.Document.Evidence (mkEvidence)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.EvidenceSelector (evidenceSelectorComponent)
import Competences.Frontend.SyncDocument
  ( SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~), (^.))

data Model = Model
  { evidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action

evidenceEditorComponent :: SyncDocumentRef -> M.Component p Model Action
evidenceEditorComponent r =
  M.component model update view
  where
    model = Model Nothing
    update _ = pure ()

    view m =
      V.viewFlow
        (V.hFlow & #expandDirection .~ V.Expand V.Start)
        [ V.component "evidence-editor-selection" (evidenceSelectorComponent r #evidence)
        , evidenceEditor
        ]
      where
        evidenceEditor =
          M.div_ [] []
