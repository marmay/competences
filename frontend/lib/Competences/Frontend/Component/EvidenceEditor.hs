module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), EvidencePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.IxSet qualified as IxSet
import Competences.Document
  ( Document (..)
  , Evidence (..)
  , Lock (..)
  , User (..)
  )
-- ActivityTasks removed - using oldTasks field now
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.EvidenceSelector (evidenceSelectorComponent)
import Competences.Frontend.Component.Selector.ObservationSelector qualified as TE
import Competences.Frontend.Component.Selector.UserSelector (multiUserEditorField)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (?~), (^.))
import qualified Competences.Frontend.View.Tailwind as TW

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
      V.sideMenu
        (V.componentA "evidence-editor-selection" [TW.tailwind [TW.HFull]] (evidenceSelectorComponent r #evidence))
        (V.componentA evidenceEditorId [TW.tailwind [TW.HFull]] (TE.editorComponent evidenceEditor r))
      where
        evidenceEditorId = "evidence-editor-editor-" <> maybe "empty" (M.ms . show . (.id)) m.evidence
        evidenceEditable =
          TE.editable
            ( \d -> do
                e <- m.evidence
                fmap
                  (\c -> (c, (d ^. #locks) Map.!? EvidenceLock c.id))
                  (Ix.getOne $ d.evidences Ix.@= e.id)
            )
            & (#modify ?~ (\e modify -> Evidences $ OnEvidences (Modify e.id modify)))
            & (#delete ?~ (\e -> Evidences $ OnEvidences (Delete e.id)))
        evidenceEditor =
          TE.editor
            ( TE.editorFormView'
                (C.translate' C.LblEditEvidence)
                id
            )
            evidenceEditable
            `TE.addNamedField` ( C.translate' C.LblEvidenceDate
                               , TE.dayEditorField #date #date
                               )
            `TE.addNamedField` ( C.translate' C.LblActivityType
                               , TE.enumEditorField
                                   (C.translate' . C.LblActivityTypeDescription)
                                   #activityType
                                   #activityType
                               )
            `TE.addNamedField` ( C.translate' C.LblStudents
                               , multiUserEditorField
                                   r
                                   (evidenceEditorId <> "-users")
                                   isStudent
                                   (entityPatchTransformedLens #userIds #userIds (.id) Set.fromList)
                               )
            `TE.addNamedField` ( "Legacy Tasks"
                               , TE.textEditorField #oldTasks #oldTasks
                               )
            `TE.addNamedField` ( C.translate' C.LblActivityObservations
                               , TE.observationEditorField
                                   r
                                   (evidenceEditorId <> "-observations")
                                   (.id)
                                   (entityPatchTransformedLens #observations #observations id IxSet.fromList)
                               )
