module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  , EvidenceMode (..)
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
import Competences.Frontend.Component.Selector.UserSelector (searchableMultiUserEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Miso qualified as M
import Optics.Core ((&), (?~), (^.))

-- | Mode for the evidence editor component (single mode, no switcher shown)
data EvidenceMode = EvidenceEdit
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Evidence editor component using SelectorDetail pattern
evidenceEditorComponent
  :: SyncDocumentRef
  -> M.Component p (SD.Model Evidence EvidenceMode) (SD.Action EvidenceMode)
evidenceEditorComponent r =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "evidence"
      , SD.selectorComponent = evidenceSelectorComponent r
      , SD.detailView = \EvidenceEdit -> evidenceDetailView r
      , SD.modeLabel = \EvidenceEdit -> C.translate' C.LblEdit
      , SD.modeIcon = \EvidenceEdit -> Just IcnEdit
      , SD.availableModes = EvidenceEdit :| []
      , SD.defaultMode = EvidenceEdit
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }

-- | Detail view for editing an evidence
evidenceDetailView
  :: SyncDocumentRef
  -> Evidence
  -> M.View (SD.Model Evidence EvidenceMode) (SD.Action EvidenceMode)
evidenceDetailView r evidence =
  V.component
    ("evidence-editor-" <> M.ms (show evidence.id))
    (TE.editorComponent evidenceEditor r)
  where
    evidenceEditorId = "evidence-editor-" <> M.ms (show evidence.id)
    evidenceEditable =
      TE.editable
        ( \d -> do
            fmap
              (\c -> (c, (d ^. #locks) Map.!? EvidenceLock c.id))
              (Ix.getOne $ d.evidences Ix.@= evidence.id)
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
                           , searchableMultiUserEditorField
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
