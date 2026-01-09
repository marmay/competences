module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  , EvidenceMode (..)
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), EvidencePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.IxSet qualified as IxSet
import Competences.Document
  ( Competence (..)
  , Document (..)
  , Evidence (..)
  , Lock (..)
  , User (..)
  , emptyDocument
  )
import Competences.Document.Evidence (Observation (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.EvidenceSelector (EvidenceSelectorStyle (..), evidenceSelectorComponent)
import Competences.Frontend.Component.Selector.MultiTaskSelector (searchableMultiTaskEditorField)
import Competences.Frontend.Component.Selector.ObservationSelector qualified as TE
import Competences.Frontend.Component.Selector.UserSelector (searchableSingleUserEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~), (?~), (^.))

-- | Mode for the evidence component
data EvidenceMode
  = EvidenceView
  | EvidenceEdit
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Evidence editor component using SelectorDetail pattern
evidenceEditorComponent
  :: SyncContext
  -> Bool
  -- ^ Can edit evidences? (True for teachers, False for students)
  -> M.Component p (SD.Model Evidence EvidenceMode) (SD.Action EvidenceMode)
evidenceEditorComponent r canEdit =
  let style = if canEdit then EvidenceSelectorViewAndCreate else EvidenceSelectorViewOnly
      (defaultMode, availableModes, detailView) =
        if canEdit
          then (EvidenceEdit, EvidenceEdit :| [], evidenceEditorDetailView r)
          else (EvidenceView, EvidenceView :| [], evidenceViewerDetailView r)
   in SD.selectorDetailComponent
        SD.SelectorDetailConfig
          { SD.selectorId = "evidence"
          , SD.selectorComponent = evidenceSelectorComponent r style
          , SD.detailView = const detailView
          , SD.modeLabel = \case
              EvidenceView -> C.translate' C.LblView
              EvidenceEdit -> C.translate' C.LblEdit
          , SD.modeIcon = \case
              EvidenceView -> Just IcnView
              EvidenceEdit -> Just IcnEdit
          , SD.availableModes = availableModes
          , SD.defaultMode = defaultMode
          , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
          }

-- ============================================================================
-- VIEW MODE DETAIL (Read-only)
-- ============================================================================

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { document :: !Document
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
newtype ViewerAction = ViewerUpdateDocument DocumentChange
  deriving (Eq, Show)

-- | Read-only view for an evidence
evidenceViewerDetailView
  :: SyncContext
  -> Evidence
  -> M.View (SD.Model Evidence EvidenceMode) (SD.Action EvidenceMode)
evidenceViewerDetailView r evidence =
  V.component
    ("evidence-viewer-" <> M.ms (show evidence.id))
    (viewerComponent r evidence)

viewerComponent :: SyncContext -> Evidence -> M.Component p ViewerModel ViewerAction
viewerComponent r evidence =
  (M.component model update view)
    { M.subs = [subscribeDocument r ViewerUpdateDocument]
    }
  where
    model = ViewerModel emptyDocument

    update (ViewerUpdateDocument (DocumentChange doc _)) =
      M.modify $ #document .~ doc

    view m =
      M.div_
        [class_ "space-y-4"]
        [ Typography.h2 (C.translate' C.LblEvidences)
        , viewField (C.translate' C.LblEvidenceDate) (C.formatDay evidence.date)
        , viewField (C.translate' C.LblActivityType) (C.translate' $ C.LblActivityTypeDescription evidence.activityType)
        , viewField (C.translate' C.LblTasksAndGroups) (viewTasks m evidence.tasks)
        , viewObservations m evidence
        ]

    viewField label value =
      M.div_
        [class_ ""]
        [ M.div_ [class_ "text-sm font-medium text-muted-foreground"] [M.text label]
        , M.div_ [class_ "mt-1"] [M.text value]
        ]

    viewTasks m taskIds =
      let taskNames = map (getTaskName m) taskIds
       in if null taskNames
            then C.translate' C.LblNoTasksSelected
            else M.ms $ T.intercalate ", " taskNames

    getTaskName m taskId =
      case Ix.getOne (m.document.tasks Ix.@= taskId) of
        Just task -> let TaskIdentifier ident = task.identifier in ident
        Nothing -> T.pack $ show taskId

    viewObservations m e =
      M.div_
        []
        [ M.div_ [class_ "text-sm font-medium text-muted-foreground mb-2"] [M.text $ C.translate' C.LblActivityObservations]
        , if null (Ix.toList e.observations)
            then M.div_ [class_ "text-muted-foreground"] [M.text "Keine Beobachtungen"]
            else M.div_ [class_ "space-y-2"] (map (viewObservation m) (Ix.toList e.observations))
        ]

    viewObservation m obs =
      let competenceName = getCompetenceName m obs.competenceLevelId
       in M.div_
            [class_ "flex items-center justify-between p-2 bg-muted/50 rounded"]
            [ M.span_ [class_ "text-sm"] [M.text $ M.ms competenceName]
            , M.span_ [class_ "text-sm font-medium"] [M.text $ C.translate' $ C.LblAbility obs.ability]
            ]

    getCompetenceName m (competenceId, level) =
      case Ix.getOne (m.document.competences Ix.@= competenceId) of
        Just comp ->
          case comp.levelDescriptions Map.!? level of
            Just desc -> desc
            Nothing -> comp.description <> " - " <> T.pack (show level)
        Nothing -> T.pack $ show (competenceId, level)

-- ============================================================================
-- EDIT MODE DETAIL
-- ============================================================================

-- | Detail view for editing an evidence
evidenceEditorDetailView
  :: SyncContext
  -> Evidence
  -> M.View (SD.Model Evidence EvidenceMode) (SD.Action EvidenceMode)
evidenceEditorDetailView r evidence =
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
        `TE.addNamedField` ( C.translate' C.LblStudent
                           , searchableSingleUserEditorField
                               r
                               (evidenceEditorId <> "-user")
                               isStudent
                               (entityPatchTransformedLens #userId #userId (.id) id)
                           )
        `TE.addNamedField` ( C.translate' C.LblTasksAndGroups
                           , searchableMultiTaskEditorField
                               r
                               (evidenceEditorId <> "-tasks")
                               (entityPatchTransformedLens #tasks #tasks id id)
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
