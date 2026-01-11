module Competences.Frontend.Component.EvidenceEditor
  ( evidenceEditorComponent
  , EvidenceMode (..)
  , EvidenceEditorModel (..)
  , EvidenceEditorAction (..)
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), EvidencePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.IxSet qualified as IxSet
import Competences.Document
  ( Competence (..)
  , Document (..)
  , Evidence (..)
  , LevelInfo (..)
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
import Competences.Frontend.Component.EvidenceEditor.BulkEvidenceEditor (bulkEvidenceEditorComponent)
import Competences.Frontend.Component.Selector.EvidenceSelector
  ( EvidenceSelectorStyle (..)
  , evidenceSelectorComponent
  )
import Competences.Frontend.Component.Selector.MultiTaskSelector (searchableMultiTaskEditorField)
import Competences.Frontend.Component.Selector.ObservationSelector qualified as TE
import Competences.Frontend.Component.Selector.UserSelector (searchableSingleUserEditorField)
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
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

-- | Model for the evidence editor component
data EvidenceEditorModel = EvidenceEditorModel
  { selectedEvidence :: !(Maybe Evidence)
  , bulkEditorActive :: !Bool
  , activeMode :: !EvidenceMode
  }
  deriving (Eq, Generic, Show)

-- | Action for the evidence editor component
data EvidenceEditorAction
  = SwitchMode !EvidenceMode
  deriving (Eq, Show)

-- | Evidence editor component with support for bulk editing
evidenceEditorComponent
  :: SyncContext
  -> Bool
  -- ^ Can edit evidences? (True for teachers, False for students)
  -> M.Component p EvidenceEditorModel EvidenceEditorAction
evidenceEditorComponent r canEdit =
  let style = if canEdit then EvidenceSelectorViewAndCreate else EvidenceSelectorViewOnly
      defaultMode = if canEdit then EvidenceEdit else EvidenceView
   in M.component model update (mainView r style canEdit defaultMode)
  where
    model = EvidenceEditorModel Nothing False (if canEdit then EvidenceEdit else EvidenceView)
    update (SwitchMode mode) = M.modify $ #activeMode .~ mode

-- | Main view with selector on left and detail/bulk editor on right
mainView
  :: SyncContext
  -> EvidenceSelectorStyle
  -> Bool
  -> EvidenceMode
  -> EvidenceEditorModel
  -> M.View EvidenceEditorModel EvidenceEditorAction
mainView r style canEdit defaultMode m =
  V.sideMenu
    ( V.componentA
        "evidence-selector"
        [class_ "h-full"]
        (evidenceSelectorComponent r style #selectedEvidence #bulkEditorActive)
    )
    (detailPanel r canEdit defaultMode m)

-- | Detail panel - shows bulk editor or normal detail view
detailPanel
  :: SyncContext
  -> Bool
  -> EvidenceMode
  -> EvidenceEditorModel
  -> M.View EvidenceEditorModel EvidenceEditorAction
detailPanel r canEdit _defaultMode m
  | m.bulkEditorActive =
      V.component "bulk-evidence-editor" (bulkEvidenceEditorComponent r)
  | otherwise = case m.selectedEvidence of
      Nothing -> Typography.muted (C.translate' C.LblPleaseSelectItem)
      Just evidence ->
        V.viewFlow
          ( V.vFlow
              & (#expandDirection .~ V.Expand V.Start)
              & (#expandOrthogonal .~ V.Expand V.Start)
              & (#gap .~ V.MediumSpace)
          )
          [ if canEdit then modeSwitcher m else V.empty
          , V.flexGrow $
              if canEdit
                then evidenceEditorDetailView r evidence
                else evidenceViewerDetailView r evidence
          ]

-- | Mode switcher buttons
modeSwitcher :: EvidenceEditorModel -> M.View EvidenceEditorModel EvidenceEditorAction
modeSwitcher m =
  V.viewFlow
    ( V.hFlow
        & (#expandDirection .~ V.Expand V.Center)
    )
    [ Button.buttonGroup
        [ modeButton m.activeMode EvidenceView (C.translate' C.LblView) (Just IcnView)
        , modeButton m.activeMode EvidenceEdit (C.translate' C.LblEdit) (Just IcnEdit)
        ]
    ]

modeButton
  :: EvidenceMode
  -> EvidenceMode
  -> M.MisoString
  -> Maybe Icon
  -> M.View EvidenceEditorModel EvidenceEditorAction
modeButton activeMode mode label mIcon =
  let variant = if mode == activeMode then Button.Primary else Button.Outline
      baseButton =
        Button.button variant label
          & Button.withSize Button.Small
          & Button.withClick (SwitchMode mode)
   in case mIcon of
        Nothing -> Button.renderButton baseButton
        Just icon -> Button.renderButton (Button.withIcon icon baseButton)

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
  -> M.View EvidenceEditorModel EvidenceEditorAction
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
          case comp.levels Map.!? level of
            Just levelInfo -> levelInfo.description
            Nothing -> comp.description <> " - " <> T.pack (show level)
        Nothing -> T.pack $ show (competenceId, level)

-- ============================================================================
-- EDIT MODE DETAIL
-- ============================================================================

-- | Detail view for editing an evidence
evidenceEditorDetailView
  :: SyncContext
  -> Evidence
  -> M.View EvidenceEditorModel EvidenceEditorAction
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
