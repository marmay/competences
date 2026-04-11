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
  ( Assignment (..)
  , Competence (..)
  , Document (..)
  , Evidence (..)
  , LevelInfo (..)
  , Lock (..)
  , User (..)
  , emptyDocument
  , lockOwner
  )
import Competences.Document.Evidence (Observation (..))
import Competences.Document.Task (Task (..), TaskId, taskDisplayName)
import Competences.Document.User (UserId, isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), entityPatchTransformedLens)
import Competences.Frontend.Component.EvidenceEditor.BulkEvidenceEditor (bulkEvidenceEditorComponent)
import Competences.Frontend.Component.Selector.AssignmentSelector (searchableSingleAssignmentEditorField)
import Competences.Frontend.Component.Selector.EvidenceSelector
  ( EvidenceSelectorStyle (..)
  , evidenceSelectorComponent
  )
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..))
import Competences.Frontend.Component.Selector.SearchSelectEditorField (searchSelectEditorField, searchSelectSingleEditorField)
import Competences.Query.Task qualified as QTask
import Competences.Frontend.Component.Selector.ObservationSelector qualified as TE
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Data.Bifunctor (bimap)
import Optics.Core ((&), (.~), (?~), lens)

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
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Action for the evidence editor component
data EvidenceEditorAction
  = SwitchMode !EvidenceMode
  | ToggleSidebar
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
    model = EvidenceEditorModel Nothing False (if canEdit then EvidenceEdit else EvidenceView) True
    update (SwitchMode mode) = M.modify $ #activeMode .~ mode
    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

-- | Main view with selector on left and detail/bulk editor on right
mainView
  :: SyncContext
  -> EvidenceSelectorStyle
  -> Bool
  -> EvidenceMode
  -> EvidenceEditorModel
  -> M.View EvidenceEditorModel EvidenceEditorAction
mainView r style canEdit defaultMode m =
  V.collapsibleSideMenu
    m.sidebarOpen
    ToggleSidebar
    ( V.inlineComponentAttrs
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
      V.inlineComponent "bulk-evidence-editor" (bulkEvidenceEditorComponent r)
  | otherwise = case m.selectedEvidence of
      Nothing -> V.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
      Just evidence ->
        V.vFlow (V.gapM <> V.wFull <> V.crossStart)
          [ if canEdit then modeSwitcher m else V.empty
          , V.flexGrow $
              if canEdit
                then evidenceEditorDetailView r evidence
                else evidenceViewerDetailView r evidence
          ]

-- | Mode switcher buttons
modeSwitcher :: EvidenceEditorModel -> M.View EvidenceEditorModel EvidenceEditorAction
modeSwitcher m =
  V.hFlow (V.wFull <> V.mainCenter)
    [ Button.buttonGroup
        [ modeButton m.activeMode EvidenceView (C.translate' C.LblView) (Just Icon.IcnView)
        , modeButton m.activeMode EvidenceEdit (C.translate' C.LblEdit) (Just Icon.IcnEdit)
        ]
    ]

modeButton
  :: EvidenceMode
  -> EvidenceMode
  -> M.MisoString
  -> Maybe Icon.Icon
  -> M.View EvidenceEditorModel EvidenceEditorAction
modeButton activeMode mode label mIcon =
   case mIcon of
     Nothing -> Button.toggleSm (mode == activeMode) (Button.button label (SwitchMode mode))
     Just icon -> Button.toggleSm (mode == activeMode) (Button.button (icon, label) (SwitchMode mode))

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
  V.inlineComponent
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
        , viewField (C.translate' C.LblTasksAndGroups) (viewTasks m (Map.keys evidence.tasks))
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
        Just task -> taskDisplayName task
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
  V.inlineComponent
    ("evidence-editor-" <> M.ms (show evidence.id))
    (TE.editorComponent evidenceEditor r)
  where
    evidenceEditorId = "evidence-editor-" <> M.ms (show evidence.id)
    evidenceEditable =
      TE.editable
        ( \d -> do
            fmap
              (\c -> (c, lockOwner (EvidenceLock c.id) d))
              (Ix.getOne $ d.evidences Ix.@= evidence.id)
        )
        & (#modify ?~ (\e modify -> Evidences $ OnEvidences (Modify e.id modify)))
        & (#delete ?~ (\e -> Evidences $ OnEvidences (Delete e.id)))
    -- | Lens bridging Map TaskId TaskEvaluations <-> [TaskId] for the task selector.
    -- The selector only edits which tasks are present (keys); evaluations are
    -- preserved for existing tasks and default to empty for newly added ones.
    tasksToTasksLens = EntityPatchTransformedLens
      { viewLens = lens
          (\e -> Map.keys e.tasks)
          (\e ids -> e & #tasks .~ Map.fromList
            [(tid, Map.findWithDefault Map.empty tid e.tasks) | tid <- ids])
      , patchLens = lens
          (\p -> fmap (bimap Map.keys Map.keys) p.tasks)
          (\p mc -> p & #tasks .~ fmap (bimap toTaskMap toTaskMap) mc)
      , transform = (.id)
      , embed = id
      }
    toTaskMap ids = Map.fromList [(tid, Map.empty) | tid <- ids]
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
                           , searchSelectSingleEditorField
                               r
                               (evidenceEditorId <> "-user")
                               userSearchConfig
                               (.userId)
                               (entityPatchTransformedLens #userId #userId (.id) id)
                           )
        `TE.addNamedField` ( C.translate' C.LblTasksAndGroups
                           , searchSelectEditorField
                               r
                               (evidenceEditorId <> "-tasks")
                               taskSearchConfig
                               (Map.keys . (.tasks))
                               tasksToTasksLens
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignments
                           , searchableSingleAssignmentEditorField
                               r
                               (evidenceEditorId <> "-assignment")
                               (entityPatchTransformedLens #assignmentId #assignmentId (.id) id)
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

-- | SearchSelect config for student user selection
userSearchConfig :: SearchSelectConfig User UserId
userSearchConfig =
  SearchSelectConfig
    { projectItems = \doc -> filter isStudent $ Ix.toAscList (Proxy @Text) doc.users
    , itemId = (.id)
    , itemLabel = (.name)
    , metaFilters = []
    , viewTag = \u -> (Icon.IcnSocialFormIndividual, M.ms u.name)
    , placeholder = M.fromMisoString $ C.translate' C.LblStudent
    , selectionOrder = AutoOrder id
    , tagLayout = TagsInline
    , onCreate = Nothing
    }

-- | Shared SearchSelect config for tasks
taskSearchConfig :: SearchSelectConfig Task TaskId
taskSearchConfig =
  SearchSelectConfig
    { projectItems = QTask.allTasksSorted
    , itemId = (.id)
    , itemLabel = taskDisplayName
    , metaFilters = []
    , viewTag = \t -> (Icon.IcnTask, M.ms $ taskDisplayName t)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectTasks
    , selectionOrder = AutoOrder id
    , tagLayout = TagsInline
    , onCreate = Nothing
    }
