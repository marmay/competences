-- | Detailed task view: state machine, view primitives, effectful update,
-- and full Miso component.
module Competences.Frontend.Component.Task.Detailed
  ( -- * State machine
    TaskDetailedState (..)
  , TaskDetailedAction (..)
  , initialTaskDetailedState
  , updateTaskDetailedPure
    -- * Embeddable update
  , updateTaskDetailed
    -- * Menu entries
  , addSolutionExtraEntry
  , exportTaskExtraEntry
    -- * Task list rendering
  , taskListView
  , renderSolutionList
    -- * Task header
  , taskHeader
  , taskHeaderWithBadges
    -- * Task content
  , taskContentView
  , taskContentDisclosure
    -- * Solutions
  , solutionView
  , solutionInlineView
  , solutionTypeLabel
    -- * Composites
  , taskItemView
  , taskDisclosureView
  , taskOpenView
  , taskStaticHeader
  , taskCardView
    -- * Full component
  , TaskDetailedConfig (..)
  , TaskDetailedSettings (..)
  , defaultTaskDetailedSettings
  , taskDetailedComponent
    -- * History row helpers
  , historyRowMenu
  )
where

import Competences.Frontend.Common.Effect (liftEffect_)
import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.Set (toggle)
import Competences.Document (Assignment (..), Document (..), Solution (..), Task (..), User (..))
import Competences.Document.Evidence (Evidence (..))
import Competences.Document.Solution (SolutionId, SolutionType (..), mkSolution)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Exchange.Build (taskExchange)
import Competences.Frontend.Clipboard (copyToClipboard)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.EntityMenu qualified as EM
import Competences.Frontend.Exchange (encodeExchangeYaml)
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.Component.Task.EditButton (solutionEditButton)
import Competences.Frontend.Fragment.Task.Badge (assessmentStar, taskStatusHeaderBg, taskStatusPalette)
import Competences.Frontend.Fragment.Task.History
  ( EvidenceRow (..)
  , evaluationHistory
  )
import Competences.Frontend.Fragment.Task.History qualified as History
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.Fragment.TaskStatus (viewTaskCompletionStatus)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , PinViewerRequest (..)
  , SyncContext (..)
  , isTeacher
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.SyncDocument (SyncDocument (..), SyncDocumentEnv (..), readSyncDocument, syncDocumentEnv)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Color (PaletteName)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus
  ( TaskCompletionStatus (..)
  , taskCompletionStatus
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, ms)
import Optics.Core (Lens', (%), (%~), (.~))

-- ============================================================================
-- State machine
-- ============================================================================

data TaskDetailedState = TaskDetailedState
  { expandedTasks :: !(Set TaskId)
  , expandedSolutions :: !(Set SolutionId)
  , collapsedHistory :: !(Set TaskId)
  -- ^ Inverted: history is open by default, this set tracks which
  -- tasks have had their history section explicitly hidden.
  , holdDeleteSolution :: !(HoldButton.HoldState SolutionId)
  }
  deriving (Eq, Generic, Show)

data TaskDetailedAction
  = ToggleTask !TaskId
  | ToggleSolution !SolutionId
  | ToggleHistory !TaskId
  | HoldDeleteSolution !(HoldButton.HoldAction SolutionId)
  deriving (Eq, Show)

initialTaskDetailedState :: [TaskId] -> TaskDetailedState
initialTaskDetailedState expanded =
  TaskDetailedState
    { expandedTasks = Set.fromList expanded
    , expandedSolutions = Set.empty
    , collapsedHistory = Set.empty
    , holdDeleteSolution = HoldButton.emptyHoldState
    }

updateTaskDetailedPure :: TaskDetailedAction -> TaskDetailedState -> TaskDetailedState
updateTaskDetailedPure (ToggleTask tid) = #expandedTasks %~ toggle tid
updateTaskDetailedPure (ToggleSolution sid) = #expandedSolutions %~ toggle sid
updateTaskDetailedPure (ToggleHistory tid) = #collapsedHistory %~ toggle tid
updateTaskDetailedPure _ = id

-- ============================================================================
-- Embeddable update
-- ============================================================================

updateTaskDetailed
  :: Lens' model TaskDetailedState
  -> SyncContext
  -> (TaskDetailedAction -> action)
  -> TaskDetailedAction
  -> M.Effect parent model action
updateTaskDetailed stateLens r lift = go
  where
    go (HoldDeleteSolution ha) =
      liftEffect_ (stateLens % #holdDeleteSolution) (lift . HoldDeleteSolution) $
        HoldButton.updateHold (\sid -> modifySyncDocument r $ Solutions (OnSolutions (Delete sid))) ha
    go action = M.modify (stateLens %~ updateTaskDetailedPure action)

-- ============================================================================
-- Menu entries
-- ============================================================================

-- | EntityMenu entry that creates a new solution for the given task and
-- hands editing over to the lock-watching flow. Only meaningful for
-- teachers; callers should gate on 'isTeacher'.
addSolutionExtraEntry :: SyncContext -> TaskId -> EM.ExtraEntry
addSolutionExtraEntry r tid = EM.ExtraEntry
  { EM.icon = Icon.IcnAdd
  , EM.label = C.translate' C.LblAddSolution
  , EM.action = do
      solId <- nextId r
      let uid = (syncDocumentEnv r).connectedUser.id
      modifySyncDocument r $ Solutions (OnSolutions (CreateAndLock (mkSolution solId tid uid)))
  }

-- | EntityMenu entry that copies the task as YAML onto the clipboard.
-- The 'EntityOrigin' selects whether the task lives in the published
-- or draft pool — exporters round-trip back into the same one.
exportTaskExtraEntry :: SyncContext -> Task -> EntityOrigin -> EM.ExtraEntry
exportTaskExtraEntry r task origin = EM.ExtraEntry
  { EM.icon = Icon.IcnExport
  , EM.label = C.translate' C.LblExport
  , EM.action = exportTaskToClipboard r task origin
  }

exportTaskToClipboard :: SyncContext -> Task -> EntityOrigin -> IO ()
exportTaskToClipboard r task origin = do
  syncDoc <- readSyncDocument r
  let xdoc = taskExchange syncDoc.localDocument (origin == Draft) task
  encodeExchangeYaml xdoc $ \case
    Left reason -> putStrLn $ "Export failed: " <> T.unpack reason
    Right yamlText -> copyToClipboard yamlText

-- ============================================================================
-- Task list rendering
-- ============================================================================

taskListView
  :: SyncContext
  -> TaskDetailedState
  -> (TaskId -> Maybe TaskCompletionStatus)
  -> (TaskWithSolutions -> [M.View m a])
  -> (TaskId -> [M.View m a])
  -> (TaskDetailedAction -> a)
  -> [TaskWithSolutions]
  -> M.View m a
taskListView _ _ _ _ _ _ [] =
  Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
taskListView r state statusLookup mkAnnotations mkExtraBody liftAction tasks =
  Layout.vFlow Layout.gapM (map renderOne tasks)
  where
    renderOne tws =
      let tid = tws.task.id
          name = ms (taskDisplayName tws.task)
          expanded = Set.member tid state.expandedTasks
          contentPresent = case tws.taskContent of
            Nothing -> False
            Just c -> c /= mempty
          solsPresent = not (null tws.solutions)
          extra = mkExtraBody tid

          parts = concat
            [ [ taskContentView (renderRichTextWithFiles r.formulaCache r tws.task.attachments rc)
              | contentPresent
              , Just rc <- [tws.taskContent]
              ]
            , [renderSolutionList r state liftAction tid tws.solutions | solsPresent]
            , extra
            ]

          mBody = if null parts then Nothing else Just (MH.div_ [class_ "space-y-3"] parts)
       in taskItemView (statusLookup tid) (liftAction (ToggleTask tid)) name (mkAnnotations tws) expanded mBody

renderSolutionList
  :: SyncContext
  -> TaskDetailedState
  -> (TaskDetailedAction -> a)
  -> TaskId
  -> [Solution]
  -> M.View m a
renderSolutionList r state liftAction _tid sols =
  MH.div_ [class_ "space-y-1"]
    (map (renderOneSol r state liftAction (isTeacher r)) sols)

renderOneSol
  :: SyncContext
  -> TaskDetailedState
  -> (TaskDetailedAction -> a)
  -> Bool
  -> Solution
  -> M.View m a
renderOneSol r state liftAction isTeacher' sol =
  let isExpanded = Set.member sol.id state.expandedSolutions
      rendered
        | sol.content == mempty = Typography.muted (C.translate' C.LblNoContent)
        | otherwise = taskContentView (renderRichText r.formulaCache sol.content)
      actions
        | isTeacher' =
            [ Disclosure.viewAction (solutionEditButton r sol)
            , Disclosure.holdDestructiveAction (liftAction . HoldDeleteSolution) state.holdDeleteSolution sol.id
            ]
        | otherwise = []
   in solutionView (solutionTypeLabel sol.solutionType) isExpanded rendered actions (liftAction (ToggleSolution sol.id))

-- ============================================================================
-- Task header
-- ============================================================================

taskHeader :: MisoString -> M.View m a
taskHeader displayName = Disclosure.titleIconText Icon.IcnTask displayName

taskHeaderWithBadges :: MisoString -> [M.View m a] -> M.View m a
taskHeaderWithBadges displayName extras =
  Disclosure.titleWithAnnotation
    (Disclosure.titleIconText Icon.IcnTask displayName)
    (Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) extras)

-- ============================================================================
-- Task content
-- ============================================================================

taskContentView :: M.View m a -> M.View m a
taskContentView renderedContent =
  MH.div_
    [class_ "prose prose-stone prose-sm max-w-none"]
    [renderedContent]

taskContentDisclosure :: Bool -> M.View m a -> a -> M.View m a
taskContentDisclosure isExpanded renderedContent toggleAction =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents
      (Disclosure.titleText (C.translate' C.LblTaskContent))
      isExpanded
      (taskContentView renderedContent)
      []

-- ============================================================================
-- Solutions
-- ============================================================================

solutionTypeLabel :: SolutionType -> MisoString
solutionTypeLabel = C.translate' . C.LblSolutionType

solutionView
  :: MisoString -> Bool -> M.View m a -> [Disclosure.DisclosureAction m a] -> a -> M.View m a
solutionView typeLabel isExpanded renderedContent actions toggleAction =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents
      (Disclosure.titleIconText Icon.IcnSolution typeLabel)
      isExpanded
      renderedContent
      actions

solutionInlineView :: MisoString -> M.View m a -> M.View m a
solutionInlineView typeLabel renderedContent =
  Layout.vFlow Layout.gapMicro
    [ Typography.small typeLabel
    , renderedContent
    ]

-- ============================================================================
-- Composites
-- ============================================================================

taskItemView
  :: Maybe TaskCompletionStatus -> a -> MisoString -> [M.View m a] -> Bool -> Maybe (M.View m a) -> M.View m a
taskItemView mStatus toggleAction displayName annotations isExpanded = \case
  Just body -> taskDisclosureView (taskStatusPalette mStatus) toggleAction displayName annotations isExpanded body
  Nothing -> taskStaticHeader displayName (taskStatusHeaderBg mStatus) annotations

taskDisclosureView
  :: Maybe PaletteName -> a -> MisoString -> [M.View m a] -> Bool -> M.View m a -> M.View m a
taskDisclosureView mPalette toggleAction displayName annotations isExpanded body =
  let title = taskHeaderWithBadges displayName annotations
   in Disclosure.maybePaletteDisclosure mPalette toggleAction $
        Disclosure.contents title isExpanded body []

taskOpenView :: MisoString -> [M.View m a] -> M.View m a -> M.View m a
taskOpenView displayName annotations body =
  Disclosure.staticDisclosure $
    Disclosure.contents (taskHeaderWithBadges displayName annotations) True body []

taskStaticHeader :: MisoString -> Text -> [M.View m a] -> M.View m a
taskStaticHeader displayName headerBg annotations =
  MH.div_
    [class_ "border rounded-lg overflow-hidden"]
    [ MH.div_
        [class_ $ "flex items-center justify-between px-3 py-2 " <> headerBg]
        [ taskHeader displayName
        , Layout.hFlow (Layout.gapS <> Layout.crossCenter) annotations
        ]
    ]

taskCardView :: MisoString -> [M.View m a] -> M.View m a
taskCardView displayName bodyParts =
  Card.contentCard Icon.IcnTask displayName bodyParts

-- ============================================================================
-- Full component
-- ============================================================================

data TaskDetailedConfig = TaskDetailedConfig
  { taskId :: !TaskId
  , origin :: !EntityOrigin
  , settings :: !TaskDetailedSettings
  }

data TaskDetailedSettings = TaskDetailedSettings
  { collapsible :: !Bool
  , showSolutions :: !Bool
  , showAnnotations :: !Bool
  , startExpanded :: !Bool
  , enableGoTo :: !Bool
  , enableDelete :: !Bool
  }
  deriving (Eq, Show)

defaultTaskDetailedSettings :: TaskDetailedSettings
defaultTaskDetailedSettings = TaskDetailedSettings
  { collapsible = False
  , showSolutions = True
  , showAnnotations = True
  , startExpanded = True
  , enableGoTo = True
  , enableDelete = False
  }

data TaskProjection = TaskProjection
  { task :: !(Maybe Task)
  , solutions :: ![Solution]
  , focusedUser :: !(Maybe User)
  , history :: ![EvidenceRow]
  , headerStatus :: !TaskCompletionStatus
  }
  deriving (Eq, Generic, Show)

data ComponentModel = ComponentModel
  { projection :: !TaskProjection
  , viewState :: !TaskDetailedState
  }
  deriving (Eq, Generic, Show)

data ComponentAction
  = ProjectionChanged !(ProjectedChange TaskProjection)
  | ViewAction !TaskDetailedAction
  deriving (Eq, Show)

taskDetailedComponent :: SyncContext -> TaskDetailedConfig -> M.Component p ComponentModel ComponentAction
taskDetailedComponent r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (taskProjection cfg) ProjectionChanged]
    }
  where
    model = ComponentModel
      { projection = TaskProjection
          { task = Nothing
          , solutions = []
          , focusedUser = Nothing
          , history = []
          , headerStatus = TaskNotEvaluated
          }
      , viewState = initialTaskDetailedState [cfg.taskId | cfg.settings.startExpanded]
      }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction a) = updateTaskDetailed #viewState r ViewAction a

    view' m = case m.projection.task of
      Nothing -> Layout.empty
      Just task -> viewTask r cfg m task

taskProjection :: TaskDetailedConfig -> Document -> Maybe User -> TaskProjection
taskProjection cfg doc mUser =
  TaskProjection
    { task = mTask
    , solutions = Ix.toList (doc.solutions Ix.@= cfg.taskId)
    , focusedUser = mUser
    , history = case (mUser, mTask) of
        (Just u, Just t) -> evaluationHistory doc u t
        _ -> []
    , headerStatus = case (mUser, mTask) of
        (Just u, Just t) -> taskCompletionStatus doc u.id t
        _ -> TaskNotEvaluated
    }
  where
    mTask = case cfg.origin of
      Published -> Ix.getOne (doc.tasks Ix.@= cfg.taskId)
      Draft -> Ix.getOne (doc.draftTasks Ix.@= cfg.taskId)

viewTask :: SyncContext -> TaskDetailedConfig -> ComponentModel -> Task -> M.View ComponentModel ComponentAction
viewTask r cfg m task =
  let displayName = ms (taskDisplayName task)
      annotations
        | cfg.settings.showAnnotations = headerAnnotations r cfg m task
        | otherwise = []
      body = taskBody r cfg m task
      expanded = Set.member cfg.taskId m.viewState.expandedTasks
   in if cfg.settings.collapsible
        then taskDisclosureView Nothing (ViewAction (ToggleTask cfg.taskId)) displayName annotations expanded body
        else taskOpenView displayName annotations body

headerAnnotations :: SyncContext -> TaskDetailedConfig -> ComponentModel -> Task -> [M.View ComponentModel ComponentAction]
headerAnnotations r cfg m task =
  concat
    [ [viewTaskCompletionStatus m.projection.headerStatus]
    , [assessmentStar task.purpose]
    , [ inlineComponent ("task-menu-" <> ms (show task.id))
          (EM.entityMenuComponent r EM.EntityMenuConfig
            { edit = Just (EM.taskEdit task.id cfg.origin)
            , pin = Just (PinTaskViewer task)
            , goTo = if cfg.settings.enableGoTo then Just (ManageTasks (Just task.id)) else Nothing
            , delete = if cfg.settings.enableDelete then Just (EM.taskDelete task.id cfg.origin) else Nothing
            , extraEntries = [addSolutionExtraEntry r task.id, exportTaskExtraEntry r task cfg.origin]
            })
      | isTeacher r
      ]
    ]

taskBody :: SyncContext -> TaskDetailedConfig -> ComponentModel -> Task -> M.View ComponentModel ComponentAction
taskBody r cfg m task =
  MH.div_ [class_ "space-y-3"] $
    concat
      [ [taskContentRendered r task | hasContent task]
      , [ renderSolutionList r m.viewState ViewAction cfg.taskId m.projection.solutions
        | cfg.settings.showSolutions
        , not (null m.projection.solutions) || isTeacher r
        ]
      , [historySection r cfg m | not (null m.projection.history)]
      ]

-- ============================================================================
-- Evaluation history
-- ============================================================================

historySection :: SyncContext -> TaskDetailedConfig -> ComponentModel -> M.View ComponentModel ComponentAction
historySection r cfg m =
  History.historySection
    (renderRichText r.formulaCache)
    (historyRowMenu r)
    m.viewState.collapsedHistory
    (ViewAction . ToggleHistory)
    cfg.taskId
    m.projection.history

-- | Entity-menu slot for a single evaluation-history row. Polymorphic
-- in the parent's model/action so both the standalone task detailed
-- view and the assignment viewer can reuse it.
historyRowMenu :: SyncContext -> TaskId -> EvidenceRow -> M.View m a
historyRowMenu r taskId row = case row.assignment of
  Nothing -> Layout.empty
  Just a ->
    inlineComponent ("history-asn-menu-" <> ms (show taskId) <> "-" <> ms (show row.evidence.id))
      ( EM.entityMenuComponent r EM.EntityMenuConfig
          { edit = Nothing
          , pin = Just (PinAssignmentViewer a)
          , goTo = Just (ManageAssignments (Just a.id))
          , delete = Nothing
          , extraEntries = []
          }
      )

hasContent :: Task -> Bool
hasContent task = case task.content of
  Nothing -> False
  Just c -> c /= mempty

taskContentRendered :: SyncContext -> Task -> M.View ComponentModel ComponentAction
taskContentRendered r task = case task.content of
  Nothing -> Layout.empty
  Just content ->
    if content == mempty
      then Layout.empty
      else taskContentView (renderRichTextWithFiles r.formulaCache r task.attachments content)
