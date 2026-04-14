-- | Connected task view component.
--
-- A Miso component that subscribes to the document and renders a task
-- with configurable sections. Provides the Edit button (LockButton) and
-- solution management.
--
-- Usage:
--
-- @
-- inlineComponent ("task-" <> ms (show taskId))
--   (taskComponent r (TaskConfig taskId defaultTaskOptions))
-- @
module Competences.Frontend.Component.Task
  ( TaskConfig (..)
  , TaskOptions (..)
  , defaultTaskOptions
  , taskComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Task qualified as V
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((.~))

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for a task component instance.
data TaskConfig = TaskConfig
  { taskId :: !TaskId
  , origin :: !EntityOrigin
  , options :: !TaskOptions
  }

-- | Display options for the task component.
data TaskOptions = TaskOptions
  { showEditButton :: !Bool
  -- ^ Show LockButton (Edit) in the header
  , showSolutions :: !Bool
  -- ^ Show solutions section (with add-solution for teachers)
  , showPurposeBadge :: !Bool
  -- ^ Show Practice/Assessment badge
  , startExpanded :: !Bool
  -- ^ Start with content expanded
  }

-- | Default options: edit button, solutions, purpose badge, collapsed.
defaultTaskOptions :: TaskOptions
defaultTaskOptions = TaskOptions
  { showEditButton = True
  , showSolutions = True
  , showPurposeBadge = True
  , startExpanded = False
  }

-- ============================================================================
-- Model & Actions
-- ============================================================================

data TaskProjection = TaskProjection
  { task :: !(Maybe Task)
  , solutions :: ![Solution]
  , isTeacher :: !Bool
  }
  deriving (Eq, Generic, Show)

data Model = Model
  { projection :: !TaskProjection
  , expanded :: !Bool
  , expandedSolutions :: !(Set SolutionId)
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange TaskProjection)
  | ToggleExpanded
  | ToggleSolution !SolutionId
  | AddSolution
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

taskComponent :: SyncContext -> TaskConfig -> M.Component p Model Action
taskComponent r cfg =
  (M.component model update view)
    { M.subs = [subscribeWithProjection r (taskProjection cfg) ProjectionChanged]
    }
  where
    model = Model
      { projection = TaskProjection Nothing [] False
      , expanded = cfg.options.startExpanded
      , expandedSolutions = Set.empty
      }

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection
    update ToggleExpanded =
      M.modify $ \m -> m {expanded = not m.expanded}
    update (ToggleSolution sid) =
      M.modify $ \m -> m
        { expandedSolutions =
            if Set.member sid m.expandedSolutions
              then Set.delete sid m.expandedSolutions
              else Set.insert sid m.expandedSolutions
        }
    update AddSolution = pure () -- TODO: wire up solution creation

    view m = case m.projection.task of
      Nothing -> Layout.empty
      Just task -> viewTask r cfg m task

-- ============================================================================
-- Projection
-- ============================================================================

taskProjection :: TaskConfig -> Document -> Maybe User -> TaskProjection
taskProjection cfg doc mUser =
  let mTask = case cfg.origin of
        Published -> Ix.getOne (doc.tasks Ix.@= cfg.taskId)
        Draft -> Ix.getOne (doc.draftTasks Ix.@= cfg.taskId)
      solutions = Ix.toList (doc.solutions Ix.@= cfg.taskId)
      isTeacher = case mUser of
        Just u -> u.role == Teacher
        Nothing -> False
   in TaskProjection mTask solutions isTeacher

-- ============================================================================
-- View
-- ============================================================================

viewTask :: SyncContext -> TaskConfig -> Model -> Task -> M.View Model Action
viewTask r cfg m task =
  let displayName = ms (taskDisplayName task)
      hasContent = case task.content of
        Nothing -> False
        Just c -> c /= mempty
      hasSolutions = not (null m.projection.solutions)
      isExpandable = hasContent || hasSolutions

      -- Header
      headerLeft = V.taskHeader displayName
      headerRight = Layout.hFlow (Layout.gapS <> Layout.crossCenter) $
        concat
          [ [ V.purposeBadge task.purpose | cfg.options.showPurposeBadge ]
          , [ editButton r cfg task | cfg.options.showEditButton ]
          ]

      -- Body
      bodyContent = MH.div_
        [class_ "space-y-3"]
        ( concat
            [ [ taskContentRendered r task | hasContent ]
            , [ viewSolutions r m m.projection.solutions | cfg.options.showSolutions && hasSolutions ]
            , [ addSolutionButton | cfg.options.showSolutions && m.projection.isTeacher ]
            ]
        )
   in if isExpandable
        then
          Disclosure.disclosure ToggleExpanded $
            Disclosure.contents
              (Disclosure.titleWithAnnotation headerLeft headerRight)
              m.expanded
              bodyContent
              []
        else
          MH.div_
            [class_ "border rounded-lg overflow-hidden"]
            [ MH.div_
                [class_ "flex items-center justify-between px-3 py-2 bg-muted/50"]
                [headerLeft, headerRight]
            ]

-- | Render the Edit button (LockButton).
editButton :: SyncContext -> TaskConfig -> Task -> M.View Model Action
editButton r cfg task =
  let wrap = wrapForOrigin cfg.origin
   in inlineComponent
        ("task-edit-btn-" <> ms (show task.id))
        (lockButtonComponent r
          (LockButtonConfig (TaskLock task.id) (wrap (Tasks (OnTasks (Modify task.id Lock)))) Button.IconOnlyS))

-- | Render task content (rich text with file embeds).
taskContentRendered :: SyncContext -> Task -> M.View Model Action
taskContentRendered r task = case task.content of
  Nothing -> Layout.empty
  Just content ->
    if content == mempty
      then Layout.empty
      else V.taskContentView (renderRichTextWithFiles r.formulaCache r task.attachments content)

-- | Render solutions list.
viewSolutions :: SyncContext -> Model -> [Solution] -> M.View Model Action
viewSolutions r m sols =
  MH.div_
    [class_ "space-y-1"]
    (map (viewOneSolution r m) sols)

viewOneSolution :: SyncContext -> Model -> Solution -> M.View Model Action
viewOneSolution r m sol =
  let isExpanded = Set.member sol.id m.expandedSolutions
      rendered =
        if sol.content == mempty
          then Typography.muted "Kein Inhalt"
          else V.taskContentView (renderRichText r.formulaCache sol.content)
   in V.solutionView (V.solutionTypeLabel sol.solutionType) isExpanded rendered (ToggleSolution sol.id)

-- | Add solution button (for teachers).
addSolutionButton :: M.View Model Action
addSolutionButton =
  Button.secondary (Button.button (Button.IconTextS, Icon.IcnAdd, C.LblAddSolution) AddSolution)
