-- | Detailed task view: pure view primitives and pure state machine.
--
-- Effects for the state machine live in 'Component.Task.Detailed.Embed'.
module Competences.Frontend.Fragment.Task.Detailed
  ( -- * State machine
    TaskDetailedState (..)
  , TaskDetailedAction (..)
  , initialTaskDetailedState
  , updateTaskDetailedPure
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
  )
where

import Competences.Common.Set (toggle)
import Competences.Document (Task)
import Competences.Document.Solution (SolutionId, SolutionType (..))
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Color (PaletteName)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.Fragment.Task.Badge (taskStatusHeaderBg, taskStatusPalette)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)
import Optics.Core ((%~), (.~))

-- ============================================================================
-- State machine
-- ============================================================================

-- | Shared state for the detailed task view: expansion and hold-to-delete.
data TaskDetailedState = TaskDetailedState
  { expandedTasks :: !(Set TaskId)
  , expandedSolutions :: !(Set SolutionId)
  , holdDeleteSolution :: !(HoldButton.HoldState SolutionId)
  , menuDismissed :: !Bool
  }
  deriving (Eq, Generic, Show)

data TaskDetailedAction
  = ToggleTask !TaskId
  | ToggleSolution !SolutionId
  | AddSolution !TaskId
  | HoldDeleteSolution !(HoldButton.HoldAction SolutionId)
  | MenuEdit !TaskId
  | MenuPin !Task
  | MenuGoTo !TaskId
  | MenuDelete !TaskId
  | MenuReset
  deriving (Eq, Show)

-- | Initial state with a given set of initially-expanded tasks.
initialTaskDetailedState :: [TaskId] -> TaskDetailedState
initialTaskDetailedState expanded =
  TaskDetailedState
    { expandedTasks = Set.fromList expanded
    , expandedSolutions = Set.empty
    , holdDeleteSolution = HoldButton.emptyHoldState
    , menuDismissed = False
    }

-- | Pure update for the toggle branches; effectful branches are no-ops here.
updateTaskDetailedPure :: TaskDetailedAction -> TaskDetailedState -> TaskDetailedState
updateTaskDetailedPure (ToggleTask tid) = #expandedTasks %~ toggle tid
updateTaskDetailedPure (ToggleSolution sid) = #expandedSolutions %~ toggle sid
updateTaskDetailedPure MenuReset = #menuDismissed .~ False
updateTaskDetailedPure _ = id


-- ============================================================================
-- Task header
-- ============================================================================

-- | Simple task header: icon + display name.
taskHeader :: MisoString -> M.View m a
taskHeader displayName = Disclosure.titleIconText Icon.IcnTask displayName

-- | Task header with right-side annotations (badges, buttons).
taskHeaderWithBadges :: MisoString -> [M.View m a] -> M.View m a
taskHeaderWithBadges displayName extras =
  Disclosure.titleWithAnnotation
    (Disclosure.titleIconText Icon.IcnTask displayName)
    (Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) extras)

-- ============================================================================
-- Task content
-- ============================================================================

-- | Render task content (pre-rendered rich text).
-- Takes the already-rendered content as a View parameter.
taskContentView :: M.View m a -> M.View m a
taskContentView renderedContent =
  MH.div_
    [class_ "prose prose-stone prose-sm max-w-none"]
    [renderedContent]

-- | Render task content as a collapsible inner disclosure.
taskContentDisclosure
  :: Bool
  -- ^ Is expanded
  -> M.View m a
  -- ^ Rendered content
  -> a
  -- ^ Toggle action
  -> M.View m a
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

-- | Solution type label.
solutionTypeLabel :: SolutionType -> MisoString
solutionTypeLabel = C.translate' . C.LblSolutionType

-- | Render a solution as an inner disclosure (collapsible).
solutionView
  :: MisoString
  -- ^ Solution type label
  -> Bool
  -- ^ Is expanded
  -> M.View m a
  -- ^ Rendered solution content
  -> [Disclosure.DisclosureAction m a]
  -- ^ Header actions (edit, delete — empty for non-teachers)
  -> a
  -- ^ Toggle action
  -> M.View m a
solutionView typeLabel isExpanded renderedContent actions toggleAction =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents
      (Disclosure.titleIconText Icon.IcnSolution typeLabel)
      isExpanded
      renderedContent
      actions

-- | Render a solution always visible (no disclosure).
solutionInlineView :: MisoString -> M.View m a -> M.View m a
solutionInlineView typeLabel renderedContent =
  Layout.vFlow Layout.gapMicro
    [ Typography.small typeLabel
    , renderedContent
    ]

-- ============================================================================
-- Composites
-- ============================================================================

-- | Render a task item: disclosure if there's body content, static header otherwise.
taskItemView
  :: Maybe TaskCompletionStatus
  -> a
  -- ^ Toggle action
  -> MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Header annotations (right side)
  -> Bool
  -- ^ Expanded
  -> Maybe (M.View m a)
  -- ^ Body content ('Nothing' for non-expandable tasks)
  -> M.View m a
taskItemView mStatus toggleAction displayName annotations isExpanded = \case
  Just body -> taskDisclosureView (taskStatusPalette mStatus) toggleAction displayName annotations isExpanded body
  Nothing -> taskStaticHeader displayName (taskStatusHeaderBg mStatus) annotations

-- | Collapsible task view (disclosure).
taskDisclosureView
  :: Maybe PaletteName
  -> a
  -- ^ Toggle action
  -> MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Header annotations (right side)
  -> Bool
  -- ^ Expanded
  -> M.View m a
  -- ^ Body content
  -> M.View m a
taskDisclosureView mPalette toggleAction displayName annotations isExpanded body =
  let title = taskHeaderWithBadges displayName annotations
   in Disclosure.maybePaletteDisclosure mPalette toggleAction $
        Disclosure.contents title isExpanded body []

-- | Always-open task view (same frame as disclosure, no chevron or toggle).
taskOpenView
  :: MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Header annotations (right side)
  -> M.View m a
  -- ^ Body content
  -> M.View m a
taskOpenView displayName annotations body =
  Disclosure.staticDisclosure $
    Disclosure.contents (taskHeaderWithBadges displayName annotations) True body []

-- | Non-expandable task header (no body content).
taskStaticHeader
  :: MisoString
  -- ^ Display name
  -> Text
  -- ^ Header background class (e.g., from 'taskStatusHeaderBg')
  -> [M.View m a]
  -- ^ Annotations (right side)
  -> M.View m a
taskStaticHeader displayName headerBg annotations =
  MH.div_
    [class_ "border rounded-lg overflow-hidden"]
    [ MH.div_
        [class_ $ "flex items-center justify-between px-3 py-2 " <> headerBg]
        [ taskHeader displayName
        , Layout.hFlow (Layout.gapS <> Layout.crossCenter) annotations
        ]
    ]

-- | Always-expanded task card.
taskCardView
  :: MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Body content sections
  -> M.View m a
taskCardView displayName bodyParts =
  Card.contentCard Icon.IcnTask displayName bodyParts
