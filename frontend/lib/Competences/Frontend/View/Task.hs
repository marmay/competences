-- | Pure view primitives for rendering tasks.
--
-- No SyncContext dependency — takes pre-rendered content as parameters.
-- Used by 'Component.Task' (connected wrapper) and directly by views
-- that already have the necessary data.
module Competences.Frontend.View.Task
  ( -- * Task header
    taskHeader
  , taskHeaderWithBadges
    -- * Purpose badge
  , purposeBadge
  , purposeBadgeVariant
  , assessmentStar
    -- * Task content
  , taskContentView
    -- * Solutions
  , solutionView
  , solutionInlineView
  , solutionTypeLabel
    -- * Status styling
  , taskStatusPalette
  , taskStatusHeaderBg
  )
where

import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task (TaskPurpose (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Color (PaletteName)
import Competences.Frontend.View.Color.Status qualified as Status
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus (..))
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

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
-- Purpose badge
-- ============================================================================

-- | Render a Practice/Assessment badge.
purposeBadge :: TaskPurpose -> M.View m a
purposeBadge purpose =
  Badge.variant
    (purposeBadgeVariant purpose)
    (Badge.badgeLabel $ C.LblTaskPurpose purpose)

-- | Badge variant for a task purpose.
purposeBadgeVariant :: TaskPurpose -> Badge.BadgeVariant
purposeBadgeVariant Practice = Badge.Secondary
purposeBadgeVariant Assessment = Badge.Primary

-- | Assessment star icon (shown alongside the purpose badge).
assessmentStar :: TaskPurpose -> M.View m a
assessmentStar Assessment = Icon.icon [class_ "w-4 h-4 text-amber-500"] Icon.IcnStar
assessmentStar Practice = Layout.empty

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
  -> a
  -- ^ Toggle action
  -> M.View m a
solutionView typeLabel isExpanded renderedContent toggleAction =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents
      (Disclosure.titleIconText Icon.IcnSolution typeLabel)
      isExpanded
      renderedContent
      []

-- | Render a solution always visible (no disclosure).
solutionInlineView :: MisoString -> M.View m a -> M.View m a
solutionInlineView typeLabel renderedContent =
  Layout.vFlow Layout.gapMicro
    [ Typography.small typeLabel
    , renderedContent
    ]

-- ============================================================================
-- Status styling
-- ============================================================================

-- | Convert task completion status to a color palette.
taskStatusPalette :: Maybe TaskCompletionStatus -> Maybe PaletteName
taskStatusPalette (Just (TaskDone _)) = Just (Status.statusPalette Status.Ok)
taskStatusPalette (Just (TaskNotDone _)) = Just (Status.statusPalette Status.Pending)
taskStatusPalette _ = Nothing

-- | Header background class based on task completion status.
taskStatusHeaderBg :: Maybe TaskCompletionStatus -> Text
taskStatusHeaderBg (Just (TaskDone _)) = "bg-status-ok"
taskStatusHeaderBg (Just (TaskNotDone _)) = "bg-status-pending"
taskStatusHeaderBg _ = "bg-muted/50"
