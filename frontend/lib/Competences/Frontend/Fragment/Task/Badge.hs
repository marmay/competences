-- | Badge primitives for task rendering.
--
-- Purpose badges, status palettes, assessment stars.
-- Leaf module with no View.Task.* dependencies.
module Competences.Frontend.Fragment.Task.Badge
  ( -- * Purpose badge
    purposeBadge
  , purposeBadgeVariant
  , assessmentStar
    -- * Status styling
  , taskStatusPalette
  , taskStatusHeaderBg
  )
where

import Competences.Document.Task (TaskPurpose (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Color (PaletteName)
import Competences.Frontend.View.Color.Status qualified as Status
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.TaskStatus (TaskCompletionStatus (..))
import Data.Text (Text)
import Miso qualified as M

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
