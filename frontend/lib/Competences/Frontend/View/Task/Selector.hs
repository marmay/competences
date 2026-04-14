-- | Selector list item rendering for tasks.
--
-- Pure view helpers for task items in selector panels.
-- Leaf module with no View.Task.* dependencies.
module Competences.Frontend.View.Task.Selector
  ( -- * Draft badge
    draftBadge
    -- * Selector items
  , taskSelectorItem
  )
where

import Competences.Document.Task (Task (..), taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Miso qualified as M
import Miso.String (ms)

-- | Draft badge, shown for draft tasks. Nothing for published tasks.
draftBadge :: EntityOrigin -> Maybe (M.View m a)
draftBadge Draft = Just $ Badge.secondary (Badge.badgeText (C.translate' C.LblDraft))
draftBadge Published = Nothing

-- | Render a task as a selector list item with optional draft badge.
taskSelectorItem :: Bool -> EntityOrigin -> Task -> a -> M.View m a
taskSelectorItem isSelected origin task action =
  SL.selectorItemWithBadge
    isSelected
    Icon.IcnTask
    (ms $ taskDisplayName task)
    (draftBadge origin)
    action
