-- | Common two-phase list reorder pattern.
--
-- Phase 1: User selects source item ("Start reorder")
-- Phase 2: User picks target position ("Insert before/after")
module Competences.Frontend.Common.ListReorder
  ( ListReorderState (..)
  , initialListReorderState
  , ListReorderAction (..)
  , ListReorderButtons (..)
  , listReorderButtons
  , moveElement
  )
where

import GHC.Generics (Generic)

-- | Tracks which item (by index) is selected for reordering.
newtype ListReorderState = ListReorderState
  { reorderFrom :: Maybe Int
  }
  deriving (Eq, Show, Generic)

-- | Initial state with no selection.
initialListReorderState :: ListReorderState
initialListReorderState = ListReorderState Nothing

-- | Actions for two-phase list reorder.
data ListReorderAction
  = -- | Select source item by index
    StartListReorder !Int
  | -- | Cancel the reorder
    CancelListReorder
  | -- | Move source index to before target index (in the original list)
    ListReorderTo !Int !Int
  deriving (Eq, Show)

-- | What buttons to show for an item at a given index.
data ListReorderButtons
  = -- | Normal mode: show a "reorder" button
    ShowReorderStart
  | -- | This item is the selected source: show "cancel"
    ShowReorderCancel
  | -- | Reorder in progress, this is a target: show up/down arrows
    --   Fields: source index, this item's index
    ShowReorderTargets !Int !Int
  deriving (Eq, Show)

-- | Determine which buttons to show for an item at a given index.
listReorderButtons :: ListReorderState -> Int -> ListReorderButtons
listReorderButtons (ListReorderState Nothing) _idx = ShowReorderStart
listReorderButtons (ListReorderState (Just fromIdx)) idx
  | fromIdx == idx = ShowReorderCancel
  | otherwise = ShowReorderTargets fromIdx idx

-- | Move element from source index to before target index (target is in the original list).
moveElement :: Int -> Int -> [a] -> [a]
moveElement from to xs
  | from == to || from + 1 == to = xs -- no-op
  | otherwise =
      case splitAt from xs of
        (before, e : rest) ->
          let removed = before <> rest
              insertIdx = if to > from then to - 1 else to
              (b2, a2) = splitAt insertIdx removed
           in b2 <> [e] <> a2
        _ -> xs
