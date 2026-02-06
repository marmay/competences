-- | Completion status color palettes for task/assignment progress indicators.
--
-- Maps completion states to CSS color palettes for consistent status visualization.
module Competences.Frontend.View.Color.Completion
  ( CompletionStatus (..)
  , completionPalette
  )
where

import Competences.Frontend.View.Color (PaletteName (..))

-- | Completion status for tasks and assignments.
data CompletionStatus
  = Open -- ^ Not started (muted gray)
  | InProgress -- ^ Working on it (yellow)
  | Done -- ^ Completed by student (green)
  | Assessed -- ^ Teacher verified (sky/primary)
  | Failed -- ^ Not achieved (red)
  deriving (Eq, Show)

-- | Map a completion status to its CSS color palette.
--
-- @completionPalette Open@ = @PaletteName "completion-open"@ (gray/muted)
-- @completionPalette InProgress@ = @PaletteName "completion-progress"@ (yellow)
-- @completionPalette Done@ = @PaletteName "completion-done"@ (green)
-- @completionPalette Assessed@ = @PaletteName "completion-assessed"@ (sky)
-- @completionPalette Failed@ = @PaletteName "completion-failed"@ (red)
completionPalette :: CompletionStatus -> PaletteName
completionPalette Open = PaletteName "completion-open"
completionPalette InProgress = PaletteName "completion-progress"
completionPalette Done = PaletteName "completion-done"
completionPalette Assessed = PaletteName "completion-assessed"
completionPalette Failed = PaletteName "completion-failed"
