-- | Assignment completion color palette.
--
-- Maps 'AssignmentCompletionCategory' values to semantic color palettes
-- using CSS variables defined in @input.css@.
module Competences.Frontend.View.Color.AssignmentCompletion
  ( assignmentCompletionPalette
  )
where

import Competences.Frontend.View.Color (PaletteName (..))
import Competences.Query.Assignment (AssignmentCompletionCategory (..))

-- | Get the color palette name for an assignment completion category.
assignmentCompletionPalette :: AssignmentCompletionCategory -> PaletteName
assignmentCompletionPalette AsgCompleted = PaletteName "asg-completed"
assignmentCompletionPalette AsgCorrectedNotDone = PaletteName "asg-corrected"
assignmentCompletionPalette AsgSubmittedNotCorrected = PaletteName "asg-submitted"
assignmentCompletionPalette AsgNotSubmitted = PaletteName "asg-missing"
