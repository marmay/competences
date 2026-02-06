-- | Grade color palette.
--
-- Maps 'Grade' values to semantic color palettes. Uses a traffic-light
-- pattern based on grade quality:
--
-- * Grades 1-3: green (success)
-- * Grades 3-4, 4, 4-5: yellow (warning)
-- * Grade 5: red (danger)
--
-- Uses CSS variables defined in input.css following the same pattern
-- as ability, mastery, and status palettes.
module Competences.Frontend.View.Color.Grade
  ( gradePalette
  )
where

import Competences.Document.Grade (Grade (..))
import Competences.Frontend.View.Color (PaletteName (..))

-- | Get the color palette for a grade.
gradePalette :: Grade -> PaletteName
gradePalette g = case g of
  Grade1 -> PaletteName "grade-success"
  Grade1_2 -> PaletteName "grade-success"
  Grade2 -> PaletteName "grade-success"
  Grade2_3 -> PaletteName "grade-success"
  Grade3 -> PaletteName "grade-success"
  Grade3_4 -> PaletteName "grade-warning"
  Grade4 -> PaletteName "grade-warning"
  Grade4_5 -> PaletteName "grade-warning"
  Grade5 -> PaletteName "grade-danger"
