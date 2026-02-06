-- | Grade color palette.
--
-- Maps 'Grade' values to semantic color palettes. Uses a traffic-light
-- pattern based on grade quality:
--
-- * Grades 1-3: green (good)
-- * Grades 3-4, 4, 4-5: yellow (warning)
-- * Grade 5: red (danger)
module Competences.Frontend.View.Color.Grade
  ( gradePalette
  )
where

import Competences.Document.Grade (Grade (..))
import Competences.Frontend.View.Color (ColorPalette (..))

-- | Get the color palette for a grade.
gradePalette :: Grade -> ColorPalette
gradePalette g = case g of
  Grade1 -> greenPalette
  Grade1_2 -> greenPalette
  Grade2 -> greenPalette
  Grade2_3 -> greenPalette
  Grade3 -> greenPalette
  Grade3_4 -> yellowPalette
  Grade4 -> yellowPalette
  Grade4_5 -> yellowPalette
  Grade5 -> redPalette

-- Local palettes using Tailwind colors
greenPalette :: ColorPalette
greenPalette = ColorPalette "text-green-800" "bg-green-100" "border-green-300"

yellowPalette :: ColorPalette
yellowPalette = ColorPalette "text-yellow-800" "bg-yellow-100" "border-yellow-300"

redPalette :: ColorPalette
redPalette = ColorPalette "text-red-800" "bg-red-100" "border-red-300"
