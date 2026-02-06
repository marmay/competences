-- | Grade color palette.
--
-- Maps 'Grade' values to semantic color palettes. Uses a traffic-light
-- pattern based on grade quality:
--
-- * Grades 1-3: green (good)
-- * Grades 3-4, 4, 4-5: yellow (warning)
-- * Grade 5: red (danger)
--
-- Note: These palettes use standard Tailwind color classes rather than
-- custom CSS variables, unlike the ability palettes. This is intentional
-- as grades don't need striped background support.
module Competences.Frontend.View.Color.Grade
  ( gradePalette
  , GradePalette (..)
  )
where

import Competences.Document.Grade (Grade (..))
import Data.Text (Text)

-- | A simple palette for grade colors.
-- Unlike 'PaletteName', this stores explicit class names since grades
-- use standard Tailwind colors without stripe variants.
data GradePalette = GradePalette
  { foreground :: !Text -- ^ text color class (e.g., "text-green-800")
  , background :: !Text -- ^ background class (e.g., "bg-green-100")
  , border :: !Text -- ^ border color class (e.g., "border-green-300")
  }

-- | Get the color palette for a grade.
gradePalette :: Grade -> GradePalette
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
greenPalette :: GradePalette
greenPalette = GradePalette "text-green-800" "bg-green-100" "border-green-300"

yellowPalette :: GradePalette
yellowPalette = GradePalette "text-yellow-800" "bg-yellow-100" "border-yellow-300"

redPalette :: GradePalette
redPalette = GradePalette "text-red-800" "bg-red-100" "border-red-300"
