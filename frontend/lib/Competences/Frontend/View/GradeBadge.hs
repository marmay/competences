module Competences.Frontend.View.GradeBadge
  ( gradeBadgeView
  , gradePalette
  , gradeShortLabel
  )
where

import Competences.Document.Grade (Grade (..))
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Color qualified as Color
import Data.Text qualified as T
import Miso qualified as M

-- | Create a colored badge for a grade
-- Color coding: 1-3 green, 3-4/4/4-5 yellow, 5 red
gradeBadgeView :: Grade -> M.View m action
gradeBadgeView g =
  Badge.customBadge (gradePalette g) (M.ms (gradeShortLabel g) :: M.MisoString)

-- | Get color palette for a grade
gradePalette :: Grade -> Color.ColorPalette
gradePalette g = case g of
  Grade1 -> Color.green
  Grade1_2 -> Color.green
  Grade2 -> Color.green
  Grade2_3 -> Color.green
  Grade3 -> Color.green
  Grade3_4 -> Color.yellow
  Grade4 -> Color.yellow
  Grade4_5 -> Color.yellow
  Grade5 -> Color.red

-- | Short label for grade (just the number part)
gradeShortLabel :: Grade -> T.Text
gradeShortLabel g = case g of
  Grade1 -> "1"
  Grade1_2 -> "1-2"
  Grade2 -> "2"
  Grade2_3 -> "2-3"
  Grade3 -> "3"
  Grade3_4 -> "3-4"
  Grade4 -> "4"
  Grade4_5 -> "4-5"
  Grade5 -> "5"
