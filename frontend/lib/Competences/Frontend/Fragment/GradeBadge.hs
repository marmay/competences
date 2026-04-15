module Competences.Frontend.Fragment.GradeBadge
  ( gradeBadgeView
  , gradePalette
  , gradeShortLabel
  )
where

import Competences.Document.Grade (Grade (..))
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Color.Grade (gradePalette)
import Data.Text qualified as T
import Miso qualified as M

-- | Create a colored badge for a grade
-- Color coding: 1-3 green, 3-4/4/4-5 yellow, 5 red
gradeBadgeView :: Grade -> M.View m action
gradeBadgeView g = Badge.badge (gradePalette g) (Badge.badgeText $ M.ms $ gradeShortLabel g)

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
