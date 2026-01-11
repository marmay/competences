module Competences.Frontend.View.GradeBadge
  ( gradeBadgeView
  , gradeColorClasses
  , gradeShortLabel
  )
where

import Competences.Document.Grade (Grade (..))
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH

-- | Create a colored badge for a grade
-- Color coding: 1-3 green, 3-4/4/4-5 yellow, 5 red
gradeBadgeView :: Grade -> M.View m action
gradeBadgeView g =
  let (bgClass, textClass) = gradeColorClasses g
      shortLabel = gradeShortLabel g
   in MH.span_
        [ class_ $ "inline-flex items-center justify-center rounded-full px-2.5 py-1 text-sm font-medium " <> bgClass <> " " <> textClass
        ]
        [M.text (M.ms shortLabel)]

-- | Get background and text color classes for a grade
gradeColorClasses :: Grade -> (T.Text, T.Text)
gradeColorClasses g = case g of
  Grade1 -> ("bg-green-100", "text-green-700")
  Grade1_2 -> ("bg-green-100", "text-green-700")
  Grade2 -> ("bg-green-100", "text-green-700")
  Grade2_3 -> ("bg-green-100", "text-green-700")
  Grade3 -> ("bg-green-100", "text-green-700")
  Grade3_4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4_5 -> ("bg-yellow-100", "text-yellow-700")
  Grade5 -> ("bg-red-100", "text-red-700")

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
