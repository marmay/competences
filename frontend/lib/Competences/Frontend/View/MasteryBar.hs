module Competences.Frontend.View.MasteryBar
  ( MasteryDisplayConfig (..)
  , masteryDisplay
  )
where

import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Color (bgClass')
import Competences.Frontend.View.Color.Mastery (masteryPalette)
import Competences.Frontend.View.StackedBar (BarSegment (..), StackedBarConfig (..), stackedBar)
import Competences.Frontend.View.Tooltip (Tooltip (..))
import Competences.Query.Mastery (MasteryStatus (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.String (MisoString)

-- | Configuration for the mastery display component
data MasteryDisplayConfig = MasteryDisplayConfig
  { totalStudents :: !Int
  , stats :: !(Map MasteryStatus Int)
  , students :: !(Map MasteryStatus [User])
  }

-- | Get background class for a mastery status.
-- Uses mastery palette colors, with stone-300 fallback for NotTried.
masteryBgClass :: MasteryStatus -> Text
masteryBgClass status = maybe "bg-stone-300" bgClass' (masteryPalette status)

-- | Render mastery distribution as horizontal stacked bars with tooltips.
-- Always shows all 6 indicators (dimmed when count is 0) for consistent navigation.
masteryDisplay :: MasteryDisplayConfig -> M.View m action
masteryDisplay config =
  stackedBar $
    StackedBarConfig
      { total = config.totalStudents
      , segments = map toSegment statusLabels
      }
  where
    getCount status = Map.findWithDefault 0 status config.stats
    getStudents status = Map.findWithDefault [] status config.students

    statusLabels :: [(MasteryStatus, MisoString)]
    statusLabels =
      [ (StreakTwoAssessed, C.translate' C.LblMasteryStreakTwoAssessed)
      , (StreakTwoPlus, C.translate' C.LblMasteryStreakTwoPlus)
      , (OneSuccess, C.translate' C.LblMasteryOneSuccess)
      , (OnlySillyMistakes, C.translate' C.LblMasteryOnlySillyMistakes)
      , (MasteryNotYet, C.translate' C.LblMasteryNotYet)
      , (NotTried, C.translate' C.LblMasteryNotTried)
      ]

    toSegment :: (MasteryStatus, MisoString) -> BarSegment m action
    toSegment (status, lbl) =
      let count = getCount status
          studentList = getStudents status
          studentNames = T.intercalate ", " $ map (.name) studentList
          tooltipContent = lbl <> "\n" <> M.ms studentNames
       in BarSegment
            { count = count
            , colorClass = masteryBgClass status
            , tooltip =
                if count == 0
                  then NoTooltip
                  else RichTooltip (M.text tooltipContent)
            }
