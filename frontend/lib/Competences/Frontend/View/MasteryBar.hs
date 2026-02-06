module Competences.Frontend.View.MasteryBar
  ( MasteryDisplayConfig (..)
  , masteryDisplay
  )
where

import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Color (bgClass')
import Competences.Frontend.View.Color.Mastery (masteryPalette)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Query.Mastery (MasteryStatus (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH

-- | Configuration for the mastery display component
data MasteryDisplayConfig = MasteryDisplayConfig
  { totalStudents :: !Int
  , stats :: !(Map MasteryStatus Int)
  , students :: !(Map MasteryStatus [User])
  }

-- | Get background class for a mastery status.
-- Uses mastery palette colors, with stone-300 fallback for NotTried.
masteryBgClass :: MasteryStatus -> Text
masteryBgClass status = fromMaybe "bg-stone-300" (bgClass' <$> masteryPalette status)

-- | Render mastery distribution as horizontal stacked bars with tooltips.
-- Always shows all 6 indicators (dimmed when count is 0) for consistent navigation.
masteryDisplay :: MasteryDisplayConfig -> M.View m action
masteryDisplay config =
  MH.div_
    [class_ "flex flex-col gap-1 mt-1"]
    [ -- Stacked horizontal bar (only segments with count > 0)
      MH.div_
        [class_ "flex h-3 rounded overflow-hidden bg-stone-100"]
        (map renderSegment segments)
    , -- Count labels below - always show all 6, with CSS tooltips
      MH.div_
        [class_ "flex gap-x-2 text-xs"]
        (map renderIndicator segments)
    ]
  where
    getCount status = Map.findWithDefault 0 status config.stats
    getStudents status = Map.findWithDefault [] status config.students

    segments =
      [ (StreakTwoAssessed, C.translate' C.LblMasteryStreakTwoAssessed)
      , (StreakTwoPlus, C.translate' C.LblMasteryStreakTwoPlus)
      , (OneSuccess, C.translate' C.LblMasteryOneSuccess)
      , (OnlySillyMistakes, C.translate' C.LblMasteryOnlySillyMistakes)
      , (MasteryNotYet, C.translate' C.LblMasteryNotYet)
      , (NotTried, C.translate' C.LblMasteryNotTried)
      ]

    percentage count =
      if config.totalStudents > 0
        then (fromIntegral count * 100.0 / fromIntegral config.totalStudents) :: Double
        else 0.0

    -- Render bar segment (only if count > 0, otherwise skip to keep bar compact)
    renderSegment (status, _label) =
      let count = getCount status
          pct = percentage count
          colorClass = masteryBgClass status
       in if count > 0
            then
              MH.div_
                [ class_ $ colorClass <> " h-full"
                , MC.style_ [("width", M.ms $ show pct <> "%")]
                ]
                []
            else M.text ""

    -- Render count indicator with CSS tooltip showing student names
    renderIndicator (status, label) =
      let count = getCount status
          studentList = getStudents status
          isZero = count == 0
          -- Dim both the color box and text when count is 0
          opacityClass = if isZero then " opacity-30" else ""
          textColorClass = if isZero then "text-stone-400" else "text-stone-600"
          colorClass = masteryBgClass status
          -- Build tooltip content: label on first line, student names on second
          studentNames = T.intercalate ", " $ map (.name) studentList
          tooltipContent = label <> "\n" <> M.ms studentNames
          -- Only show tooltip if there are students (no point showing empty tooltip)
          tip =
            if isZero
              then NoTooltip
              else RichTooltip (M.text tooltipContent)
       in withTooltip tip $
            MH.div_
              [class_ $ "flex items-center gap-0.5" <> opacityClass]
              [ -- Colored square
                MH.div_ [class_ $ "w-2 h-2 rounded-sm " <> colorClass] []
              , -- Count
                MH.span_ [class_ textColorClass] [M.text $ M.ms $ show count]
              ]
