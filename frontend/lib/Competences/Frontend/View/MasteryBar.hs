module Competences.Frontend.View.MasteryBar
  ( MasteryDisplayConfig (..)
  , masteryDisplay
  )
where

import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (groupHoverTooltip)
import Competences.Query.Mastery (MasteryStatus (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
      [ (StreakTwoAssessed, "bg-green-700", C.translate' C.LblMasteryStreakTwoAssessed)
      , (StreakTwoPlus, "bg-green-500", C.translate' C.LblMasteryStreakTwoPlus)
      , (OneSuccess, "bg-green-300", C.translate' C.LblMasteryOneSuccess)
      , (OnlySillyMistakes, "bg-yellow-400", C.translate' C.LblMasteryOnlySillyMistakes)
      , (MasteryNotYet, "bg-yellow-600", C.translate' C.LblMasteryNotYet)
      , (NotTried, "bg-stone-300", C.translate' C.LblMasteryNotTried)
      ]

    percentage count =
      if config.totalStudents > 0
        then (fromIntegral count * 100.0 / fromIntegral config.totalStudents) :: Double
        else 0.0

    -- Render bar segment (only if count > 0, otherwise skip to keep bar compact)
    renderSegment (status, colorClass, _label) =
      let count = getCount status
          pct = percentage count
       in if count > 0
            then
              MH.div_
                [ class_ $ colorClass <> " h-full"
                , MC.style_ [("width", M.ms $ show pct <> "%")]
                ]
                []
            else M.text ""

    -- Render count indicator with CSS tooltip showing student names
    renderIndicator (status, colorClass, label) =
      let count = getCount status
          studentList = getStudents status
          isZero = count == 0
          -- Dim both the color box and text when count is 0
          opacityClass = if isZero then " opacity-30" else ""
          textClass = if isZero then "text-stone-400" else "text-stone-600"
          -- Build tooltip content: label on first line, student names on second
          studentNames = T.intercalate ", " $ map (.name) studentList
          tooltipContent = label <> "\n" <> M.ms studentNames
          -- Only show tooltip if there are students (no point showing empty tooltip)
          tooltipView =
            if isZero
              then M.text ""
              else groupHoverTooltip tooltipContent
       in MH.div_
            [class_ $ "group relative flex items-center gap-0.5" <> opacityClass]
            [ tooltipView
            , -- Colored square
              MH.div_ [class_ $ "w-2 h-2 rounded-sm " <> colorClass] []
            , -- Count
              MH.span_ [class_ textClass] [M.text $ M.ms $ show count]
            ]
