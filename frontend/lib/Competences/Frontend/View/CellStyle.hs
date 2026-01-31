-- | Stateless cell styling utilities for competence grid cells.
--
-- Provides background color mapping from 'Status',
-- a diagonal stripe pattern for cells without content,
-- and colored stripe patterns for mastery status display.
module Competences.Frontend.View.CellStyle
  ( statusBgClass
  , stripedStyle
  , masteryStripedStyle
  )
where

import Competences.Frontend.View.StatusIcon (Status (..))
import Competences.Query.Mastery (MasteryStatus (..))
import Data.Text (Text)
import Miso qualified as M

-- | Background CSS class for a cell based on its visual status.
-- For cells without a level description, pass 'NoStatus' (yields @\"\"@).
statusBgClass :: Status -> Text
statusBgClass Achieved = "bg-green-100"
statusBgClass InProgress = "bg-yellow-100"
statusBgClass Locked = "bg-stone-200"
statusBgClass NoStatus = ""

-- | Diagonal stripe pattern for cells that have no level description.
-- Apply as inline style on the @\<td\>@ element.
stripedStyle :: [(M.MisoString, M.MisoString)]
stripedStyle =
  [ ( "background"
    , "repeating-linear-gradient(135deg, rgb(245 245 244) 0px, rgb(245 245 244) 4px, rgb(231 229 228) 4px, rgb(231 229 228) 8px)"
    )
  ]

-- | Colored diagonal stripe pattern for mastery status in grid cells.
-- Uses graduated greens for positive states and yellows for not-yet states.
-- Returns empty list for 'NotTried' (no visual indicator).
masteryStripedStyle :: MasteryStatus -> [(M.MisoString, M.MisoString)]
masteryStripedStyle StreakTwoAssessed = coloredStripes "rgb(187 247 208)" "rgb(134 239 172)" -- green-200 / green-300
masteryStripedStyle StreakTwoPlus = coloredStripes "rgb(220 252 231)" "rgb(187 247 208)" -- green-100 / green-200
masteryStripedStyle OneSuccess = coloredStripes "rgb(240 253 244)" "rgb(220 252 231)" -- green-50 / green-100
masteryStripedStyle OnlySillyMistakes = coloredStripes "rgb(254 252 232)" "rgb(254 249 195)" -- yellow-50 / yellow-100
masteryStripedStyle MasteryNotYet = coloredStripes "rgb(254 249 195)" "rgb(254 240 138)" -- yellow-100 / yellow-200
masteryStripedStyle NotTried = []

-- | Build a diagonal stripe background with two alternating colors.
coloredStripes :: M.MisoString -> M.MisoString -> [(M.MisoString, M.MisoString)]
coloredStripes light dark =
  [ ( "background"
    , "repeating-linear-gradient(135deg, "
        <> light
        <> " 0px, "
        <> light
        <> " 4px, "
        <> dark
        <> " 4px, "
        <> dark
        <> " 8px)"
    )
  ]
