-- | Stateless cell styling utilities for competence grid cells.
--
-- Provides background color mapping from 'Status',
-- a diagonal stripe pattern for cells without content,
-- and colored stripe patterns for mastery status display.
module Competences.Frontend.View.CellStyle
  ( statusBgClass
  , stripedStyle
  , masteryStripedStyle
  , masteryBadgeColors
  )
where

import Competences.Frontend.View.Color (bgClass, paletteStripedStyle, textClass)
import Competences.Frontend.View.Color.Mastery (masteryPalette)
import Competences.Frontend.View.StatusIcon (Status (..))
import Competences.Query.Mastery (MasteryStatus (..))
import Data.Text (Text)
import Miso qualified as M

-- | Background CSS class for a cell based on its visual status.
-- For cells without a level description, pass 'NoStatus' (yields @\"\"@).
statusBgClass :: Status -> Text
statusBgClass Achieved = "bg-green-200"
statusBgClass InProgress = "bg-yellow-200"
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
--
-- The stripe colors are defined via CSS variables in @input.css@:
-- @--mastery-{status}-stripe-light@ and @--mastery-{status}-stripe-dark@
masteryStripedStyle :: MasteryStatus -> [(M.MisoString, M.MisoString)]
masteryStripedStyle status = case masteryPalette status of
  Nothing -> []
  Just p -> paletteStripedStyle p

-- | Badge color classes for mastery status (bg, text).
-- Returns 'Nothing' for 'NotTried' (no badge shown).
--
-- Uses the mastery palette to derive CSS class names.
masteryBadgeColors :: MasteryStatus -> Maybe (Text, Text)
masteryBadgeColors status = case masteryPalette status of
  Nothing -> Nothing
  Just p -> Just (bgClass p, textClass p)
