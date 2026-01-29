module Competences.Frontend.View.DateDisplay
  ( formatDateRange
  , shortDate
  )
where

import Competences.Frontend.Common qualified as C
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Miso.String (MisoString, ms)

-- | Format a date range for display.
-- Handles all combinations of optional from/to dates.
formatDateRange :: Maybe Day -> Maybe Day -> MisoString
formatDateRange Nothing Nothing = ""
formatDateRange (Just from) Nothing = C.translate' C.LblMesoPlanDateFrom <> ": " <> C.formatDay from
formatDateRange Nothing (Just to) = C.translate' C.LblMesoPlanDateTo <> ": " <> C.formatDay to
formatDateRange (Just from) (Just to) = C.formatDay from <> " \x2013 " <> C.formatDay to

-- | Format a date in short DD.MM form (without year), for compact display.
shortDate :: Day -> MisoString
shortDate d = ms $ formatTime defaultTimeLocale "%d.%m" d
