-- | Stateless cell styling utilities for competence grid cells.
--
-- Provides background color mapping from 'Status' and
-- a diagonal stripe pattern for cells without content.
module Competences.Frontend.View.CellStyle
  ( statusBgClass
  , stripedStyle
  )
where

import Competences.Frontend.View.StatusIcon (Status (..))
import Miso qualified as M

-- | Background CSS class for a cell based on its visual status.
-- For cells without a level description, pass 'NoStatus' (yields @\"\"@).
statusBgClass :: Status -> M.MisoString
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
