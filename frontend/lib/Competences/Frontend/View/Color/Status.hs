-- | Status color palettes for achievement/progress indicators.
--
-- Maps semantic status states to CSS color palettes.
module Competences.Frontend.View.Color.Status
  ( Status (..)
  , statusPalette
  )
where

import Competences.Frontend.View.Color (PaletteName (..))

-- | Semantic status for achievement/progress indicators.
data Status
  = Ok
  | Pending
  | Error
  deriving (Eq, Show)

-- | Map a status to its CSS color palette.
--
-- @statusPalette Ok@ = @PaletteName "status-ok"@ (green)
-- @statusPalette Pending@ = @PaletteName "status-pending"@ (yellow)
-- @statusPalette Error@ = @PaletteName "status-error"@ (red)
statusPalette :: Status -> PaletteName
statusPalette Ok = PaletteName "status-ok"
statusPalette Pending = PaletteName "status-pending"
statusPalette Error = PaletteName "status-error"
