-- | Status dot indicator view.
--
-- A small colored dot for indicating status states.
-- Uses the Status palette from "Color.Status".
module Competences.Frontend.View.StatusDot
  ( statusDot
  , statusDotAnimated
  )
where

import Competences.Frontend.View.Color (PaletteColor (..), bgClass)
import Competences.Frontend.View.Color.Status (Status, statusPalette)
import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M

-- | Render a status dot with the given status color.
--
-- A small (w-2.5 h-2.5) colored circle indicator.
statusDot :: Status -> M.View model action
statusDot status =
  M.span_
    [class_ $ "w-2.5 h-2.5 rounded-full " <> bgClass Accent (statusPalette status)]
    []

-- | Render an animated status dot (pulses).
--
-- Useful for indicating active/warning states like disconnection.
statusDotAnimated :: Status -> M.View model action
statusDotAnimated status =
  M.span_
    [class_ $ "w-2.5 h-2.5 rounded-full animate-pulse " <> bgClass Accent (statusPalette status)]
    []
