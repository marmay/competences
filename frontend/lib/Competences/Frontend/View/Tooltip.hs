module Competences.Frontend.View.Tooltip
  ( withTooltip
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import Miso qualified as M
import Miso.Html qualified as M

withTooltip :: M.View m a -> M.View m a -> M.View m a
withTooltip v tooltip =
  M.div_ [T.tailwind [T.Relative]] [v, M.div_ [T.tailwind [T.Absolute, T.BottomFull, T.M4, T.TooltipBox]] [tooltip]]
