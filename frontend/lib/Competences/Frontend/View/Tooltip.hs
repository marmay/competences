module Competences.Frontend.View.Tooltip
  ( withTooltip
  , withTooltipPosition
  , TooltipPosition (..)
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)

-- | Tooltip position variants
data TooltipPosition = TooltipTop | TooltipBottom | TooltipLeft | TooltipRight
  deriving (Eq, Show)

-- | Add a tooltip to an element (defaults to top position)
-- Uses Basecoat's data-tooltip attribute pattern (pure CSS, no JavaScript needed)
withTooltip :: MisoString -> M.View m a -> M.View m a
withTooltip = withTooltipPosition TooltipTop

-- | Add a tooltip to an element with specified position
-- Uses Basecoat's data-tooltip attribute pattern for positioning and styling
withTooltipPosition :: TooltipPosition -> MisoString -> M.View m a -> M.View m a
withTooltipPosition pos tooltipText element =
  M.span_
    [ class_ "relative inline-block"
    , M.textProp "data-tooltip" tooltipText
    , M.textProp "data-tooltip-position" (posToText pos)
    ]
    [element]
  where
    posToText TooltipTop = "top"
    posToText TooltipBottom = "bottom"
    posToText TooltipLeft = "left"
    posToText TooltipRight = "right"
