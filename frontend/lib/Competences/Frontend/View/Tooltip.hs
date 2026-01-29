module Competences.Frontend.View.Tooltip
  ( withTooltip
  , withTooltipPosition
  , TooltipPosition (..)
  , groupHoverTooltip
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
-- Note: Basecoat uses data-side for positioning (not data-tooltip-position)
withTooltipPosition :: TooltipPosition -> MisoString -> M.View m a -> M.View m a
withTooltipPosition pos tooltipText element =
  M.span_
    [ class_ "relative inline-block"
    , M.textProp "data-tooltip" tooltipText
    , M.textProp "data-side" (posToText pos)
    ]
    [element]
  where
    posToText TooltipTop = "top"
    posToText TooltipBottom = "bottom"
    posToText TooltipLeft = "left"
    posToText TooltipRight = "right"

-- | Render a tooltip that appears on group hover (pure CSS, no JavaScript).
-- The parent element must have @"group relative"@ classes.
-- Uses Basecoat styling (bg-primary, text-primary-foreground).
groupHoverTooltip :: MisoString -> M.View m a
groupHoverTooltip tooltipText =
  M.span_
    [ class_
        "absolute bottom-full left-0 mb-1 px-2 py-1 \
        \bg-primary text-primary-foreground text-xs rounded-md \
        \whitespace-pre-line min-w-48 max-w-xs \
        \opacity-0 group-hover:opacity-100 \
        \pointer-events-none transition-opacity z-50"
    ]
    [M.text tooltipText]
