module Competences.Frontend.View.Tooltip
  ( -- * Tooltip type
    Tooltip (..)

    -- * Foundation: attrs + children helpers (for component integration)
  , tooltipAttrs
  , tooltipChild

    -- * Convenience: View modifier (for standalone use)
  , withTooltip
  , withTooltip'
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso (View (..))
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)

-- | Tooltip attached to a UI element
data Tooltip model action
  = -- | No tooltip
    NoTooltip
  | -- | Simple tooltip via HTML title attribute
    PlainTooltip !MisoString
  | -- | Rich tooltip rendered as a CSS group-hover positioned popup
    RichTooltip !(View model action)

-- | Attributes to add to the host element for tooltip support
tooltipAttrs :: Tooltip model action -> [M.Attribute action]
tooltipAttrs NoTooltip = []
tooltipAttrs (PlainTooltip t) = [MP.title_ t]
tooltipAttrs (RichTooltip _) = [class_ "group relative"]

-- | Child elements to append for tooltip rendering
tooltipChild :: Tooltip model action -> [View model action]
tooltipChild NoTooltip = []
tooltipChild (PlainTooltip _) = []
tooltipChild (RichTooltip content) =
  [ M.div_
      [ class_
          "absolute bottom-full left-0 mb-1 px-2 py-1 \
          \bg-primary text-primary-foreground text-xs rounded-md \
          \whitespace-pre-line min-w-48 max-w-xs \
          \opacity-0 group-hover:opacity-100 \
          \pointer-events-none transition-opacity z-50"
      ]
      [content]
  ]

-- | Apply a tooltip to a View element
--
-- Pattern matches on Miso's View constructors:
--
--   * 'VNode': inject attrs and append children directly (no extra wrapper)
--   * 'VText': wrap in @span@ (text nodes can't have attrs/children)
--   * 'VComp': wrap in @span@ (components don't expose children)
withTooltip :: Tooltip model action -> View model action -> View model action
withTooltip NoTooltip v = v
withTooltip tip (VNode ns tag attrs children) =
  VNode ns tag (attrs <> tooltipAttrs tip) (children <> tooltipChild tip)
withTooltip tip v =
  -- VText and VComp: wrap in a span
  M.span_
    (tooltipAttrs tip)
    (v : tooltipChild tip)

withTooltip' :: Maybe (Tooltip model action) -> View model action -> View model action
withTooltip' Nothing = id
withTooltip' (Just tip) = withTooltip tip
