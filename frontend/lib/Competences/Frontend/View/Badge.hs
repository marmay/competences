{- |
Module: Competences.Frontend.View.Badge
Description: Basecoat badge components with builder-pattern API

Provides badge components following Basecoat design patterns.
Two color sources (orthogonal to interactivity):

  * Basecoat variants: 'Primary', 'Secondary', 'Destructive', 'Outline'
  * Semantic palettes: 'PaletteName' from Color module

Two interactivity modes:

  * Static: simple badge display
  * Interactive: optional action button on hover

For tooltips, use 'withTooltip' from the Tooltip module to wrap badges.
-}
module Competences.Frontend.View.Badge
  ( -- * Badge variants
    BadgeVariant (..)

    -- * Badge contents
  , BadgeContents (..)
  , ToBadgeContents (..)

    -- * Static badges
  , render
  , badge
  , primary
  , secondary
  , destructive
  , outline
  , badgeCustomView

    -- * Interactive badges
  , interactive
  , paletteInteractive
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Color (PaletteName, bgClass', borderClass', textClass')
import Competences.Frontend.View.Icon (Icon, icon)
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)

-- | Badge variant following Basecoat design system
data BadgeVariant = Primary | Secondary | Destructive | Outline
  deriving (Eq, Show)

-- | Badge contents
data BadgeContents
  = TextOnly !MisoString
  | IconOnly !Icon
  | IconText !Icon !MisoString
  deriving (Eq, Show)

class ToBadgeContents a where
  toBadgeContents :: a -> BadgeContents

instance ToBadgeContents BadgeContents where
  toBadgeContents = id

instance ToBadgeContents MisoString where
  toBadgeContents = TextOnly

instance ToBadgeContents Label where
  toBadgeContents = TextOnly . translate'

instance ToBadgeContents Icon where
  toBadgeContents = IconOnly

instance ToBadgeContents (Icon, MisoString) where
  toBadgeContents (i, t) = IconText i t

instance ToBadgeContents (Icon, Label) where
  toBadgeContents (i, l) = IconText i (translate' l)

-- ============================================================================
-- INTERNAL: Shared rendering
-- ============================================================================

-- | Render badge contents as child views
renderContents :: BadgeContents -> [M.View model action]
renderContents (TextOnly t) = [M.text t]
renderContents (IconOnly i) = [icon [] i]
renderContents (IconText i t) = [icon [] i, M.text t]

-- | Internal: render a static badge with given CSS classes
renderBadge :: Text -> BadgeContents -> M.View model action
renderBadge classes contents =
  M.span_ [class_ classes] (renderContents contents)

-- | Internal: render an interactive badge with given CSS classes
renderInteractiveBadge
  :: Text
  -> Maybe (Icon, action)
  -> BadgeContents
  -> M.View model action
renderInteractiveBadge baseClasses mAction contents =
  M.span_
    [class_ $ baseClasses <> " group" <> if hasAction then " pr-1" else ""]
    (renderContents contents <> actionButton mAction)
  where
    hasAction = case mAction of Just _ -> True; Nothing -> False

-- | Render the optional action button that appears on hover
actionButton :: Maybe (Icon, action) -> [M.View model action]
actionButton Nothing = []
actionButton (Just (icn, action)) =
  [ M.button_
      [ class_ actionButtonClasses
      , MP.type_ "button"
      , M.intProp "tabindex" (-1)
      , M.onClick action
      ]
      [icon [] icn]
  ]

actionButtonClasses :: Text
actionButtonClasses =
  "opacity-0 group-hover:opacity-100 pointer-events-none group-hover:pointer-events-auto \
  \-mr-1 ml-0.5 h-4 w-4 rounded-full \
  \flex items-center justify-center text-secondary-foreground/70 \
  \hover:bg-destructive hover:text-destructive-foreground \
  \transition-opacity focus:opacity-100 [&>svg]:size-3"

-- ============================================================================
-- COLOR SOURCES: Variant vs Palette
-- ============================================================================

-- | Map a Basecoat variant to its CSS class
variantClass :: BadgeVariant -> Text
variantClass Primary = "badge"
variantClass Secondary = "badge-secondary"
variantClass Destructive = "badge-destructive"
variantClass Outline = "badge-outline"

-- | Basecoat badge class with palette color overrides
paletteClasses :: PaletteName -> Text
paletteClasses p = "badge " <> bgClass' p <> " " <> textClass' p <> " " <> borderClass' p

-- ============================================================================
-- PUBLIC API: Static badges
-- ============================================================================

-- | Render a badge with a Basecoat semantic variant
render :: (ToBadgeContents c) => BadgeVariant -> c -> M.View model action
render variant = renderBadge (variantClass variant) . toBadgeContents

-- | Render a badge with a semantic color palette
badge :: (ToBadgeContents c) => PaletteName -> c -> M.View model action
badge palette = renderBadge (paletteClasses palette) . toBadgeContents

-- | Convenience constructors for Basecoat variants
primary, secondary, destructive, outline
  :: (ToBadgeContents c) => c -> M.View model action
primary = render Primary
secondary = render Secondary
destructive = render Destructive
outline = render Outline

-- | Render a badge with arbitrary View content (escape hatch)
badgeCustomView :: BadgeVariant -> M.View model action -> M.View model action
badgeCustomView variant content =
  M.span_ [class_ $ variantClass variant] [content]

-- ============================================================================
-- PUBLIC API: Interactive badges
-- ============================================================================

-- | Render an interactive badge with a Basecoat variant
--
-- The action icon (e.g. a cancel icon for delete) appears on hover.
-- For tooltips, wrap the result with 'withTooltip' from the Tooltip module.
interactive
  :: (ToBadgeContents c)
  => BadgeVariant
  -> Maybe (Icon, action)
  -> c
  -> M.View model action
interactive variant mAction =
  renderInteractiveBadge (variantClass variant) mAction . toBadgeContents

-- | Render an interactive badge with a semantic color palette
--
-- The action icon (e.g. a cancel icon for delete) appears on hover.
-- For tooltips, wrap the result with 'withTooltip' from the Tooltip module.
paletteInteractive
  :: (ToBadgeContents c)
  => PaletteName
  -> Maybe (Icon, action)
  -> c
  -> M.View model action
paletteInteractive palette mAction =
  renderInteractiveBadge (paletteClasses palette) mAction . toBadgeContents
