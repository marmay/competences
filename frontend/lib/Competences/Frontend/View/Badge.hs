{- |
Module: Competences.Frontend.View.Badge
Description: Basecoat badge components

Provides badge components following Basecoat design patterns.
Two color sources (orthogonal to interactivity):

  * Basecoat variants: 'Primary', 'Secondary', 'Destructive', 'Outline'
  * Semantic palettes: 'PaletteName' from Color module

Two interactivity modes:

  * Static: simple badge display
  * Interactive: optional action button on hover

For tooltips, use 'withTooltip' from the Tooltip module to wrap badges.

== Usage

@
-- Text badge with variant
Badge.primary (Badge.badgeText "New")

-- Text badge with palette
Badge.badge myPalette (Badge.badgeText "Status")

-- Icon + text badge
Badge.secondary (Badge.badgeIconText Icon.IcnTask "Task 1")

-- Custom content badge
Badge.badge palette $
  MH.div_ [class_ "flex gap-1"] [icon1, icon2]
@
-}
module Competences.Frontend.View.Badge
  ( -- * Badge variants
    BadgeVariant (..)

    -- * Static badges
  , badge
  , variant
  , primary
  , secondary
  , destructive
  , outline

    -- * Interactive badges
  , interactive
  , interactiveMulti
  , withActions
  , paletteInteractive

    -- * Content helpers
  , badgeText
  , badgeLabel
  , badgeIcon
  , badgeIconText
  , badgeIconLabel
  )
where

import Competences.Frontend.Common.Translate (Label, translate')
import Competences.Frontend.View.Color (PaletteName, bgClass', borderClass', textClass')
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)

-- | Badge variant following Basecoat design system
data BadgeVariant = Primary | Secondary | Destructive | Outline
  deriving (Eq, Show)

-- ============================================================================
-- INTERNAL: Shared rendering
-- ============================================================================

-- | Internal: render a static badge with given CSS classes
renderBadge :: Text -> M.View model action -> M.View model action
renderBadge classes content =
  M.span_ [class_ classes] [content]

-- | Internal: render an interactive badge with given CSS classes
renderInteractiveBadge
  :: Text
  -> Maybe (Icon.Icon, action)
  -> M.View model action
  -> M.View model action
renderInteractiveBadge baseClasses mAction content =
  M.span_
    [class_ $ baseClasses <> " group" <> if hasAction then " pr-1" else ""]
    ([content] <> actionButton mAction)
  where
    hasAction = case mAction of Just _ -> True; Nothing -> False

-- | Internal: render a badge with multiple hover-revealed action buttons.
renderInteractiveMultiBadge
  :: Text
  -> [(Icon.Icon, action)]
  -> M.View model action
  -> M.View model action
renderInteractiveMultiBadge baseClasses actions content =
  M.span_
    [class_ $ baseClasses <> " group" <> if null actions then "" else " pr-0.5"]
    ([content] <> map (renderActionBtn hoverActionBtnClasses) actions)

-- | Internal: render a badge with multiple always-visible action buttons.
renderWithActionsBadge
  :: Text
  -> [(Icon.Icon, action)]
  -> M.View model action
  -> M.View model action
renderWithActionsBadge baseClasses actions content =
  M.span_
    [class_ $ baseClasses <> if null actions then "" else " pr-0.5"]
    ([content] <> map (renderActionBtn visibleActionBtnClasses) actions)

-- | Render the optional action button that appears on hover
actionButton :: Maybe (Icon.Icon, action) -> [M.View model action]
actionButton Nothing = []
actionButton (Just (icn, action)) =
  [renderActionBtn hoverActionBtnClasses (icn, action)]

-- | Render a single action button with given CSS classes.
renderActionBtn :: Text -> (Icon.Icon, action) -> M.View model action
renderActionBtn classes (icn, action) =
  M.button_
    [ class_ classes
    , MP.type_ "button"
    , M.intProp "tabindex" (-1)
    , M.onClick action
    ]
    [Icon.icon [] icn]

-- | Shared button sizing/layout classes.
actionBtnBase :: Text
actionBtnBase =
  "-mr-0.5 ml-0.5 h-4 w-4 rounded-full \
  \flex items-center justify-center text-secondary-foreground/70 \
  \hover:bg-destructive hover:text-destructive-foreground \
  \transition-opacity focus:opacity-100 [&>svg]:size-3"

-- | Hover-revealed action button classes.
hoverActionBtnClasses :: Text
hoverActionBtnClasses =
  "opacity-0 group-hover:opacity-100 pointer-events-none group-hover:pointer-events-auto " <> actionBtnBase

-- | Always-visible action button classes.
visibleActionBtnClasses :: Text
visibleActionBtnClasses = actionBtnBase

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

-- | Render a badge with a semantic color palette.
badge :: PaletteName -> M.View model action -> M.View model action
badge palette = renderBadge (paletteClasses palette)

-- | Render a badge with a Basecoat semantic variant.
variant :: BadgeVariant -> M.View model action -> M.View model action
variant v = renderBadge (variantClass v)

-- | Convenience constructors for Basecoat variants.
primary, secondary, destructive, outline
  :: M.View model action -> M.View model action
primary = variant Primary
secondary = variant Secondary
destructive = variant Destructive
outline = variant Outline

-- ============================================================================
-- PUBLIC API: Interactive badges
-- ============================================================================

-- | Render an interactive badge with a Basecoat variant.
--
-- The action icon (e.g. a cancel icon for delete) appears on hover.
-- For tooltips, wrap the result with 'withTooltip' from the Tooltip module.
interactive
  :: BadgeVariant
  -> Maybe (Icon.Icon, action)
  -> M.View model action
  -> M.View model action
interactive v mAction =
  renderInteractiveBadge (variantClass v) mAction

-- | Render an interactive badge with multiple hover-revealed action buttons.
--
-- All action icons appear on hover in a compact row inside the badge.
interactiveMulti
  :: BadgeVariant
  -> [(Icon.Icon, action)]
  -> M.View model action
  -> M.View model action
interactiveMulti v = renderInteractiveMultiBadge (variantClass v)

-- | Render a badge with always-visible action buttons.
--
-- Like 'interactiveMulti' but buttons are always shown (for active reorder states).
withActions
  :: BadgeVariant
  -> [(Icon.Icon, action)]
  -> M.View model action
  -> M.View model action
withActions v = renderWithActionsBadge (variantClass v)

-- | Render an interactive badge with a semantic color palette.
--
-- The action icon (e.g. a cancel icon for delete) appears on hover.
-- For tooltips, wrap the result with 'withTooltip' from the Tooltip module.
paletteInteractive
  :: PaletteName
  -> Maybe (Icon.Icon, action)
  -> M.View model action
  -> M.View model action
paletteInteractive palette mAction =
  renderInteractiveBadge (paletteClasses palette) mAction

-- ============================================================================
-- Content Helpers
-- ============================================================================

-- | Badge content with just text.
badgeText :: MisoString -> M.View model action
badgeText t = M.text t

-- | Badge content with a translated label.
badgeLabel :: Label -> M.View model action
badgeLabel = M.text . translate'

-- | Badge content with just an icon.
badgeIcon :: Icon.Icon -> M.View model action
badgeIcon = Icon.icon []

-- | Badge content with icon and text.
badgeIconText :: Icon.Icon -> MisoString -> M.View model action
badgeIconText i t =
  M.span_
    [class_ "inline-flex items-center gap-1"]
    [Icon.icon [] i, M.text t]

-- | Badge content with icon and translated label.
badgeIconLabel :: Icon.Icon -> Label -> M.View model action
badgeIconLabel i l = badgeIconText i (translate' l)
