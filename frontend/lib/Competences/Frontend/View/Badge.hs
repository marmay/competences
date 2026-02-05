{- |
Module: Competences.Frontend.View.Badge
Description: Basecoat badge components with builder-pattern API

Provides badge components following Basecoat design patterns.
Two render paths: semantic variants (via Basecoat CSS classes) and
custom palettes (via 'ColorPalette' from "View.Color").
-}
module Competences.Frontend.View.Badge
  ( -- * Badge variants
    BadgeVariant (..)

    -- * Badge contents
  , BadgeContents (..)
  , ToBadgeContents (..)

    -- * Rendering
  , render
  , primary
  , secondary
  , destructive
  , outline
  , customBadge
  , badgeCustomView

    -- * Interactive badges
  , interactive
  , customInteractive
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Color (ColorPalette (..))
import Competences.Frontend.View.Icon (Icon, icon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
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
-- RENDERING
-- ============================================================================

-- | Map a badge variant to its Basecoat CSS class
variantClass :: BadgeVariant -> Text
variantClass Primary = "badge"
variantClass Secondary = "badge-secondary"
variantClass Destructive = "badge-destructive"
variantClass Outline = "badge-outline"

-- | Render badge contents as child views
renderContents :: BadgeContents -> [M.View model action]
renderContents (TextOnly t) = [M.text t]
renderContents (IconOnly i) = [icon [] i]
renderContents (IconText i t) = [icon [] i, M.text t]

-- | Render a badge with a Basecoat semantic variant
render :: (ToBadgeContents c) => BadgeVariant -> c -> M.View model action
render variant contents =
  M.span_
    [class_ $ variantClass variant]
    (renderContents (toBadgeContents contents))

-- | Render a badge with a custom color palette
customBadge :: (ToBadgeContents c) => ColorPalette -> c -> M.View model action
customBadge palette contents =
  M.span_
    [ class_ $
        "inline-flex items-center justify-center gap-1 rounded-full border \
        \px-2 py-0.5 text-xs font-medium [&>svg]:size-3 "
          <> palette.background
          <> " "
          <> palette.foreground
          <> " "
          <> palette.border
    ]
    (renderContents (toBadgeContents contents))

-- | Render a badge with arbitrary View content (escape hatch)
badgeCustomView :: BadgeVariant -> M.View model action -> M.View model action
badgeCustomView variant content =
  M.span_
    [class_ $ variantClass variant]
    [content]

-- | Convenience constructors
primary, secondary, destructive, outline
  :: (ToBadgeContents c) => c -> M.View model action
primary = render Primary
secondary = render Secondary
destructive = render Destructive
outline = render Outline

-- ============================================================================
-- INTERACTIVE BADGES
-- ============================================================================

-- | Render an interactive badge with optional tooltip and action icon
--
-- The action icon (e.g. a cancel icon for delete) appears on hover.
-- Tooltips use the 'Tooltip' type from "View.Tooltip".
interactive
  :: (ToBadgeContents c)
  => BadgeVariant
  -> Tooltip model action
  -> Maybe (Icon, action)
  -> c
  -> M.View model action
interactive variant tip mAction contents =
  withTooltip tip $
    M.span_
      [ class_ $
          variantClass variant
            <> " group"
            <> if hasAction then " pr-1" else ""
      ]
      ( renderContents (toBadgeContents contents)
          <> actionButton mAction
      )
  where
    hasAction = case mAction of Just _ -> True; Nothing -> False

-- | Render an interactive badge with a custom color palette
customInteractive
  :: (ToBadgeContents c)
  => ColorPalette
  -> Tooltip model action
  -> Maybe (Icon, action)
  -> c
  -> M.View model action
customInteractive palette tip mAction contents =
  withTooltip tip $
    M.span_
      [ class_ $
          "inline-flex items-center justify-center gap-1 rounded-full border \
          \px-2 py-0.5 text-xs font-medium [&>svg]:size-3 group "
            <> palette.background
            <> " "
            <> palette.foreground
            <> " "
            <> palette.border
            <> if hasAction then " pr-1" else ""
      ]
      ( renderContents (toBadgeContents contents)
          <> actionButton mAction
      )
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
