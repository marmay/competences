{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Competences.Frontend.View.Badge
Description: Basecoat-inspired badge/tag components

This module provides badge and tag components following Basecoat design patterns.
-}
module Competences.Frontend.View.Badge
  ( -- * Badge variants
    BadgeVariant (..)
  , badge
  , badgePrimary
  , badgeSecondary
  , badgeDestructive
  , badgeOutline

    -- * Interactive badge (with tooltip and delete)
  , InteractiveBadgeConfig (..)
  , interactiveBadge
  )
where

import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)

-- | Badge variant following Basecoat design system
data BadgeVariant
  = BadgePrimary     -- ^ Primary badge (sky-600 background)
  | BadgeSecondary   -- ^ Secondary badge (stone-200 background)
  | BadgeDestructive -- ^ Destructive badge (red-600 background)
  | BadgeOutline     -- ^ Outline badge (transparent with border)
  deriving (Eq, Show)

-- | Create a badge with given variant and text
badge :: BadgeVariant -> MisoString -> M.View model action
badge variant text =
  M.span_
    [ class_ $ baseClasses <> " " <> variantClasses variant
    ]
    [M.text text]
  where
    -- Base classes from Basecoat (using semantic CSS variables)
    baseClasses = "inline-flex items-center justify-center rounded-full border px-2 py-0.5 text-xs font-medium focus-visible:border-ring focus-visible:ring-[3px]"

    -- Variant classes (using semantic CSS variables)
    variantClasses BadgePrimary = "bg-primary text-primary-foreground border-primary"
    variantClasses BadgeSecondary = "bg-secondary text-secondary-foreground border-secondary"
    variantClasses BadgeDestructive = "bg-destructive text-destructive-foreground border-destructive"
    variantClasses BadgeOutline = "text-foreground border-input hover:bg-accent"

-- | Convenient badge constructors
badgePrimary, badgeSecondary, badgeDestructive, badgeOutline
  :: MisoString -> M.View model action
badgePrimary = badge BadgePrimary
badgeSecondary = badge BadgeSecondary
badgeDestructive = badge BadgeDestructive
badgeOutline = badge BadgeOutline

-- ============================================================================
-- INTERACTIVE BADGE (with tooltip and delete button)
-- ============================================================================

-- | Configuration for an interactive badge
--
-- This badge variant supports:
-- - Tooltip on hover (via data-tooltip attribute)
-- - Optional delete button (X) that appears on hover
data InteractiveBadgeConfig action = InteractiveBadgeConfig
  { text :: !MisoString
  -- ^ Badge label text (e.g., "1.2.3")
  , tooltip :: !(Maybe MisoString)
  -- ^ Optional tooltip content
  , onDelete :: !(Maybe action)
  -- ^ Optional delete action (shows X button if present)
  }
  deriving (Generic)

-- | Render an interactive badge with tooltip and optional delete button
--
-- Behavior:
-- - Tooltip appears on hover (using CSS group-hover, not data-tooltip due to
--   OKLCH color variable incompatibility with Basecoat's CSS-only tooltips)
-- - X button appears on hover if onDelete is provided
-- - Clicking X button triggers the delete action
interactiveBadge :: InteractiveBadgeConfig action -> M.View model action
interactiveBadge config =
  M.span_
    [class_ badgeClasses]
    [ -- Tooltip (shown on hover via CSS)
      case config.tooltip of
        Nothing -> M.text ""
        Just tip -> tooltipView tip
    , -- Badge text
      M.span_ [] [M.text config.text]
    , -- Delete button
      case config.onDelete of
        Nothing -> M.text ""
        Just deleteAction -> deleteButton deleteAction
    ]
  where
    -- Badge classes with group for hover-based visibility
    badgeClasses =
      "group relative inline-flex items-center gap-1 rounded-full border px-2 py-0.5 \
      \text-xs font-medium bg-secondary text-secondary-foreground border-secondary"

    -- CSS-based tooltip using group-hover (avoids Basecoat color variable issues)
    -- Uses whitespace-pre-line to respect newlines in tooltip text
    tooltipView tip =
      M.span_
        [ class_
            "absolute bottom-full left-0 mb-2 px-3 py-1.5 \
            \bg-primary text-primary-foreground text-xs rounded-md \
            \whitespace-pre-line min-w-64 max-w-lg text-left \
            \opacity-0 group-hover:opacity-100 \
            \pointer-events-none transition-opacity z-50"
        ]
        [M.text tip]

    -- Delete button that appears on hover
    -- Uses tabindex=-1 to prevent stealing focus from the container
    deleteButton action =
      M.button_
        [ class_ deleteButtonClasses
        , MP.type_ "button"
        , M.intProp "tabindex" (-1)
        , M.onClick action
        ]
        [ M.text "×"
        ]

    deleteButtonClasses =
      "opacity-0 group-hover:opacity-100 pointer-events-none group-hover:pointer-events-auto \
      \-mr-1 ml-0.5 h-4 w-4 rounded-full \
      \flex items-center justify-center text-secondary-foreground/70 \
      \hover:bg-destructive hover:text-destructive-foreground \
      \transition-opacity focus:opacity-100"
