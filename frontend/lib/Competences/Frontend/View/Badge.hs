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
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
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
    -- Base classes from Basecoat
    baseClasses = "inline-flex items-center justify-center rounded-full border px-2 py-0.5 text-xs font-medium focus-visible:border-sky-600 focus-visible:ring-[3px]"

    -- Variant classes
    variantClasses BadgePrimary = "bg-sky-600 text-white border-sky-600"
    variantClasses BadgeSecondary = "bg-stone-200 text-stone-900 border-stone-200"
    variantClasses BadgeDestructive = "bg-red-600 text-white border-red-600"
    variantClasses BadgeOutline = "text-stone-900 border-stone-300 hover:bg-stone-100"

-- | Convenient badge constructors
badgePrimary, badgeSecondary, badgeDestructive, badgeOutline
  :: MisoString -> M.View model action
badgePrimary = badge BadgePrimary
badgeSecondary = badge BadgeSecondary
badgeDestructive = badge BadgeDestructive
badgeOutline = badge BadgeOutline
