{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Competences.Frontend.View.DesignTokens
Description: Basecoat-inspired design tokens as simple Text constants

This module provides design tokens extracted from Basecoat UI for consistent styling.
These are simple Text constants that can be composed in class_ calls.

Example usage:
@
import Competences.Frontend.View.DesignTokens as DT
import Competences.Frontend.View.Tailwind (class_)

-- Using spacing tokens
button_ [class_ $ "gap-" <> DT.space2] [text "Click"]

-- Using color tokens
div_ [class_ $ "bg-" <> DT.colorPrimary <> "-600"] []

-- Using typography tokens
h1_ [class_ $ "text-" <> DT.textXl] [text "Title"]
@
-}
module Competences.Frontend.View.DesignTokens
  ( -- * Spacing Scale
    space0
  , space1
  , space2
  , space3
  , space4
  , space6
  , space8
  , space12
  , space16

    -- * Colors (Semantic Names)
  , colorPrimary
  , colorSecondary
  , colorDestructive
  , colorAccent
  , colorMuted

    -- * Border Radius
  , radiusNone
  , radiusSm
  , radiusMd
  , radiusLg
  , radiusXl
  , radiusFull

    -- * Shadows
  , shadowNone
  , shadowXs
  , shadowSm
  , shadowMd
  , shadowLg

    -- * Typography Scale
  , textXs
  , textSm
  , textBase
  , textLg
  , textXl
  , text2xl
  , text3xl

    -- * Common Class Patterns
  , focusRing
  , transition
  )
where

import Data.Text (Text)

-- | Spacing scale from Tailwind/Basecoat
--
-- Use with utility prefixes: "gap-", "px-", "py-", "m-", "p-", etc.
--
-- Examples:
-- @
-- "gap-" <> space2  -- "gap-2"
-- "px-" <> space4   -- "px-4"
-- @
space0, space1, space2, space3, space4, space6, space8, space12, space16 :: Text
space0 = "0"
space1 = "1"
space2 = "2"
space3 = "3"
space4 = "4"
space6 = "6"
space8 = "8"
space12 = "12"
space16 = "16"

-- | Semantic color names mapped to Tailwind palette
--
-- Use with utility prefixes: "bg-", "text-", "border-", etc.
-- Typically combined with color step (e.g., "-600", "-200")
--
-- Examples:
-- @
-- "bg-" <> colorPrimary <> "-600"      -- "bg-sky-600"
-- "text-" <> colorDestructive <> "-700" -- "text-red-700"
-- @
colorPrimary, colorSecondary, colorDestructive, colorAccent, colorMuted :: Text
colorPrimary = "sky"      -- Primary actions, links
colorSecondary = "stone"  -- Secondary actions, neutral elements
colorDestructive = "red"  -- Destructive/delete actions, errors
colorAccent = "stone"     -- Hover states, subtle highlights (using stone-100)
colorMuted = "stone"      -- Muted backgrounds and text (using stone-100, stone-500)

-- | Border radius scale
--
-- Use with "rounded-" prefix
--
-- Examples:
-- @
-- "rounded-" <> radiusMd   -- "rounded-md"
-- "rounded-" <> radiusFull -- "rounded-full"
-- @
radiusNone, radiusSm, radiusMd, radiusLg, radiusXl, radiusFull :: Text
radiusNone = "none"
radiusSm = "sm"
radiusMd = "md"
radiusLg = "lg"
radiusXl = "xl"
radiusFull = "full"

-- | Shadow scale
--
-- Use with "shadow-" prefix
--
-- Examples:
-- @
-- "shadow-" <> shadowSm -- "shadow-sm"
-- "shadow-" <> shadowLg -- "shadow-lg"
-- @
shadowNone, shadowXs, shadowSm, shadowMd, shadowLg :: Text
shadowNone = "none"
shadowXs = "xs"
shadowSm = "sm"
shadowMd = "md"
shadowLg = "lg"

-- | Typography scale
--
-- Use with "text-" prefix
--
-- Examples:
-- @
-- "text-" <> textXl   -- "text-xl"
-- "text-" <> textBase -- "text-base"
-- @
textXs, textSm, textBase, textLg, textXl, text2xl, text3xl :: Text
textXs = "xs"
textSm = "sm"
textBase = "base"
textLg = "lg"
textXl = "xl"
text2xl = "2xl"
text3xl = "3xl"

-- | Standard focus ring pattern from Basecoat
--
-- Use directly in class lists
focusRing :: Text
focusRing = "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2"

-- | Standard transition for color changes
--
-- Use directly in class lists
transition :: Text
transition = "transition-colors"
