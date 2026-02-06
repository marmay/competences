-- | Color palette interface for domain-specific colors.
--
-- This module defines the 'PaletteName' type, 'PaletteColor' variants, and
-- helper functions for deriving CSS class names and variable references.
-- Domain-specific palettes are defined in their own modules:
--
-- * 'Competences.Frontend.View.Color.Ability' - ability level colors
-- * 'Competences.Frontend.View.Color.Grade' - grade colors
-- * 'Competences.Frontend.View.Color.Mastery' - mastery status colors
-- * 'Competences.Frontend.View.Color.Status' - status indicator colors
module Competences.Frontend.View.Color
  ( -- * Palette name type
    PaletteName (..)
    -- * Palette color variants
  , PaletteColor (..)
    -- * CSS class derivation (with explicit variant)
  , textClass
  , bgClass
  , borderClass
    -- * CSS class derivation (convenience wrappers with default variants)
  , textClass'
  , bgClass'
  , borderClass'
    -- * CSS variable references
  , cssVar
    -- * Striped background generation
  , paletteStripedStyle
  )
where

import Data.Text (Text)
import Miso.String (MisoString, ms)

-- | A semantic color palette name.
--
-- Corresponds to CSS variables following Basecoat pattern:
--
-- * @--{name}@ : base color (background)
-- * @--{name}-accent@ : accent color (icons, strokes, vibrant)
-- * @--{name}-alt@ : alternate color (stripes, subtle variant)
newtype PaletteName = PaletteName {unPaletteName :: Text}
  deriving (Eq, Show)

-- | Color variant within a palette.
--
-- Each palette defines three color variants:
--
-- * 'Base' - Background color, the default/primary variant
-- * 'Accent' - Vibrant color for icons, strokes, and emphasis
-- * 'Alt' - Alternate/lighter variant for striping and subtle effects
data PaletteColor
  = Base
  | Accent
  | Alt
  deriving (Eq, Show)

-- | CSS suffix for a palette color variant.
colorSuffix :: PaletteColor -> Text
colorSuffix Base = ""
colorSuffix Accent = "-accent"
colorSuffix Alt = "-alt"

-- | CSS class for text in the palette color.
--
-- @textClass Accent (PaletteName "ability-success")@ = @"text-ability-success-accent"@
-- @textClass Base (PaletteName "ability-success")@ = @"text-ability-success"@
textClass :: PaletteColor -> PaletteName -> Text
textClass variant (PaletteName n) = "text-" <> n <> colorSuffix variant

-- | CSS class for background color.
--
-- @bgClass Base (PaletteName "ability-success")@ = @"bg-ability-success"@
-- @bgClass Alt (PaletteName "ability-success")@ = @"bg-ability-success-alt"@
bgClass :: PaletteColor -> PaletteName -> Text
bgClass variant (PaletteName n) = "bg-" <> n <> colorSuffix variant

-- | CSS class for border color.
--
-- @borderClass Accent (PaletteName "ability-success")@ = @"border-ability-success-accent"@
borderClass :: PaletteColor -> PaletteName -> Text
borderClass variant (PaletteName n) = "border-" <> n <> colorSuffix variant

-- | CSS class for accent text color (icons, strokes).
-- Convenience wrapper: @textClass' = textClass Accent@
textClass' :: PaletteName -> Text
textClass' = textClass Accent

-- | CSS class for base background color.
-- Convenience wrapper: @bgClass' = bgClass Base@
bgClass' :: PaletteName -> Text
bgClass' = bgClass Base

-- | CSS class for accent border color.
-- Convenience wrapper: @borderClass' = borderClass Accent@
borderClass' :: PaletteName -> Text
borderClass' = borderClass Accent

-- | CSS variable reference for a palette color variant.
--
-- @cssVar Base (PaletteName "ability-success")@ = @"var(--ability-success)"@
-- @cssVar Accent (PaletteName "ability-success")@ = @"var(--ability-success-accent)"@
-- @cssVar Alt (PaletteName "ability-success")@ = @"var(--ability-success-alt)"@
cssVar :: PaletteColor -> PaletteName -> Text
cssVar variant (PaletteName n) = "var(--" <> n <> colorSuffix variant <> ")"

-- | Generate striped background style using the palette's alt and base variants.
--
-- Returns an inline style attribute value for a diagonal stripe pattern
-- using the palette's @-alt@ (light) and base (dark) CSS variables.
paletteStripedStyle :: PaletteName -> [(MisoString, MisoString)]
paletteStripedStyle p =
  [ ( "background"
    , "repeating-linear-gradient(135deg, "
        <> ms (cssVar Alt p)
        <> " 0px, "
        <> ms (cssVar Alt p)
        <> " 4px, "
        <> ms (cssVar Base p)
        <> " 4px, "
        <> ms (cssVar Base p)
        <> " 8px)"
    )
  ]
