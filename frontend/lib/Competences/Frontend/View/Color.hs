-- | Color palette interface for domain-specific colors.
--
-- This module defines the 'PaletteName' type and helper functions for deriving
-- CSS class names and variable references. Domain-specific palettes are defined
-- in their own modules:
--
-- * 'Competences.Frontend.View.Color.Ability' - ability level colors
-- * 'Competences.Frontend.View.Color.Grade' - grade colors
-- * 'Competences.Frontend.View.Color.Mastery' - mastery status colors
module Competences.Frontend.View.Color
  ( -- * Palette name type
    PaletteName (..)
    -- * CSS class derivation
  , textClass
  , textOnBgClass
  , bgClass
  , borderClass
    -- * Badge palette conversion
  , toBadgePalette
    -- * CSS variable references (for inline styles like gradients)
  , cssVar
  , cssVarStripeLight
  , cssVarStripeDark
    -- * Striped background generation
  , paletteStripedStyle
  )
where

import Data.Text (Text)
import Miso.String (MisoString)

-- | A semantic color palette name.
--
-- Corresponds to CSS variables following Basecoat pattern:
--
-- * @--{name}@ : the semantic color (text/stroke/icons, background)
-- * @--{name}-foreground@ : contrast text color for on top of bg
-- * @--{name}-stripe-light@ : light variant for striped backgrounds
-- * @--{name}-stripe-dark@ : dark variant for striped backgrounds
newtype PaletteName = PaletteName {unPaletteName :: Text}
  deriving (Eq, Show)

-- | CSS class for text in the palette color (icons, accents).
--
-- @textClass (PaletteName "ability-success")@ = @"text-ability-success"@
textClass :: PaletteName -> Text
textClass (PaletteName n) = "text-" <> n

-- | CSS class for contrast text on top of the palette background.
--
-- @textOnBgClass (PaletteName "ability-success")@ = @"text-ability-success-foreground"@
textOnBgClass :: PaletteName -> Text
textOnBgClass (PaletteName n) = "text-" <> n <> "-foreground"

-- | CSS class for background color.
--
-- @bgClass (PaletteName "ability-success")@ = @"bg-ability-success"@
bgClass :: PaletteName -> Text
bgClass (PaletteName n) = "bg-" <> n

-- | CSS class for border color.
--
-- @borderClass (PaletteName "ability-success")@ = @"border-ability-success"@
borderClass :: PaletteName -> Text
borderClass (PaletteName n) = "border-" <> n

-- | Convert a PaletteName to a tuple of explicit class names (text, bg, border).
-- Useful for building palette records that expect separate fields.
toBadgePalette :: PaletteName -> (Text, Text, Text)
toBadgePalette p = (textClass p, bgClass p, borderClass p)

-- | CSS variable reference for the base color.
--
-- @cssVar (PaletteName "ability-success")@ = @"var(--ability-success)"@
cssVar :: PaletteName -> Text
cssVar (PaletteName n) = "var(--" <> n <> ")"

-- | CSS variable reference for the light stripe variant.
--
-- @cssVarStripeLight (PaletteName "ability-success")@ = @"var(--ability-success-stripe-light)"@
cssVarStripeLight :: PaletteName -> Text
cssVarStripeLight (PaletteName n) = "var(--" <> n <> "-stripe-light)"

-- | CSS variable reference for the dark stripe variant.
--
-- @cssVarStripeDark (PaletteName "ability-success")@ = @"var(--ability-success-stripe-dark)"@
cssVarStripeDark :: PaletteName -> Text
cssVarStripeDark (PaletteName n) = "var(--" <> n <> "-stripe-dark)"

-- | Generate striped background style using the palette's stripe variants.
--
-- Returns an inline style attribute value for a diagonal stripe pattern
-- using the palette's @-stripe-light@ and @-stripe-dark@ CSS variables.
paletteStripedStyle :: PaletteName -> [(MisoString, MisoString)]
paletteStripedStyle p =
  [ ( "background"
    , "repeating-linear-gradient(135deg, "
        <> cssVarStripeLight p
        <> " 0px, "
        <> cssVarStripeLight p
        <> " 4px, "
        <> cssVarStripeDark p
        <> " 4px, "
        <> cssVarStripeDark p
        <> " 8px)"
    )
  ]
