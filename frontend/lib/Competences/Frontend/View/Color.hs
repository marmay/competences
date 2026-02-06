-- | Color palette interface for domain-specific colors.
--
-- This module defines only the interface. Domain-specific palettes
-- are defined in their own modules:
--
-- * 'Competences.Frontend.View.Color.Ability' - ability level colors
-- * 'Competences.Frontend.View.Color.Grade' - grade colors
module Competences.Frontend.View.Color
  ( ColorPalette (..)
  )
where

import Data.Text (Text)

-- | A color palette with foreground (text/stroke), background, and border colors.
-- Each field contains a Tailwind CSS class name.
data ColorPalette = ColorPalette
  { foreground :: !Text -- ^ text/stroke color class (e.g., "text-ability-success")
  , background :: !Text -- ^ full background class (e.g., "bg-ability-success")
  , border :: !Text -- ^ border color class (e.g., "border-ability-success")
  }
