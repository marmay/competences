module Competences.Frontend.View.Colors
  ( -- * Ability colors using CSS variables (preferred)
    abilityTextClass
  , abilityBgClass
    -- * Ability colors using Tailwind palette (for backwards compat)
  , abilityColor
    -- * Percentage gradients
  , gradualPercentageColor
  , gradualPercentageTextClass
  )
where

import Competences.Document.Evidence (Ability (..))
import Competences.Frontend.View.Tailwind
import Data.Text (Text)

-- ============================================================================
-- CSS Variable-based ability colors (preferred approach)
-- ============================================================================

-- | Ability text color class using CSS variables
-- Use this with SVG icons via currentColor pattern
abilityTextClass :: Ability -> Text
abilityTextClass SelfReliant = "text-ability-success"
abilityTextClass SelfReliantWithSillyMistakes = "text-ability-success-light"
abilityTextClass WithSupport = "text-ability-warning"
abilityTextClass NotYet = "text-ability-danger"

-- | Ability background color class using CSS variables
abilityBgClass :: Ability -> Text
abilityBgClass SelfReliant = "bg-ability-success"
abilityBgClass SelfReliantWithSillyMistakes = "bg-ability-success-light"
abilityBgClass WithSupport = "bg-ability-warning"
abilityBgClass NotYet = "bg-ability-danger"

-- ============================================================================
-- Tailwind palette-based colors (for coloredText_ / tailwindColors)
-- ============================================================================

-- | Ability color using Tailwind palette (for coloredText_ compatibility)
abilityColor :: Ability -> (Color, ColorStep, Opacity)
abilityColor SelfReliant = (Green, I700, O100)
abilityColor SelfReliantWithSillyMistakes = (Green, I500, O100)
abilityColor WithSupport = (Amber, I600, O100)
abilityColor NotYet = (Red, I600, O100)

-- | Percentage gradient color using Tailwind palette
gradualPercentageColor :: Double -> (Color, ColorStep, Opacity)
gradualPercentageColor percentage
  | percentage < 0.25 = (Red, I600, O100)
  | percentage < 0.5 = (Amber, I600, O100)
  | percentage < 0.75 = (Green, I500, O100)
  | otherwise = (Green, I700, O100)

-- | Percentage gradient text class using CSS variables
gradualPercentageTextClass :: Double -> Text
gradualPercentageTextClass percentage
  | percentage < 0.25 = "text-ability-danger"
  | percentage < 0.5 = "text-ability-warning"
  | percentage < 0.75 = "text-ability-success-light"
  | otherwise = "text-ability-success"
