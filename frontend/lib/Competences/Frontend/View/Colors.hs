module Competences.Frontend.View.Colors
  ( -- * Ability colors using CSS variables (preferred)
    abilityTextClass
    -- * Ability colors using Tailwind palette (for backwards compat)
  , abilityColor
    -- * Percentage gradients
  , gradualPercentageColor
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
