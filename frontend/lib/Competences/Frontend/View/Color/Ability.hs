-- | Ability-level color palette.
--
-- Maps 'Ability' values to semantic color palettes using CSS variables
-- defined in @input.css@. The colors follow a traffic-light pattern:
--
-- * 'SelfReliant' - green (success)
-- * 'SelfReliantWithSillyMistakes' - light green (success-light)
-- * 'WithSupport' - yellow/amber (warning)
-- * 'NotYet' - red (danger)
module Competences.Frontend.View.Color.Ability
  ( abilityPalette
  )
where

import Competences.Document.Evidence (Ability (..))
import Competences.Frontend.View.Color (PaletteName (..))

-- | Get the color palette name for an ability level.
abilityPalette :: Ability -> PaletteName
abilityPalette SelfReliant = PaletteName "ability-success"
abilityPalette SelfReliantWithSillyMistakes = PaletteName "ability-success-light"
abilityPalette WithSupport = PaletteName "ability-warning"
abilityPalette NotYet = PaletteName "ability-danger"
