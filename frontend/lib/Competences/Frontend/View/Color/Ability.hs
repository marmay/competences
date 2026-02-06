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
import Competences.Frontend.View.Color (ColorPalette (..))

-- | Get the color palette for an ability level.
abilityPalette :: Ability -> ColorPalette
abilityPalette SelfReliant =
  ColorPalette
    { foreground = "text-ability-success"
    , background = "bg-ability-success"
    , border = "border-ability-success"
    }
abilityPalette SelfReliantWithSillyMistakes =
  ColorPalette
    { foreground = "text-ability-success-light"
    , background = "bg-ability-success-light"
    , border = "border-ability-success-light"
    }
abilityPalette WithSupport =
  ColorPalette
    { foreground = "text-ability-warning"
    , background = "bg-ability-warning"
    , border = "border-ability-warning"
    }
abilityPalette NotYet =
  ColorPalette
    { foreground = "text-ability-danger"
    , background = "bg-ability-danger"
    , border = "border-ability-danger"
    }
