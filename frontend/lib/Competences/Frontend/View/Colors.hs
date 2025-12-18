module Competences.Frontend.View.Colors
  ( abilityColor
  , gradualPercentageColor
  )
where

import Competences.Document.Evidence (Ability (..))
import Competences.Frontend.View.Tailwind

abilityColor :: Ability -> (Color, ColorStep, Opacity)
abilityColor SelfReliant = (Green, I700, O100)
abilityColor SelfReliantWithSillyMistakes = (Green, I500, O100)
abilityColor WithSupport = (Amber, I600, O100)
abilityColor NotYet = (Red, I600, O100)

gradualPercentageColor :: Double -> (Color, ColorStep, Opacity)
gradualPercentageColor percentage
  | percentage < 0.25 = (Red, I600, O100)
  | percentage < 0.5 = (Amber, I600, O100)
  | percentage < 0.75 = (Green, I500, O100)
  | otherwise = (Green, I700, O100)
