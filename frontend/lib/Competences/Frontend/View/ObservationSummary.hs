-- | Shared component for displaying observations with icons and tooltips
-- Designed for reuse across components showing observation summaries
module Competences.Frontend.View.ObservationSummary
  ( observationSummaryView
  , observationIconView
  )
where

import Competences.Document.Evidence (Ability (..), Observation (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Color (textClass)
import Competences.Frontend.View.Color.Ability (abilityPalette)
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Svg.Property qualified as MSP

-- | Get icon for an ability level
abilityIcon :: Ability -> Icon
abilityIcon SelfReliant = IcnAbilitySelfReliant
abilityIcon SelfReliantWithSillyMistakes = IcnAbilitySillyMistakes
abilityIcon WithSupport = IcnAbilityWithSupport
abilityIcon NotYet = IcnAbilityNotYet

-- | Render a single observation as a colored icon with tooltip
-- Uses ability icon, colored by ability level, with tooltip showing ability name
observationIconView :: Observation -> M.View m a
observationIconView obs =
  let abilityClass = textClass (abilityPalette obs.ability)
      abilityIcn = abilityIcon obs.ability
      tooltipText = C.translate' (C.LblAbility obs.ability)
   in withTooltip (PlainTooltip tooltipText) $
        M.span_
          [class_ abilityClass]
          [icon [MSP.stroke_ "currentColor", class_ "w-4 h-4"] abilityIcn]

-- | Render multiple observations as a row of icons
observationSummaryView :: [Observation] -> M.View m a
observationSummaryView observations =
  M.div_
    [class_ "flex gap-1 items-center"]
    (map observationIconView observations)
