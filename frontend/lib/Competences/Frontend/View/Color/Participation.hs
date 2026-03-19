-- | Participation observation color palettes and icon mappings.
--
-- Maps participation types and levels to CSS color palettes and icons
-- for consistent visualization across components.
module Competences.Frontend.View.Color.Participation
  ( participationPalette
  , participationLevelIcon
  , participationTypeIcon
  )
where

import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationType (..))
import Competences.Frontend.View.Color (PaletteName (..))
import Competences.Frontend.View.Icon qualified as Icon

-- | Map a participation type to its CSS color palette.
--
-- * Participation, Collaboration → @participation-positive@ (green)
-- * PoorWorkEthic → @participation-negative@ (red)
participationPalette :: ParticipationType -> PaletteName
participationPalette PoorWorkEthic = PaletteName "participation-negative"
participationPalette _ = PaletteName "participation-positive"

-- | Icon for a participation level within its type.
--
-- * Positive types: Level1 → +, Level2 → ++
-- * PoorWorkEthic: Level1 → −, Level2 → −−
participationLevelIcon :: ParticipationType -> ParticipationLevel -> Icon.Icon
participationLevelIcon PoorWorkEthic ParticipationLevel1 = Icon.IcnMinus
participationLevelIcon PoorWorkEthic ParticipationLevel2 = Icon.IcnMinusMinus
participationLevelIcon _ ParticipationLevel1 = Icon.IcnPlus
participationLevelIcon _ ParticipationLevel2 = Icon.IcnPlusPlus

-- | Icon representing the participation type itself (for legends).
--
-- * Participation → individual icon
-- * Collaboration → group icon
-- * PoorWorkEthic → task icon
participationTypeIcon :: ParticipationType -> Icon.Icon
participationTypeIcon Participation = Icon.IcnSocialFormIndividual
participationTypeIcon Collaboration = Icon.IcnSocialFormGroup
participationTypeIcon PoorWorkEthic = Icon.IcnTask
