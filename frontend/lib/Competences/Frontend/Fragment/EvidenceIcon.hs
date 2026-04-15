-- | Stateless icon mappings for evidence-related domain types.
--
-- Provides mapping functions from 'ActivityType' and 'SocialForm' to
-- their corresponding 'Icon' values, plus a small helper for rendering
-- icons with @stroke="currentColor"@ so they inherit the text color.
module Competences.Frontend.Fragment.EvidenceIcon
  ( activityTypeIcon
  , socialFormIcon
  , coloredStrokeIcon
  )
where

import Competences.Document.Evidence (ActivityType (..), SocialForm (..))
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Svg.Property qualified as MSP

-- | Map an 'ActivityType' to its icon.
activityTypeIcon :: ActivityType -> Icon.Icon
activityTypeIcon Conversation = Icon.IcnActivityTypeConversation
activityTypeIcon Exam = Icon.IcnActivityTypeExam
activityTypeIcon SchoolExercise = Icon.IcnActivityTypeSchoolExercise
activityTypeIcon HomeExercise = Icon.IcnActivityTypeHomeExercise

-- | Map a 'SocialForm' to its icon.
socialFormIcon :: SocialForm -> Icon.Icon
socialFormIcon Group = Icon.IcnSocialFormGroup
socialFormIcon Individual = Icon.IcnSocialFormIndividual

-- | Render an icon coloured by the given CSS class (e.g. an ability class).
-- Uses @stroke="currentColor"@ so the icon inherits the text colour.
coloredStrokeIcon :: Text -> Icon.Icon -> M.View m a
coloredStrokeIcon colorClass icn =
  MH.span_
    [class_ colorClass]
    [Icon.icon [MSP.stroke_ "currentColor"] icn]
