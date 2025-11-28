module Competences.Document.Template
  ( Template (..)
  , TemplateName (..)
  , TemplateAssessment (..)
  , TemplateIxs
  , mkTemplate
  , addAssessment
  )
where

import Competences.Document.Competence (CompetenceId, Level)
import Competences.Document.Evidence (ActivityType, SocialForm)
import Competences.Document.Id (Id)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Prelude hiding (id)

type TemplateId = Id Template

newtype TemplateName = TemplateName {unTemplateName :: Text}
  deriving (Eq, Generic, Ord, Show)

-- | When you frequently want to record the same activity, having a
-- template makes it a bit smoother.
data Template = Template
  { id :: !TemplateId
  -- ^ Id of the template.
  , name :: !TemplateName
  -- ^ Name of the template; free text field
  , date :: !Day
  -- ^ Default date of the activity.
  , activityType :: !ActivityType
  -- ^ Default ActivityType
  , socialForms :: ![SocialForm]
  -- ^ Possible SocialForms; if only one is given, then it is auto-selected.
  , assessments :: ![TemplateAssessment]
  -- ^ List of exercises and their respecitve competences.
  }
  deriving (Eq, Generic, Ord, Show)

data TemplateAssessment = TemplateAssessment
  { name :: !Text
  , achievableCompetence :: !CompetenceId
  , achievableLevels :: ![Level]
  }
  deriving (Eq, Generic, Ord, Show)

mkTemplate :: TemplateId -> TemplateName -> Day -> ActivityType -> [SocialForm] -> Template
mkTemplate id name date activityType socialForms =
  let assessments = []
  in Template {id, name, date, activityType, socialForms, assessments}

addAssessment :: TemplateAssessment -> Template -> Template
addAssessment e t = t {assessments = e : t.assessments}

type TemplateIxs = '[TemplateId, TemplateName]

instance Ix.Indexable TemplateIxs Template where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.name))

instance FromJSON TemplateName

instance ToJSON TemplateName

instance Binary TemplateName

instance FromJSON Template

instance ToJSON Template

instance Binary Template

instance FromJSON TemplateAssessment

instance ToJSON TemplateAssessment

instance Binary TemplateAssessment
