module Competences.Document.Template
  ( Template (..)
  , TemplateEvaluation (..)
  , TemplateName (..)
  , TemplateAspect (..)
  , TemplateIxs
  , mkTemplate
  , addAspect
  , mkEvidenceFromTemplateEvaluation
  )
where

import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence
  ( Ability
  , ActivityType
  , Evidence (..)
  , Observation (..)
  , SocialForm
  )
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import System.Random (RandomGen, random, randoms)
import Prelude hiding (id)

type TemplateId = Id Template

newtype TemplateName = TemplateName {unTemplateName :: Text}
  deriving (Eq, Generic, Ord, Show)

-- | When you frequently want to record the same activity, having a
-- template makes it a bit smoother. You evaluate a fixed set of
-- aspects and can auto-aggregate information from those aspects.
data Template = Template
  { id :: !TemplateId
  -- ^ Id of the template.
  , name :: !TemplateName
  -- ^ Name of the template; free text field
  , date :: !Day
  -- ^ Default date of the activity.
  , activityType :: !ActivityType
  -- ^ Default ActivityType
  , socialForm :: !SocialForm
  -- ^ Default SocialForm.
  , aspects :: ![TemplateAspect]
  -- ^ List of exercises and their respecitve competences.
  }
  deriving (Eq, Generic, Ord, Show)

data TemplateAspect = TemplateAspect
  { name :: !Text
  , achievableCompetenceLevel :: !CompetenceLevelId
  }
  deriving (Eq, Generic, Ord, Show)

data TemplateEvaluation = TemplateEvaluation
  { template :: !Template
  -- ^ Refers to the evaluated template.
  , userId :: !(Maybe UserId)
  -- ^ User that the template is evaluated for.
  , assessments :: ![(TemplateAspect, Maybe Ability)]
  -- ^ Asessments made.
  }
  deriving (Eq, Generic, Ord, Show)

mkEvidenceFromTemplateEvaluation :: (RandomGen g) => g -> TemplateEvaluation -> Evidence
mkEvidenceFromTemplateEvaluation g t =
  let (evidenceId, g') = random g
      oldTasksText = t.template.name.unTemplateName
      byCompetenceLevelId =
        Map.fromListWith max $
          mapMaybe
            ( \(task, ability) -> do
                ability' <- ability
                pure (task.achievableCompetenceLevel, ability')
            )
            t.assessments
      observations =
        zipWith
          ( \oId (competenceLevelId, ability) -> Observation {id = oId, competenceLevelId, socialForm = t.template.socialForm, ability}
          )
          (randoms g')
          (Map.toList byCompetenceLevelId)
   in Evidence
        { id = evidenceId
        , userId = t.userId
        , activityType = t.template.activityType
        , date = t.template.date
        , tasks = []
        , oldTasks = oldTasksText
        , observations = Ix.fromList observations
        }

mkTemplate :: TemplateId -> TemplateName -> Day -> ActivityType -> SocialForm -> Template
mkTemplate id name date activityType socialForm =
  let aspects = []
   in Template {id, name, date, activityType, socialForm, aspects}

addAspect :: TemplateAspect -> Template -> Template
addAspect e t = t {aspects = e : t.aspects}

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

instance FromJSON TemplateAspect

instance ToJSON TemplateAspect

instance Binary TemplateAspect
