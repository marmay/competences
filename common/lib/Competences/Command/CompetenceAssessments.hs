module Competences.Command.CompetenceAssessments
  ( CompetenceAssessmentsCommand (..)
  , CompetenceAssessmentPatch (..)
  , handleCompetenceAssessmentsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Assessment (CompetenceAssessment (..))
import Competences.Document.Competence (Level)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Patch for modifying a CompetenceAssessment.
-- Note: userId and competenceId are NOT patchable - they define the assessment subject.
-- To change these, delete the assessment and create a new one.
data CompetenceAssessmentPatch = CompetenceAssessmentPatch
  { level :: !(Change Level)
  , date :: !(Change Day)
  , comment :: !(Change (Maybe Text))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the CompetenceAssessments context
newtype CompetenceAssessmentsCommand
  = OnCompetenceAssessments (EntityCommand CompetenceAssessment CompetenceAssessmentPatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON CompetenceAssessmentPatch

instance ToJSON CompetenceAssessmentPatch

instance Binary CompetenceAssessmentPatch

instance FromJSON CompetenceAssessmentsCommand

instance ToJSON CompetenceAssessmentsCommand

instance Binary CompetenceAssessmentsCommand

-- Default instance
instance Default CompetenceAssessmentPatch where
  def = CompetenceAssessmentPatch {level = Nothing, date = Nothing, comment = Nothing}

-- | Apply a patch to a CompetenceAssessment, checking for conflicts
applyCompetenceAssessmentPatch :: CompetenceAssessment -> CompetenceAssessmentPatch -> Either Text CompetenceAssessment
applyCompetenceAssessmentPatch assessment patch =
  inContext "CompetenceAssessment" assessment $
    patchField' @"level" patch
      >=> patchField' @"date" patch
      >=> patchField' @"comment" patch

-- | Handle a CompetenceAssessments command
handleCompetenceAssessmentsCommand :: UserId -> CompetenceAssessmentsCommand -> Document -> UpdateResult
handleCompetenceAssessmentsCommand userId (OnCompetenceAssessments c) = interpretEntityCommand assessmentContext userId c
  where
    assessmentContext =
      mkEntityCommandContext
        #competenceAssessments
        #id
        CompetenceAssessmentLock
        applyCompetenceAssessmentPatch
        affectedUsers

    -- Teachers see all assessments; students only see assessments about themselves
    affectedUsers :: CompetenceAssessment -> Document -> AffectedUsers
    affectedUsers assessment d' =
      AffectedUsers $
        map (.id) $
          filter (\u -> u.role == Teacher || u.id == assessment.userId) $
            Ix.toList $
              d' ^. #users
