module Competences.Command.CompetenceAssessments
  ( CompetenceAssessmentsCommand (..)
  , CompetenceAssessmentPatch (..)
  , handleCompetenceAssessmentsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Assessment (CompetenceAssessment (..))
import Competences.Document.Competence (CompetenceId, Level)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as Ix
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((%~), (&), (^.))

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
--
-- Special handling for Create: enforces one-assessment-per-day constraint.
-- If an assessment already exists for the same (userId, competenceId, date),
-- the existing assessment is updated instead of creating a duplicate.
handleCompetenceAssessmentsCommand :: UserId -> CompetenceAssessmentsCommand -> Document -> UpdateResult
handleCompetenceAssessmentsCommand userId (OnCompetenceAssessments c) d = case c of
  -- Special Create handling: one-per-day constraint
  Create assessment -> handleCreateWithConstraint assessment d
  -- All other commands use standard interpretation
  _ -> interpretEntityCommand assessmentContext userId c d
  where
    assessmentContext =
      mkEntityCommandContext
        #competenceAssessments
        #id
        CompetenceAssessmentLock
        applyCompetenceAssessmentPatch
        affectedUsers

    -- Handle Create with one-per-day constraint:
    -- If assessment exists for same (userId, competenceId, date), update it instead
    handleCreateWithConstraint :: CompetenceAssessment -> Document -> UpdateResult
    handleCreateWithConstraint newAssessment doc =
      case findExistingForDay doc newAssessment.userId newAssessment.competenceId newAssessment.date of
        Just existing ->
          -- Update existing assessment's level and comment
          let updated =
                CompetenceAssessment
                  { id = existing.id
                  , userId = existing.userId
                  , competenceId = existing.competenceId
                  , level = newAssessment.level
                  , date = existing.date
                  , comment = newAssessment.comment
                  }
              doc' = doc & #competenceAssessments %~ Ix.updateIx existing.id updated
           in Right (doc', affectedUsers updated doc')
        Nothing ->
          -- No existing assessment for this day, create new
          interpretEntityCommand assessmentContext userId (Create newAssessment) doc

    -- Find an existing assessment for the same (userId, competenceId, date)
    findExistingForDay :: Document -> UserId -> CompetenceId -> Day -> Maybe CompetenceAssessment
    findExistingForDay doc uid cid day =
      listToMaybe $
        filter (\a -> a.competenceId == cid && a.date == day) $
          Ix.toList (doc.competenceAssessments Ix.@= uid)

    -- Teachers see all assessments; students only see assessments about themselves
    affectedUsers :: CompetenceAssessment -> Document -> AffectedUsers
    affectedUsers assessment d' =
      AffectedUsers $
        map (.id) $
          filter (\u -> u.role == Teacher || u.id == assessment.userId) $
            Ix.toList $
              d' ^. #users
