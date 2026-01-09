module Competences.Command.CompetenceGridGrades
  ( CompetenceGridGradesCommand (..)
  , CompetenceGridGradePatch (..)
  , handleCompetenceGridGradesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Grade (Grade)
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

-- | Patch for modifying a CompetenceGridGrade.
-- Note: userId and competenceGridId are NOT patchable - they define the grade subject.
-- To change these, delete the grade and create a new one.
data CompetenceGridGradePatch = CompetenceGridGradePatch
  { grade :: !(Change Grade)
  , date :: !(Change Day)
  , comment :: !(Change (Maybe Text))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the CompetenceGridGrades context
newtype CompetenceGridGradesCommand
  = OnCompetenceGridGrades (EntityCommand CompetenceGridGrade CompetenceGridGradePatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON CompetenceGridGradePatch

instance ToJSON CompetenceGridGradePatch

instance Binary CompetenceGridGradePatch

instance FromJSON CompetenceGridGradesCommand

instance ToJSON CompetenceGridGradesCommand

instance Binary CompetenceGridGradesCommand

-- Default instance
instance Default CompetenceGridGradePatch where
  def = CompetenceGridGradePatch {grade = Nothing, date = Nothing, comment = Nothing}

-- | Apply a patch to a CompetenceGridGrade, checking for conflicts
applyCompetenceGridGradePatch :: CompetenceGridGrade -> CompetenceGridGradePatch -> Either Text CompetenceGridGrade
applyCompetenceGridGradePatch gridGrade patch =
  inContext "CompetenceGridGrade" gridGrade $
    patchField' @"grade" patch
      >=> patchField' @"date" patch
      >=> patchField' @"comment" patch

-- | Handle a CompetenceGridGrades command
handleCompetenceGridGradesCommand :: UserId -> CompetenceGridGradesCommand -> Document -> UpdateResult
handleCompetenceGridGradesCommand userId (OnCompetenceGridGrades c) = interpretEntityCommand gradeContext userId c
  where
    gradeContext =
      mkEntityCommandContext
        #competenceGridGrades
        #id
        CompetenceGridGradeLock
        applyCompetenceGridGradePatch
        affectedUsers

    -- Teachers see all grades; students only see grades about themselves
    affectedUsers :: CompetenceGridGrade -> Document -> AffectedUsers
    affectedUsers gridGrade d' =
      AffectedUsers $
        map (.id) $
          filter (\u -> u.role == Teacher || u.id == gridGrade.userId) $
            Ix.toList $
              d' ^. #users
