module Competences.Model.Grid
  (
  ) where

import qualified Data.Map as M
import Data.Text (Text)
import Data.UUID (UUID)
  
newtype EvidenceId = EvidenceId UUID
  deriving (Eq, Show)
newtype CompetenceGridId = CompetenceGridId UUID
  deriving (Eq, Show)
newtype CompetenceId = CompetenceId UUID
  deriving (Eq, Show)
newtype StudentId = StudentId UUID
  deriving (Eq, Show)

-- | Describes an entire competence grid; this is a list of
-- competences with a title and description. It is a grid,
-- because each of the competences has multiple levels; so the
-- grid has a fixed number of rows (the competences) and a fixed
-- number of columns (the levels).
data CompetenceGrid = CompetenceGrid
  { competenceGridId :: !CompetenceGridId
    -- ^ Unique identifier for the competence grid.
  , competenceGridTitle :: !Text
    -- ^ Title of the competence grid.
  , competenceGridDescription :: !Text
    -- ^ Description of the competence grid.
  , competenceGridCompetences :: ![Competence]
    -- ^ List of competences in the grid.
  } deriving (Eq, Show)

data Competence = Competence
  { competenceId :: !CompetenceId
  , competenceGridId :: !CompetenceGridId
  , competenceDescription :: !Text
  , competenceBasicDescription :: !(Maybe Text)
  , competenceIntermediateDescription :: !(Maybe Text)
  , competenceAdvancedDescription :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Information about a Student.
data Student = Student
  { studentId :: !StudentId
    -- ^ Unique identifier for the student.
  , studentFirstName :: !Text
    -- ^ First name of the student.
  , studentLastName :: !Text
    -- ^ Last name of the student.
  , student
  } deriving (Eq, Show)

-- | Whether a competence is demonstrated as part of a group or
-- individually.
data SocialForm
  = Group
    -- ^ Competence is demonstrated as part of a group.
  | Individual
    -- ^ Competence is demonstrated individually.
  deriving (Eq, Show, Ord)

-- | Whether the competence was demonstrated self-reliantly,
-- with some support or not yet at all.
data Ability
  = SelfReliant
    -- ^ Competence was demonstrated self-reliantly.
  | WithSupport
    -- ^ Competence was demonstrated with some support, like
    -- giving a hint or correcting a minor mistake.
  | NotYet
    -- ^ Competence was not successfully demonstrated, either
    -- because the student did not try, did not have the correct
    -- idea or they made a significant mistake.
  deriving (Eq, Show, Ord)

-- | Level of a competence.
data Level
  = BasicLevel
    -- ^ Basic level of competence; the essentials.
  | IntermediateLevel
    -- ^ Intermediate level; slightly going above the essentials.
  | AdvancedLevel
    -- ^ Advanced level; mastering the given competence in terms
    -- of the current curriculum.
  deriving (Eq, Show, Ord)

data Evidence = Evidence
  { evidenceId :: !EvidenceId
  , evidenceCompetenceId :: !CompetenceId
  , evidenceCompetenceLevel :: !Level
  , evidenceStudentId :: !StudentId
  , evidenceDate :: !Day
  , evidenceDescription :: !(Maybe Text)
  , evidenceSocialForm :: !SocialForm
  , evidenceAbility :: !Ability
  } deriving (Eq, Show)

-- | Given a list of evidences, produces a final result.
data Assessment
  = Competent
    -- ^ When we consider all evidences in reverse chronological order up
    -- to before the first one where `Ability` is `NotYet`, that sequence
    -- has at least two entries with `Ability` being `SelfReliant` on
    -- two different days and at least one entry has the `SocialForm`
    -- `Individual`.
  | ProbablyCompetent
    -- ^ When we consider all evidences in reverse chronological order up
    -- to before the second one where `Ability` is `NotYet`, that sequence
    -- has at least two entries with `Ability` being `SelfReliant` on
    -- two different days and at least one entry has the `SocialForm`
    -- `Individual`.
  | NotYetCompetent
    -- ^ When there are at least four evidences, but non of the criteria
    -- above are satisfied, we consider the student to be `NotYetCompetent`.
  | Inconclusive
    -- ^ When none of the criteria above are satisfied, we consider the
    -- assessment to be `Inconclusive`.
  deriving (Eq, Ord)
