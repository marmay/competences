{-# LANGUAGE TypeFamilies #-}

module Competences.Document
  ( Document (..)
  , emptyDocument
  , projectDocument
  -- * Staleness computation for CompetenceAssessment
  , isCompetenceAssessmentStale
  , getActiveAssessment
  , getAssessmentHistory
  -- * Staleness computation for CompetenceGridGrade
  , isCompetenceGridGradeStale
  , getActiveGridGrade
  , getGridGradeHistory
  , module Competences.Document.Assessment
  , module Competences.Document.CompetenceGridGrade
  , module Competences.Document.Grade
  , module Competences.Document.Lock
  , module Competences.Document.Competence
  , module Competences.Document.CompetenceGrid
  , module Competences.Document.Evidence
  , module Competences.Document.Order
  , module Competences.Document.Resource
  , module Competences.Document.Task
  , module Competences.Document.Assignment
  , module Competences.Document.User
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Assessment
  ( CompetenceAssessment (..)
  , CompetenceAssessmentId
  , CompetenceAssessmentIxs
  )
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentIxs)
import Competences.Document.Competence
  ( Competence (..)
  , CompetenceId
  , CompetenceIxs
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , levelDescription
  , isLevelLocked
  , hasLevelContent
  )
import Competences.Document.CompetenceGrid
  ( CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceGridIxs
  , emptyCompetenceGrid
  )
import Competences.Document.CompetenceGridGrade
  ( CompetenceGridGrade (..)
  , CompetenceGridGradeId
  , CompetenceGridGradeIxs
  )
import Competences.Document.Evidence (Evidence (..), EvidenceId, EvidenceIxs, Observation (..))
import Competences.Document.Grade (Grade (..), grades, gradeToText)
import Competences.Document.Lock (Lock (..))
import Competences.Document.Order (Order, orderAt, orderMax, orderMin, ordered)
import Competences.Document.Resource (Resource (..), ResourceId, ResourceIxs)
import Competences.Document.Task (Task (..), TaskId, TaskIxs, TaskGroup (..), TaskGroupId, TaskGroupIxs, TaskType (..))
import Competences.Document.User (User (..), UserId, UserIxs, UserRole (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (^.))

data Document = Document
  { competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , resources :: !(Ix.IxSet ResourceIxs Resource)
  , locks :: !(M.Map Lock UserId)
  , users :: !(Ix.IxSet UserIxs User)
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , assignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , competenceAssessments :: !(Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment)
  , competenceGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  }
  deriving (Eq, Generic, Show)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v ->
    Document
      <$> v .: "competenceGrids"
      <*> fmap Ix.fromList (v .: "competences")
      <*> fmap Ix.fromList (v .: "evidences")
      <*> fmap Ix.fromList (v .: "resources")
      <*> fmap M.fromList (v .: "locks")
      <*> fmap Ix.fromList (v .: "users")
      <*> fmap Ix.fromList (v .: "tasks")
      <*> fmap Ix.fromList (v .: "taskGroups")
      <*> fmap Ix.fromList (v .: "assignments")
      <*> fmap Ix.fromList (v .:? "competenceAssessments" .!= [])
      <*> fmap Ix.fromList (v .:? "competenceGridGrades" .!= [])

instance ToJSON Document where
  toJSON d =
    object
      [ "competenceGrids" .= d.competenceGrids
      , "competences" .= Ix.toList d.competences
      , "evidences" .= Ix.toList d.evidences
      , "resources" .= Ix.toList d.resources
      , "locks" .= M.toList d.locks
      , "users" .= Ix.toList d.users
      , "tasks" .= Ix.toList d.tasks
      , "taskGroups" .= Ix.toList d.taskGroups
      , "assignments" .= Ix.toList d.assignments
      , "competenceAssessments" .= Ix.toList d.competenceAssessments
      , "competenceGridGrades" .= Ix.toList d.competenceGridGrades
      ]

emptyDocument :: Document
emptyDocument =
  Document
    { competenceGrids = Ix.empty
    , competences = Ix.empty
    , evidences = Ix.empty
    , resources = Ix.empty
    , locks = M.empty
    , users = Ix.empty
    , tasks = Ix.empty
    , taskGroups = Ix.empty
    , assignments = Ix.empty
    , competenceAssessments = Ix.empty
    , competenceGridGrades = Ix.empty
    }


-- | Project document based on user identity for access control
-- Teachers see full document, students see filtered view
projectDocument :: User -> Document -> Document
projectDocument user doc
  | user.role == Teacher = doc -- Teachers see everything
  | otherwise =
      -- Students see filtered view based on their identity
      doc
        & #users .~ Ix.fromList [user] -- Only their own user
        & #evidences .~ (doc.evidences Ix.@= user.id) -- Only evidences about them (via UserId index)
        & #assignments .~ (doc.assignments Ix.@= user.id) -- Only assignments assigned to them (via UserId index)
        & #competenceAssessments .~ (doc.competenceAssessments Ix.@= user.id) -- Only assessments about them
        & #competenceGridGrades .~ (doc.competenceGridGrades Ix.@= user.id) -- Only grid grades about them
        & #locks .~ M.filterWithKey isLockVisible (doc ^. #locks) -- Only locks on entities they can see
        -- competenceGrids, competences, resources, tasks, taskGroups: students see all (public materials)
        -- TODO: May need to filter tasks/taskGroups in the future (e.g., hide exam questions before exam date)
  where
    -- Student can see locks on entities they have access to
    isLockVisible lock _ = case lock of
      UserLock uid -> uid == user.id -- Only their own user
      EvidenceLock eid ->
        case Ix.getOne (Ix.getEQ eid (doc ^. #evidences)) of
          Just e -> Just user.id == e.userId -- Only evidences about them
          Nothing -> False
      AssignmentLock aid ->
        case Ix.getOne (Ix.getEQ aid (doc ^. #assignments)) of
          Just a -> user.id `elem` a.studentIds -- Only assignments assigned to them
          Nothing -> False
      CompetenceAssessmentLock aid ->
        case Ix.getOne (Ix.getEQ aid (doc ^. #competenceAssessments)) of
          Just a -> user.id == a.userId -- Only assessments about them
          Nothing -> False
      CompetenceGridGradeLock gid ->
        case Ix.getOne (Ix.getEQ gid (doc ^. #competenceGridGrades)) of
          Just g -> user.id == g.userId -- Only grid grades about them
          Nothing -> False
      _ -> True -- Other locks (competence, grid, etc.) are visible (public materials)

-- ============================================================================
-- STALENESS COMPUTATION
-- ============================================================================

-- | Check if a competence assessment is stale.
-- An assessment is stale if there is any evidence for this (user, competence)
-- pair that has a date newer than the assessment date.
isCompetenceAssessmentStale :: Document -> CompetenceAssessment -> Bool
isCompetenceAssessmentStale doc assessment =
  let -- Get evidences for this user that are newer than the assessment date
      -- Uses both UserId and Day indexes for efficient filtering
      newerUserEvidences = Ix.toList $ (doc.evidences Ix.@= assessment.userId) Ix.@> assessment.date
      -- Check if any have an observation for this competence
   in any (hasObservationForCompetence assessment.competenceId) newerUserEvidences

-- | Helper: check if an evidence has an observation for the given competence
hasObservationForCompetence :: CompetenceId -> Evidence -> Bool
hasObservationForCompetence competenceId evidence =
  any (\obs -> fst obs.competenceLevelId == competenceId) (Ix.toList evidence.observations)

-- | Get the active (most recent) assessment for a student/competence pair.
-- Returns Nothing if no assessment exists.
getActiveAssessment :: Document -> UserId -> CompetenceId -> Maybe CompetenceAssessment
getActiveAssessment doc userId competenceId =
  listToMaybe $ getAssessmentHistory doc userId competenceId

-- | Get all assessments for a student/competence pair, sorted by date descending.
-- The first element (if any) is the active assessment.
getAssessmentHistory :: Document -> UserId -> CompetenceId -> [CompetenceAssessment]
getAssessmentHistory doc userId competenceId =
  sortOn (Down . (.date)) $
    filter (\a -> a.competenceId == competenceId) $
      Ix.toList (doc.competenceAssessments Ix.@= userId)

-- ============================================================================
-- STALENESS COMPUTATION FOR COMPETENCE GRID GRADES
-- ============================================================================

-- | Check if a competence grid grade is stale.
-- A grid grade is stale if:
-- - Any competence assessment for this (user, grid) is updated after the grid grade date
-- - Any competence assessment for this (user, grid) is itself stale
isCompetenceGridGradeStale :: Document -> CompetenceGridGrade -> Bool
isCompetenceGridGradeStale doc gridGrade =
  let -- Get all competences in this grid
      gridCompetences = Ix.toList $ doc.competences Ix.@= gridGrade.competenceGridId
      competenceIds = map (.id) gridCompetences
      -- Get all assessments for this user that are newer than the grid grade
      newerUserAssessments = Ix.toList $ (doc.competenceAssessments Ix.@= gridGrade.userId) Ix.@> gridGrade.date
      -- Check if any newer assessment is for a competence in this grid
      hasNewerAssessmentInGrid = any (\a -> a.competenceId `elem` competenceIds) newerUserAssessments
      -- Get all assessments for this user in this grid (any date)
      allUserAssessments = Ix.toList $ doc.competenceAssessments Ix.@= gridGrade.userId
      assessmentsInGrid = filter (\a -> a.competenceId `elem` competenceIds) allUserAssessments
      -- Check if any assessment in this grid is itself stale
      hasStaleAssessmentInGrid = any (isCompetenceAssessmentStale doc) assessmentsInGrid
   in hasNewerAssessmentInGrid || hasStaleAssessmentInGrid

-- | Get the active (most recent) grid grade for a student/grid pair.
-- Returns Nothing if no grade exists.
getActiveGridGrade :: Document -> UserId -> CompetenceGridId -> Maybe CompetenceGridGrade
getActiveGridGrade doc userId gridId =
  listToMaybe $ getGridGradeHistory doc userId gridId

-- | Get all grid grades for a student/grid pair, sorted by date descending.
-- The first element (if any) is the active grade.
getGridGradeHistory :: Document -> UserId -> CompetenceGridId -> [CompetenceGridGrade]
getGridGradeHistory doc userId gridId =
  sortOn (Down . (.date)) $
    filter (\g -> g.competenceGridId == gridId) $
      Ix.toList (doc.competenceGridGrades Ix.@= userId)