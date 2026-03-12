{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Competences.Document
  ( Document (..)
  , emptyDocument
  , projectDocument
  , module Competences.Document.Assessment
  , module Competences.Document.CompetenceGridGrade
  , module Competences.Document.Grade
  , module Competences.Document.Lock
  , module Competences.Document.Competence
  , module Competences.Document.CompetenceGrid
  , module Competences.Document.Evidence
  , module Competences.Document.FileRef
  , module Competences.Document.Order
  , module Competences.Document.Resource
  , module Competences.Document.Solution
  , module Competences.Document.Task
  , module Competences.Document.Assignment
  , module Competences.Document.User
  , module Competences.Document.MesoPlan
  , module Competences.Document.Lesson
  , module Competences.Document.LessonNotes
  , module Competences.Document.ParticipationRecord
  , module Competences.Document.Absence
  , module Competences.Document.Submission
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Absence (Absence (..), AbsenceId, AbsenceIxs)
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
  , getLevelInfo
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
import Competences.Document.FileRef (FileData (..), FileRef (..), SHA256Hash (..), sha256HashToText, sha256HashFromText)
import Competences.Document.Lesson
  ( Lesson (..)
  , LessonId
  , LessonIxs
  , LessonPhase (..)
  , TeachingSocialForm (..)
  , ActionForm (..)
  )
import Competences.Document.LessonNotes
  ( LessonNoteItem (..)
  , LessonNotes (..)
  , LessonNotesId
  , LessonNotesIxs
  , mkLessonNotes
  )
import Competences.Document.MesoPlan
  ( MesoPlan (..)
  , MesoPlanId
  , MesoPlanIxs
  )
import Competences.Document.ParticipationRecord
  ( ParticipationRecord (..)
  , ParticipationRecordId
  , ParticipationRecordIxs
  , ParticipationType (..)
  )
import Competences.Document.Grade (Grade (..), grades, gradeToText)
import Competences.Document.Lock (Lock (..))
import Competences.Document.Order (Order, orderAt, orderMax, orderMin, ordered)
import Competences.Document.Resource
  ( Resource (..)
  , ResourceContent (..)
  , ResourceId
  , ResourceIdentifier (..)
  , ResourceIxs
  , mkResource
  )
import Competences.Document.Solution (Solution (..), SolutionId, SolutionIxs, SolutionType (..))
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionIxs, ownerIds)
import Competences.Document.Task (Task (..), TaskId, TaskIxs, TaskGroup (..), TaskGroupId, TaskGroupIxs, TaskType (..))
import Competences.Document.User (User (..), UserId, UserIxs, UserRole (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
#endif
import Data.Binary (Binary)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Optics.Core ((&), (.~))

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
  , solutions :: !(Ix.IxSet SolutionIxs Solution)
  , mesoPlans :: !(Ix.IxSet MesoPlanIxs MesoPlan)
  , lessons :: !(Ix.IxSet LessonIxs Lesson)
  , lessonNotes :: !(Ix.IxSet LessonNotesIxs LessonNotes)
  , participationRecords :: !(Ix.IxSet ParticipationRecordIxs ParticipationRecord)
  , absences :: !(Ix.IxSet AbsenceIxs Absence)
  , submissions :: !(Ix.IxSet SubmissionIxs Submission)
  , draftTasks :: !(Ix.IxSet TaskIxs Task)
  , draftTaskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , draftAssignments :: !(Ix.IxSet AssignmentIxs Assignment)
  }
  deriving (Eq, Generic, Show)

instance Binary Document

#ifdef WITH_AESON
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
      <*> fmap Ix.fromList (v .:? "solutions" .!= [])
      <*> fmap Ix.fromList (v .:? "mesoPlans" .!= [])
      <*> fmap Ix.fromList (v .:? "lessons" .!= [])
      <*> fmap Ix.fromList (v .:? "lessonNotes" .!= [])
      <*> fmap Ix.fromList (v .:? "participationRecords" .!= [])
      <*> fmap Ix.fromList (v .:? "absences" .!= [])
      <*> fmap Ix.fromList (v .:? "submissions" .!= [])
      <*> fmap Ix.fromList (v .:? "draftTasks" .!= [])
      <*> fmap Ix.fromList (v .:? "draftTaskGroups" .!= [])
      <*> fmap Ix.fromList (v .:? "draftAssignments" .!= [])

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
      , "solutions" .= Ix.toList d.solutions
      , "mesoPlans" .= Ix.toList d.mesoPlans
      , "lessons" .= Ix.toList d.lessons
      , "lessonNotes" .= Ix.toList d.lessonNotes
      , "participationRecords" .= Ix.toList d.participationRecords
      , "absences" .= Ix.toList d.absences
      , "submissions" .= Ix.toList d.submissions
      , "draftTasks" .= Ix.toList d.draftTasks
      , "draftTaskGroups" .= Ix.toList d.draftTaskGroups
      , "draftAssignments" .= Ix.toList d.draftAssignments
      ]
#endif

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
    , solutions = Ix.empty
    , mesoPlans = Ix.empty
    , lessons = Ix.empty
    , lessonNotes = Ix.empty
    , participationRecords = Ix.empty
    , absences = Ix.empty
    , submissions = Ix.empty
    , draftTasks = Ix.empty
    , draftTaskGroups = Ix.empty
    , draftAssignments = Ix.empty
    }


-- | Project document based on user identity for access control
-- Teachers see full document, students see filtered view
projectDocument :: User -> Document -> Document
projectDocument user doc
  | user.role == Teacher = doc -- Teachers see everything
  | otherwise =
      -- Students see filtered view based on their identity
      doc
        & #evidences .~ (doc.evidences Ix.@= user.id) -- Only evidences about them (via UserId index)
        & #assignments .~ (doc.assignments Ix.@= user.id) -- Only assignments assigned to them (via UserId index)
        & #competenceAssessments .~ (doc.competenceAssessments Ix.@= user.id) -- Only assessments about them
        & #competenceGridGrades .~ (doc.competenceGridGrades Ix.@= user.id) -- Only grid grades about them
        & #locks .~ M.filterWithKey isLockVisible doc.locks -- Only locks on entities they can see
        & #mesoPlans .~ Ix.empty -- Planning is teacher-only
        & #lessons .~ Ix.empty
        & #participationRecords .~ (doc.participationRecords Ix.@= user.id) -- Own records only
        & #absences .~ (doc.absences Ix.@= user.id) -- Own absences only
        & #submissions .~ (doc.submissions Ix.@= user.id) -- Own submissions only
        & #draftTasks .~ Ix.empty -- Drafts are teacher-only
        & #draftTaskGroups .~ Ix.empty
        & #draftAssignments .~ Ix.empty
        -- competenceGrids, competences, resources, lessonNotes, tasks, taskGroups: students see all (public materials)
  where
    -- Student can see locks on entities they have access to
    isLockVisible lock _ = case lock of
      UserLock uid -> uid == user.id -- Only their own user
      EvidenceLock eid ->
        case Ix.getOne (doc.evidences Ix.@= eid) of
          Just e -> Just user.id == e.userId -- Only evidences about them
          Nothing -> False
      AssignmentLock aid ->
        case Ix.getOne (doc.assignments Ix.@= aid) of
          Just a -> user.id `elem` a.studentIds -- Only assignments assigned to them
          Nothing -> False
      CompetenceAssessmentLock aid ->
        case Ix.getOne (doc.competenceAssessments Ix.@= aid) of
          Just a -> user.id == a.userId -- Only assessments about them
          Nothing -> False
      CompetenceGridGradeLock gid ->
        case Ix.getOne (doc.competenceGridGrades Ix.@= gid) of
          Just g -> user.id == g.userId -- Only grid grades about them
          Nothing -> False
      SolutionLock _ -> True -- Solutions are visible to all users
      ResourceLock _ -> True -- Resources are visible to all users
      LessonNotesLock _ -> True -- Lesson notes are visible to all users
      MesoPlanLock _ -> False -- Planning is teacher-only
      LessonLock _ -> False
      ParticipationRecordLock prid ->
        case Ix.getOne (doc.participationRecords Ix.@= prid) of
          Just pr -> user.id == pr.userId
          Nothing -> False
      AbsenceLock aid ->
        case Ix.getOne (doc.absences Ix.@= aid) of
          Just a -> user.id == a.userId
          Nothing -> False
      SubmissionLock sid ->
        case Ix.getOne (doc.submissions Ix.@= sid) of
          Just s -> user.id `elem` ownerIds s.ownership
          Nothing -> False
      _ -> True -- Other locks (competence, grid, etc.) are visible (public materials)