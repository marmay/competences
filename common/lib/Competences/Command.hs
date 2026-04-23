{-# LANGUAGE CPP #-}

module Competences.Command
  ( Command (..)
  , MigrationCommand (..)
  , MigrationPlan (..)
  , CommandId
  , handleCommand
  , validateLessonNotesMigration
  , module Competences.Command.Common
  , module Competences.Command.CompetenceAssessments
  , module Competences.Command.CompetenceGridGrades
  , module Competences.Command.Competences
  , module Competences.Command.Users
  , module Competences.Command.Evidences
  , module Competences.Command.Tasks
  , module Competences.Command.Assignments
  , module Competences.Command.DraftTasks
  , module Competences.Command.DraftAssignments
  , module Competences.Command.Publish
  , module Competences.Command.Solutions
  , module Competences.Command.Resources
  , module Competences.Command.MesoPlans
  , module Competences.Command.Lessons
  , module Competences.Command.LessonNotes
  , module Competences.Command.TeachingNotes
  , module Competences.Command.ParticipationRecords
  , module Competences.Command.Absences
  , module Competences.Command.Submissions
  , module Competences.Command.CompetenceLevelExamples
  , module Competences.Command.Layouts
  )
where

import Competences.Command.Absences (AbsencePatch (..), AbsencesCommand (..), handleAbsencesCommand)
import Competences.Command.CompetenceLevelExamples (CompetenceLevelExamplePatch (..), CompetenceLevelExamplesCommand (..), handleCompetenceLevelExamplesCommand)
import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..), handleAssignmentsCommand)
import Competences.Command.DraftAssignments (DraftAssignmentsCommand (..), handleDraftAssignmentsCommand)
import Competences.Command.DraftTasks (DraftTasksCommand (..), handleDraftTasksCommand)
import Competences.Command.Publish (PublishData (..), handlePublish)
import Competences.Command.Resources (ResourcePatch (..), ResourcesCommand (..), handleResourcesCommand)
import Competences.Command.Common (AffectedUsers (..), CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult, requireTeacher)
import Competences.Command.Solutions (SolutionPatch (..), SolutionsCommand (..), handleSolutionsCommand)
import Competences.Command.Submissions (SubmissionPatch (..), SubmissionsCommand (..), handleSubmissionsCommand)
import Competences.Command.CompetenceAssessments (CompetenceAssessmentPatch (..), CompetenceAssessmentsCommand (..), handleCompetenceAssessmentsCommand)
import Competences.Command.CompetenceGridGrades (CompetenceGridGradePatch (..), CompetenceGridGradesCommand (..), handleCompetenceGridGradesCommand)
import Competences.Command.Competences (CompetenceGridPatch (..), CompetencePatch (..), LevelInfoPatch (..), CompetencesCommand (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Layouts (LayoutsCommand (..), LayoutPatch (..), handleLayoutsCommand)
import Competences.Command.Lessons (LessonsCommand (..), LessonPatch (..), handleLessonsCommand)
import Competences.Command.LessonNotes (LessonNotesCommand (..), LessonNotesPatch (..), handleLessonNotesCommand)
import Competences.Command.TeachingNotes (TeachingNotesCommand (..), handleTeachingNotesCommand)
import Competences.Command.MesoPlans (MesoPlansCommand (..), MesoPlanPatch (..), handleMesoPlansCommand)
import Competences.Command.ParticipationRecords (ParticipationRecordsCommand (..), ParticipationRecordPatch (..), handleParticipationRecordsCommand)
import Competences.Command.Tasks (TasksCommand (..), TaskPatch (..), handleTasksCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), Lesson (..), TeachingNote (..), TeachingNoteId, User (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId, LessonItem (..), LessonItemContent (..), LessonPhase (..))
import Competences.Document.LessonNotes qualified as DN
import Competences.Document.LessonNotes (LessonNoteItem (..))
import Competences.Document.Lock (Lock)
import Competences.Document.User (Office365Id (..), UserRole (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (%~), (^.))

-- | Migration commands for schema evolution and startup initialization
data MigrationCommand
  = UpdateLessonAssignments ![(LessonId, [AssignmentId])]
  | InitIfEmpty
  | EnsureTeacherO365 !(Id User) !Text
  | SortAssignmentTasksByIdentifier
  | MigrateLessonNotesIntoLessons !MigrationPlan
    -- ^ One-shot: (a) fold each 'LessonNotes' into its linked 'Lesson'
    -- (items into @supplementalItems@ with @publish = True@, title into
    -- @notesTitleOverride@); (b) externalise legacy @lesson.notes@ and
    -- @phase.notes@ prose into 'TeachingNote' entities, setting the
    -- corresponding @privateNoteRef@; (c) flip @lessonNotesMigrated@.
    -- The plan supplies pre-allocated 'TeachingNoteId's so the
    -- migration is fully deterministic on replay.
  deriving (Eq, Generic, Show)

-- | Pre-allocated 'TeachingNoteId's for the lesson-and-phase notes
-- externalisation step of 'MigrateLessonNotesIntoLessons'.
data MigrationPlan = MigrationPlan
  { lessonNoteIds :: ![(LessonId, TeachingNoteId)]
  , phaseNoteIds :: ![(LessonId, Int, TeachingNoteId)]
  -- ^ @(lesson, 0-based phase index, allocated id)@.
  }
  deriving (Eq, Generic, Show)

instance Binary MigrationCommand
instance Binary MigrationPlan

#ifdef WITH_AESON
instance FromJSON MigrationCommand

instance ToJSON MigrationCommand

instance FromJSON MigrationPlan

instance ToJSON MigrationPlan
#endif

-- | Top-level command type wrapping all context commands
data Command
  = SetDocument !Document
  | Competences !CompetencesCommand
  | Users !UsersCommand
  | Evidences !EvidencesCommand
  | Tasks !TasksCommand
  | Assignments !AssignmentsCommand
  | CompetenceAssessments !CompetenceAssessmentsCommand
  | CompetenceGridGrades !CompetenceGridGradesCommand
  | Solutions !SolutionsCommand
  | Resources !ResourcesCommand
  | MesoPlans !MesoPlansCommand
  | Lessons !LessonsCommand
  | LessonNotes !LessonNotesCommand
  | TeachingNotes !TeachingNotesCommand
  | ParticipationRecords !ParticipationRecordsCommand
  | Absences !AbsencesCommand
  | Submissions !SubmissionsCommand
  | DraftTasks !DraftTasksCommand
  | DraftAssignments !DraftAssignmentsCommand
  | CompetenceLevelExamples !CompetenceLevelExamplesCommand
  | Layouts !LayoutsCommand
  | Publish !PublishData
  | Migration !MigrationCommand
  | Unlock !Lock
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance Binary Command

#ifdef WITH_AESON
instance FromJSON Command

instance ToJSON Command
#endif

-- | Handle a command and return the updated document with affected users
handleCommand :: CommandContext -> Command -> Document -> UpdateResult
handleCommand cmdCtx cmd d = case cmd of
  SetDocument newDoc ->
    -- Replace entire document, all users affected
    let allUserIds = map (.id) $ Ix.toList $ newDoc ^. #users
     in Right (newDoc, AffectedUsers allUserIds)
  -- Teacher-only commands: require the acting user to be a teacher
  Competences c -> teacherOnly $ handleCompetencesCommand cmdCtx c d
  Users c -> teacherOnly $ handleUsersCommand cmdCtx c d
  Evidences c -> teacherOnly $ handleEvidencesCommand cmdCtx c d
  Tasks c -> teacherOnly $ handleTasksCommand cmdCtx c d
  Assignments c -> teacherOnly $ handleAssignmentsCommand cmdCtx c d
  CompetenceAssessments c -> teacherOnly $ handleCompetenceAssessmentsCommand cmdCtx c d
  CompetenceGridGrades c -> teacherOnly $ handleCompetenceGridGradesCommand cmdCtx c d
  Solutions c -> teacherOnly $ handleSolutionsCommand cmdCtx c d
  Resources c -> teacherOnly $ handleResourcesCommand cmdCtx c d
  MesoPlans c -> teacherOnly $ handleMesoPlansCommand cmdCtx c d
  Lessons c -> teacherOnly $ handleLessonsCommand cmdCtx c d
  LessonNotes c -> teacherOnly $ handleLessonNotesCommand cmdCtx c d
  TeachingNotes c -> teacherOnly $ handleTeachingNotesCommand cmdCtx c d
  ParticipationRecords c -> teacherOnly $ handleParticipationRecordsCommand cmdCtx c d
  Absences c -> teacherOnly $ handleAbsencesCommand cmdCtx c d
  DraftTasks c -> teacherOnly $ handleDraftTasksCommand cmdCtx c d
  DraftAssignments c -> teacherOnly $ handleDraftAssignmentsCommand cmdCtx c d
  CompetenceLevelExamples c -> teacherOnly $ handleCompetenceLevelExamplesCommand cmdCtx c d
  Layouts c -> teacherOnly $ handleLayoutsCommand cmdCtx c d
  -- Student commands: submissions have their own role checks (requires Student)
  Submissions c -> handleSubmissionsCommand cmdCtx c d
  -- System commands: no user role check needed
  Publish pd -> handlePublish pd d
  Migration c -> handleMigrationCommand c d
  -- Lock cleanup: permissive and idempotent — server validates ownership/staleness
  -- before persisting. Safe during replay even if the lock was already released.
  Unlock lock -> Right (d & #locks %~ Map.delete lock, mempty)
  where
    teacherOnly result = requireTeacher cmdCtx.userId d >> result

-- | Handle migration commands (system-level, userId not used)
handleMigrationCommand :: MigrationCommand -> Document -> UpdateResult
handleMigrationCommand (UpdateLessonAssignments updates) d =
  let applyUpdate doc (lid, aids) =
        case Ix.getOne (doc.lessons Ix.@= lid) of
          Nothing -> doc
          Just lesson ->
            let lesson' = lesson & #assignments .~ aids
             in doc & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid
      doc' = foldl' applyUpdate d updates
   in Right (doc', allUsers doc')
handleMigrationCommand InitIfEmpty d
  | Ix.null (d ^. #users) = Right (d, allUsers d)
  | otherwise = Left "Document is not empty"
handleMigrationCommand (EnsureTeacherO365 newId email) d =
  let o365Id = Office365Id email
   in case Ix.getOne (d.users Ix.@= o365Id) of
        Just user
          | user.role == Teacher -> Left "Teacher already exists"
          | otherwise ->
              let user' = user & #role .~ Teacher
                  d' = d & #users %~ Ix.insert user' . Ix.deleteIx user.id
               in Right (d', allUsers d')
        Nothing ->
          let user =
                User
                  { id = newId
                  , name = T.takeWhile (/= '@') email
                  , role = Teacher
                  , office365Id = o365Id
                  }
              d' = d & #users %~ Ix.insert user
           in Right (d', allUsers d')
handleMigrationCommand SortAssignmentTasksByIdentifier d =
  let lookupIdentifier taskSet tid =
        case Ix.getOne (taskSet Ix.@= tid) of
          Just t -> t.identifier
          Nothing -> TaskIdentifier ""
      sortTasks taskSet a =
        a & #tasks %~ sortOn (lookupIdentifier taskSet)
      d' =
        d
          & #assignments %~ Ix.fromList . map (sortTasks d.tasks) . Ix.toList
          & #draftAssignments %~ Ix.fromList . map (sortTasks d.draftTasks) . Ix.toList
   in Right (d', allUsers d')
handleMigrationCommand (MigrateLessonNotesIntoLessons plan) d
  | d.lessonNotesMigrated = Right (d, allUsers d) -- idempotent no-op
  | otherwise =
      case validateLessonNotesMigration d of
        Left reason -> Left reason
        Right linked ->
          let docAfterFold = foldl' mergeOneNote d linked
              docAfterLessonProse = foldl' externaliseLessonNote docAfterFold plan.lessonNoteIds
              docAfterPhaseProse = foldl' externalisePhaseNote docAfterLessonProse plan.phaseNoteIds
              d' = docAfterPhaseProse & #lessonNotesMigrated .~ True
           in Right (d', allUsers d')
  where
    mergeOneNote :: Document -> (LessonId, DN.LessonNotes) -> Document
    mergeOneNote doc (lid, n) =
      case Ix.getOne (doc.lessons Ix.@= lid) of
        Nothing -> doc -- can't happen; prevalidated above
        Just lesson ->
          let newItems = map (\i -> LessonItem {content = toContent i, publish = True}) n.items
              titleOverride =
                if T.null n.title
                  then lesson.notesTitleOverride
                  else Just n.title
              lesson' = lesson
                & #supplementalItems %~ (<> newItems)
                & #notesTitleOverride .~ titleOverride
           in doc & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid

    toContent :: LessonNoteItem -> LessonItemContent
    toContent = \case
      LessonResource rid -> PhaseResource rid
      LessonTask tid -> PhaseTask tid

    externaliseLessonNote :: Document -> (LessonId, TeachingNoteId) -> Document
    externaliseLessonNote doc (lid, tnId) = case Ix.getOne (doc.lessons Ix.@= lid) of
      Nothing -> doc
      Just lesson
        | lesson.notes == mempty -> doc
        | otherwise ->
            let tn = TeachingNote {id = tnId, content = lesson.notes}
                lesson' = lesson
                  & #notes .~ mempty
                  & #privateNoteRef .~ Just tnId
             in doc
                  & #teachingNotes %~ Ix.insert tn
                  & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid

    externalisePhaseNote :: Document -> (LessonId, Int, TeachingNoteId) -> Document
    externalisePhaseNote doc (lid, idx, tnId) = case Ix.getOne (doc.lessons Ix.@= lid) of
      Nothing -> doc
      Just lesson -> case safeIndex idx lesson.phases of
        Nothing -> doc
        Just phase
          | phase.notes == mempty -> doc
          | otherwise ->
              let tn = TeachingNote {id = tnId, content = phase.notes}
                  phase' = phase
                    & #notes .~ mempty
                    & #privateNoteRef .~ Just tnId
                  lesson' = lesson
                    & #phases .~ replaceAt idx phase' lesson.phases
               in doc
                    & #teachingNotes %~ Ix.insert tn
                    & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid

    safeIndex :: Int -> [a] -> Maybe a
    safeIndex i xs
      | i < 0 = Nothing
      | otherwise = case drop i xs of
          [] -> Nothing
          (x : _) -> Just x

    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt _ _ [] = []
    replaceAt 0 v (_ : xs) = v : xs
    replaceAt i v (x : xs) = x : replaceAt (i - 1) v xs

-- | Pure validation used both by 'handleMigrationCommand' and by the
-- startup-time gate in @backend/Main.hs@. Returns the list of valid
-- (lessonId, LessonNotes) pairs to migrate, or a human-readable error
-- listing all issues (orphans, dangling refs, duplicates).
validateLessonNotesMigration :: Document -> Either Text [(LessonId, DN.LessonNotes)]
validateLessonNotesMigration d =
  let notes = Ix.toList d.lessonNotes
      orphans = [ (n.id, n.title) | n <- notes, Nothing <- [n.lessonId] ]
      dangling =
        [ (n.id, n.title, lid)
        | n <- notes
        , Just lid <- [n.lessonId]
        , Ix.null (d.lessons Ix.@= lid)
        ]
      linked = [ (lid, n) | n <- notes, Just lid <- [n.lessonId] ]
      groupedLessons =
        Map.toList $ Map.fromListWith (<>) [(lid, [n.id]) | (lid, n) <- linked]
      duplicates =
        [ (lid, nids) | (lid, nids) <- groupedLessons, length nids > 1 ]
      section label entries
        | null entries = Nothing
        | otherwise = Just (label <> ": " <> T.intercalate ", " entries)
      issues =
        [ section
            "orphans (lessonId = Nothing)"
            [ T.pack (show nid) <> " " <> title | (nid, title) <- orphans ]
        , section
            "dangling lesson references"
            [ T.pack (show nid) <> " " <> title <> " -> " <> T.pack (show lid)
            | (nid, title, lid) <- dangling
            ]
        , section
            "duplicates (multiple notes per lesson)"
            [ T.pack (show lid) <> " <- " <> T.pack (show nids)
            | (lid, nids) <- duplicates
            ]
        ]
   in case [ s | Just s <- issues ] of
        [] -> Right linked
        ss -> Left $ "LessonNotes migration issues: " <> T.intercalate "; " ss

allUsers :: Document -> AffectedUsers
allUsers d = AffectedUsers $ map (.id) $ Ix.toList $ d ^. #users
