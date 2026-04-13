{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.Matching
-- Description : Match parsed imports against document and generate previews
--
-- This module provides functions to match parsed import data against
-- the current document state and generate preview/diff information.
module Competences.Import.Matching
  ( -- * Grid Matching
    matchGridImport

    -- * Task Matching
  , matchTaskImport
  , matchSingleTask

    -- * Assignment Matching
  , matchAssignmentImport

    -- * Resource Matching
  , matchResourceImport

    -- * Lesson Matching
  , matchLessonImport

    -- * Utilities
  , normalizeText
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Competence
  ( Competence (..)
  , Level (..)
  , LevelInfo (..)
  )
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Lesson (Lesson (..), LessonPhase (..))
import Competences.Document.MesoPlan (MesoPlanId)
import Competences.Document.Order (Order, orderMax, orderMin)
import Competences.Document.Resource (Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..), defaultTask)
import Optics.Core ((&), (.~))
import Competences.Import.Types
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Competences.TaskContent.RichContent (fromTrustedInput)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID.Types qualified as UUID

-- ============================================================================
-- Grid Matching
-- ============================================================================

-- | Match parsed grids against document and produce import previews
matchGridImport :: Document -> [ParsedGrid] -> [GridImportPreview]
matchGridImport doc = map (matchSingleGrid doc)

-- | Match a single parsed grid
matchSingleGrid :: Document -> ParsedGrid -> GridImportPreview
matchSingleGrid doc parsed =
  let -- Try to find existing grid by title
      existingGrid = findGridByTitle doc parsed.title
      gridAction = case existingGrid of
        Nothing -> Create (makeNewGrid parsed)
        Just existing
          | existing.title == parsed.title -> NoChange existing
          | otherwise ->
              let updated =
                    CompetenceGrid
                      { id = existing.id
                      , order = existing.order
                      , title = parsed.title
                      , description = existing.description
                      }
               in Update existing updated

      -- Get grid ID (existing or placeholder for new)
      gridId = case existingGrid of
        Just g -> g.id
        Nothing -> Id UUID.nil -- Placeholder, will be generated on actual import

      -- Match competences within this grid
      existingCompetences =
        Ix.toAscList (Proxy @Order) $
          doc.competences Ix.@= gridId
      (competenceActions, matchedIds) = matchCompetencesWithTracking existingCompetences parsed.competences

      -- Find competences that exist but weren't matched (to be deleted)
      toDelete = filter (\c -> c.id `notElem` matchedIds) existingCompetences
   in GridImportPreview
        { gridAction = gridAction
        , competenceActions = competenceActions
        , competencesToDelete = toDelete
        }

-- | Find grid by title (case-insensitive)
findGridByTitle :: Document -> Text -> Maybe CompetenceGrid
findGridByTitle doc title =
  find (\g -> normalizeText g.title == normalizeText title) $
    Ix.toList doc.competenceGrids

-- | Create a new grid from parsed data
makeNewGrid :: ParsedGrid -> CompetenceGrid
makeNewGrid parsed =
  CompetenceGrid
    { id = Id UUID.nil -- Placeholder
    , order = orderMin -- Will be assigned on import
    , title = parsed.title
    , description = ""
    }

-- | Match parsed competences against existing ones, tracking which were matched
matchCompetencesWithTracking :: [Competence] -> [ParsedCompetence] -> ([CompetenceImportAction], [Id Competence])
matchCompetencesWithTracking existing parsed =
  let results = map (matchSingleCompetence existing) parsed
      matchedIds = mapMaybe getMatchedId results
   in (results, matchedIds)
  where
    getMatchedId :: CompetenceImportAction -> Maybe (Id Competence)
    getMatchedId ca = case ca.action of
      Create _ -> Nothing
      Update old _ -> Just old.id
      NoChange c -> Just c.id

-- | Match a single parsed competence
matchSingleCompetence :: [Competence] -> ParsedCompetence -> CompetenceImportAction
matchSingleCompetence existing parsed =
  let -- First try to match by replacesDescription (explicit rename)
      matchByReplace = case parsed.replacesDescription of
        Just origDesc ->
          find (\c -> normalizeText c.description == normalizeText origDesc) existing
        Nothing -> Nothing

      -- Fall back to matching by current description
      matchByDesc =
        find (\c -> normalizeText c.description == normalizeText parsed.description) existing

      match = matchByReplace <|> matchByDesc

      action = case match of
        Nothing -> Create (makeNewCompetence parsed)
        Just existingComp ->
          let updated = updateCompetence existingComp parsed
           in if competenceEquals existingComp updated
                then NoChange existingComp
                else Update existingComp updated
   in CompetenceImportAction
        { action = action
        , parsedCompetence = parsed
        }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new competence from parsed data
makeNewCompetence :: ParsedCompetence -> Competence
makeNewCompetence parsed =
  Competence
    { id = Id UUID.nil -- Placeholder
    , competenceGridId = Id UUID.nil -- Will be set on import
    , order = orderMin -- Will be assigned
    , description = parsed.description
    , levels = Map.map (\desc -> LevelInfo{description = desc, locked = False}) parsed.levels
    }

-- | Update existing competence with parsed data
updateCompetence :: Competence -> ParsedCompetence -> Competence
updateCompetence existing parsed =
  Competence
    { id = existing.id
    , competenceGridId = existing.competenceGridId
    , order = existing.order
    , description = parsed.description
    , levels = mergeLevels existing.levels parsed.levels
    }

-- | Merge level updates into existing levels
-- New values from import take precedence, but we preserve existing levels not in import
mergeLevels :: Map Level LevelInfo -> Map Level Text -> Map Level LevelInfo
mergeLevels existing updates =
  let updatedLevelInfos = Map.map (\desc -> LevelInfo{description = desc, locked = False}) updates
   in Map.unionWith (\_ new -> new) existing updatedLevelInfos

-- | Check if two competences are equal (for detecting changes)
competenceEquals :: Competence -> Competence -> Bool
competenceEquals a b =
  a.description == b.description
    && Map.map (.description) a.levels == Map.map (.description) b.levels

-- ============================================================================
-- Task Matching
-- ============================================================================

-- | Match parsed tasks against document and produce import previews
matchTaskImport :: Document -> [ParsedTask] -> [TaskImportPreview]
matchTaskImport doc = map (matchSingleTask doc)

-- | Match a single parsed task (always searches published tasks)
matchSingleTask :: Document -> ParsedTask -> TaskImportPreview
matchSingleTask doc = matchSingleTaskForDraft doc False

-- | Match a single parsed task, searching the correct collection based on isDraft
matchSingleTaskForDraft :: Document -> Bool -> ParsedTask -> TaskImportPreview
matchSingleTaskForDraft doc isDraft parsed =
  let -- Try to find existing task by identifier in the correct collection
      existingTask = findTaskByIdentifier doc isDraft parsed.identifier parsed.replacesIdentifier

      taskAction = case existingTask of
        Nothing -> Create (makeNewTask parsed)
        Just existing ->
          let updated = updateTask existing parsed
           in if taskEquals existing updated
                then NoChange existing
                else Update existing updated

      -- Get task ID for solution matching
      taskId = case existingTask of
        Just t -> t.id
        Nothing -> Id UUID.nil

      -- Match solutions
      existingSolutions = Ix.toList $ doc.solutions Ix.@= taskId
      solutionActions = matchSolutions existingSolutions parsed.solutions

      -- Match competences (search across all grids)
      competenceMatches = matchCompetenceRefs doc parsed.competenceRefs
      secondaryCompetenceMatches = matchCompetenceRefs doc parsed.secondaryCompetenceRefs
   in TaskImportPreview
        { taskAction = taskAction
        , solutionActions = solutionActions
        , competenceMatches = competenceMatches
        , secondaryCompetenceMatches = secondaryCompetenceMatches
        , parsedPurpose = parsed.purpose
        }

-- | Find task by identifier, checking both current and replacement identifiers.
-- When isDraft, searches draft tasks; otherwise searches published tasks.
findTaskByIdentifier :: Document -> Bool -> TaskIdentifier -> Maybe TaskIdentifier -> Maybe Task
findTaskByIdentifier doc isDraft ident mReplaces =
  let collection = if isDraft then doc.draftTasks else doc.tasks
      byReplaces = case mReplaces of
        Just origIdent -> Ix.getOne $ collection Ix.@= origIdent
        Nothing -> Nothing
      byIdent = Ix.getOne $ collection Ix.@= ident
   in byReplaces <|> byIdent
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new task from parsed data
makeNewTask :: ParsedTask -> Task
makeNewTask parsed =
  defaultTask (Id UUID.nil)
    & #identifier .~ parsed.identifier
    & #title .~ parsed.title
    & #content .~ (if T.null parsed.content then Nothing else Just (fromTrustedInput parsed.content))

-- | Update existing task with parsed data
updateTask :: Task -> ParsedTask -> Task
updateTask existing parsed =
  existing
    & #identifier .~ parsed.identifier
    & #title .~ parsed.title
    & #content .~ (if T.null parsed.content then Nothing else Just (fromTrustedInput parsed.content))

-- | Check if two tasks are equal
taskEquals :: Task -> Task -> Bool
taskEquals a b =
  a.identifier == b.identifier
    && a.title == b.title
    && a.content == b.content

-- | Match solutions (simplified for now)
matchSolutions :: [Solution] -> [ParsedSolution] -> [ImportAction Solution]
matchSolutions _existing parsed =
  -- For now, just create all parsed solutions
  -- A more sophisticated implementation would match by type
  map (\p -> Create (makeNewSolution p)) parsed

-- | Create new solution from parsed data
makeNewSolution :: ParsedSolution -> Solution
makeNewSolution parsed =
  Solution
    { id = Id UUID.nil
    , taskId = Id UUID.nil -- Will be set during import
    , userId = Id UUID.nil -- Will be set to teacher
    , solutionType = parsed.solutionType
    , content = fromTrustedInput parsed.content
    }

-- | Match competence references against grids
matchCompetenceRefs :: Document -> [(Text, Text, Level)] -> [CompetenceMatch]
matchCompetenceRefs doc = map (matchCompetenceRef doc)

-- | Match a single competence reference by grid, then description
matchCompetenceRef :: Document -> (Text, Text, Level) -> CompetenceMatch
matchCompetenceRef doc (gridName, desc, level) =
  let -- First find the grid by title
      mGrid = find (\g -> normalizeText g.title == normalizeText gridName) $
              Ix.toList doc.competenceGrids
      -- Then find competence in that grid
      match = case mGrid of
        Nothing -> Nothing
        Just grid ->
          find (\c -> c.competenceGridId == grid.id &&
                      normalizeText c.description == normalizeText desc) $
          Ix.toList doc.competences
      matchedId = case match of
        Just c -> Just (c.id, level)
        Nothing -> Nothing
   in CompetenceMatch
        { gridName = gridName
        , description = desc
        , level = level
        , matched = matchedId
        }

-- ============================================================================
-- Assignment Matching
-- ============================================================================

-- | Match parsed assignments against document and produce import previews
matchAssignmentImport :: Document -> [ParsedAssignment] -> [AssignmentImportPreview]
matchAssignmentImport doc = map (matchSingleAssignment doc)

-- | Match a single parsed assignment
matchSingleAssignment :: Document -> ParsedAssignment -> AssignmentImportPreview
matchSingleAssignment doc parsed =
  let -- Try to find existing assignment by name (in correct collection)
      existingAssignment = findAssignmentByName doc parsed.isDraft parsed.name parsed.replacesName

      assignmentAction = case existingAssignment of
        Nothing -> Create (makeNewAssignment parsed)
        Just existing ->
          let updated = updateAssignment existing parsed
           in if assignmentEquals existing updated
                then NoChange existing
                else Update existing updated

      -- Match embedded tasks (in correct collection)
      taskPreviews = map (matchSingleTaskForDraft doc parsed.isDraft) parsed.tasks
   in AssignmentImportPreview
        { assignmentAction = assignmentAction
        , taskPreviews = taskPreviews
        , isDraft = parsed.isDraft
        }

-- | Find assignment by name, checking both current and replacement names
-- When isDraft, searches draft assignments; otherwise searches published.
findAssignmentByName :: Document -> Bool -> Text -> Maybe Text -> Maybe Assignment
findAssignmentByName doc isDraft name mReplaces =
  let collection = if isDraft then doc.draftAssignments else doc.assignments
      byReplaces = case mReplaces of
        Just origName ->
          find (\a -> let AssignmentName n = a.name in normalizeText n == normalizeText origName) $
            Ix.toList collection
        Nothing -> Nothing
      byName =
        find (\a -> let AssignmentName n = a.name in normalizeText n == normalizeText name) $
          Ix.toList collection
   in byReplaces <|> byName
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new assignment from parsed data
makeNewAssignment :: ParsedAssignment -> Assignment
makeNewAssignment parsed =
  Assignment
    { id = Id UUID.nil -- Placeholder
    , name = AssignmentName parsed.name
    , description = fromTrustedInput parsed.description
    , assignmentDate = parsed.assignmentDate
    , activityType = parsed.activityType
    , studentIds = Set.empty -- Will be set during import
    , tasks = [] -- Will be filled with task IDs after tasks are created
    , groupSubmissionAllowed = False
    }

-- | Update existing assignment with parsed data
updateAssignment :: Assignment -> ParsedAssignment -> Assignment
updateAssignment existing parsed =
  Assignment
    { id = existing.id
    , name = AssignmentName parsed.name
    , description = fromTrustedInput parsed.description
    , assignmentDate = parsed.assignmentDate
    , activityType = parsed.activityType
    , studentIds = existing.studentIds -- Preserve students
    , tasks = existing.tasks -- Will be updated with new task IDs after matching
    , groupSubmissionAllowed = existing.groupSubmissionAllowed
    }

-- | Check if two assignments are equal (for detecting changes)
assignmentEquals :: Assignment -> Assignment -> Bool
assignmentEquals a b =
  a.name == b.name
    && a.description == b.description
    && a.assignmentDate == b.assignmentDate
    && a.activityType == b.activityType

-- ============================================================================
-- Resource Matching
-- ============================================================================

-- | Match parsed resources against document and produce import previews
matchResourceImport :: Document -> [ParsedResource] -> [ResourceImportPreview]
matchResourceImport doc = map (matchSingleResource doc)

-- | Match a single parsed resource
matchSingleResource :: Document -> ParsedResource -> ResourceImportPreview
matchSingleResource doc parsed =
  let existingResource = findResourceByIdentifier doc parsed.identifier parsed.replacesIdentifier

      resourceAction = case existingResource of
        Nothing -> Create (makeNewResource parsed)
        Just existing ->
          let updated = updateResource existing parsed
           in if resourceEquals existing updated
                then NoChange existing
                else Update existing updated

      competenceMatches = matchCompetenceRefs doc parsed.competenceRefs
   in ResourceImportPreview
        { resourceAction = resourceAction
        , competenceMatches = competenceMatches
        }

-- | Find resource by identifier, checking both current and replacement identifiers
findResourceByIdentifier :: Document -> Text -> Maybe Text -> Maybe Resource
findResourceByIdentifier doc ident mReplaces =
  let byReplaces = case mReplaces of
        Just origIdent ->
          find (\r -> let ResourceIdentifier ri = r.identifier in normalizeText ri == normalizeText origIdent) $
            Ix.toList doc.resources
        Nothing -> Nothing
      byIdent =
        find (\r -> let ResourceIdentifier ri = r.identifier in normalizeText ri == normalizeText ident) $
          Ix.toList doc.resources
   in byReplaces <|> byIdent
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new resource from parsed data
makeNewResource :: ParsedResource -> Resource
makeNewResource parsed =
  Resource
    { id = Id UUID.nil
    , identifier = ResourceIdentifier parsed.identifier
    , competenceLevels = [] -- Will be filled from matched competences
    , content = InlineContent (fromTrustedInput parsed.content)
    , attachments = []
    }

-- | Update existing resource with parsed data
updateResource :: Resource -> ParsedResource -> Resource
updateResource existing parsed =
  Resource
    { id = existing.id
    , identifier = ResourceIdentifier parsed.identifier
    , competenceLevels = existing.competenceLevels -- Preserved, will be updated from matches
    , content = if T.null parsed.content then existing.content else InlineContent (fromTrustedInput parsed.content)
    , attachments = existing.attachments
    }

-- | Check if two resources are equal (for detecting changes)
resourceEquals :: Resource -> Resource -> Bool
resourceEquals a b =
  a.identifier == b.identifier
    && a.content == b.content

-- ============================================================================
-- Lesson Matching
-- ============================================================================

-- | Match parsed lessons against document and produce import previews.
-- Lessons are matched within a specific MesoPlan.
matchLessonImport :: Document -> MesoPlanId -> [ParsedLesson] -> [LessonImportPreview]
matchLessonImport doc mesoPlanId = map (matchSingleLesson doc mesoPlanId)

-- | Match a single parsed lesson
matchSingleLesson :: Document -> MesoPlanId -> ParsedLesson -> LessonImportPreview
matchSingleLesson doc mesoPlanId parsed =
  let -- Find existing lesson by title within the same meso plan
      existingLesson = findLessonByTitle doc mesoPlanId parsed.title parsed.replacesTitle

      lessonAction = case existingLesson of
        Nothing -> Create (makeNewLesson mesoPlanId parsed)
        Just existing ->
          let updated = updateLesson existing parsed
           in if lessonEquals existing updated
                then NoChange existing
                else Update existing updated

      competenceMatches = matchCompetenceRefs doc parsed.competenceRefs
   in LessonImportPreview
        { lessonAction = lessonAction
        , competenceMatches = competenceMatches
        , parsedPhases = parsed.phases
        }

-- | Find lesson by title within a meso plan, checking both current and replacement titles
findLessonByTitle :: Document -> MesoPlanId -> Text -> Maybe Text -> Maybe Lesson
findLessonByTitle doc mesoPlanId title mReplaces =
  let lessonsInPlan = Ix.toList $ doc.lessons Ix.@= mesoPlanId
      byReplaces = case mReplaces of
        Just origTitle ->
          find (\l -> normalizeText l.title == normalizeText origTitle) lessonsInPlan
        Nothing -> Nothing
      byTitle =
        find (\l -> normalizeText l.title == normalizeText title) lessonsInPlan
   in byReplaces <|> byTitle
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new lesson from parsed data
makeNewLesson :: MesoPlanId -> ParsedLesson -> Lesson
makeNewLesson mesoPlanId parsed =
  Lesson
    { id = Id UUID.nil
    , mesoPlanId = mesoPlanId
    , order = orderMax
    , title = parsed.title
    , description = fromTrustedInput parsed.description
    , competenceLevels = [] -- Will be filled from matched competences
    , date = parsed.date
    , assignments = []
    , resources = []
    , phases = map toDomainPhase parsed.phases
    , notes = fromTrustedInput parsed.notes
    }

-- | Update existing lesson with parsed data
updateLesson :: Lesson -> ParsedLesson -> Lesson
updateLesson existing parsed =
  Lesson
    { id = existing.id
    , mesoPlanId = existing.mesoPlanId
    , order = existing.order
    , title = parsed.title
    , description = if T.null parsed.description then existing.description else fromTrustedInput parsed.description
    , competenceLevels = existing.competenceLevels -- Will be updated from matches
    , date = case parsed.date of
        Just d -> Just d
        Nothing -> existing.date
    , assignments = existing.assignments -- Preserved, will be matched separately
    , resources = existing.resources -- Preserved, will be matched separately
    , phases = if null parsed.phases then existing.phases else map toDomainPhase parsed.phases
    , notes = if T.null parsed.notes then existing.notes else fromTrustedInput parsed.notes
    }

-- | Convert parsed phase to domain phase
toDomainPhase :: ParsedLessonPhase -> LessonPhase
toDomainPhase p =
  LessonPhase
    { title = p.title
    , socialForm = p.socialForm
    , duration = p.duration
    , actionForm = p.actionForm
    , notes = fromTrustedInput p.notes
    }

-- | Check if two lessons are equal (for detecting changes)
lessonEquals :: Lesson -> Lesson -> Bool
lessonEquals a b =
  a.title == b.title
    && a.description == b.description
    && a.date == b.date
    && a.phases == b.phases
    && a.notes == b.notes

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Normalize text for comparison (trim whitespace, lowercase)
normalizeText :: Text -> Text
normalizeText = T.toLower . T.strip
