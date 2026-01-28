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
import Competences.Document.Order (Order, orderMin)
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..), TaskType (..), defaultTaskAttributes)
import Competences.Import.Types
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
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
                      , dateFrom = existing.dateFrom
                      , dateTo = existing.dateTo
                      , expectedLessons = existing.expectedLessons
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
    , dateFrom = Nothing
    , dateTo = Nothing
    , expectedLessons = Nothing
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

-- | Match a single parsed task
matchSingleTask :: Document -> ParsedTask -> TaskImportPreview
matchSingleTask doc parsed =
  let -- Try to find existing task by identifier
      existingTask = findTaskByIdentifier doc parsed.identifier parsed.replacesIdentifier

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
   in TaskImportPreview
        { taskAction = taskAction
        , solutionActions = solutionActions
        , competenceMatches = competenceMatches
        }

-- | Find task by identifier, checking both current and replacement identifiers
findTaskByIdentifier :: Document -> TaskIdentifier -> Maybe TaskIdentifier -> Maybe Task
findTaskByIdentifier doc ident mReplaces =
  let byReplaces = case mReplaces of
        Just origIdent -> Ix.getOne $ doc.tasks Ix.@= origIdent
        Nothing -> Nothing
      byIdent = Ix.getOne $ doc.tasks Ix.@= ident
   in byReplaces <|> byIdent
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing x = x
    (<|>) x _ = x

-- | Create new task from parsed data
makeNewTask :: ParsedTask -> Task
makeNewTask parsed =
  Task
    { id = Id UUID.nil
    , identifier = parsed.identifier
    , content = if T.null parsed.content then Nothing else Just parsed.content
    , taskType = SelfContained defaultTaskAttributes
    }

-- | Update existing task with parsed data
updateTask :: Task -> ParsedTask -> Task
updateTask existing parsed =
  Task
    { id = existing.id
    , identifier = parsed.identifier
    , content = if T.null parsed.content then Nothing else Just parsed.content
    , taskType = existing.taskType
    }

-- | Check if two tasks are equal
taskEquals :: Task -> Task -> Bool
taskEquals a b =
  a.identifier == b.identifier
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
    , content = parsed.content
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
  let -- Try to find existing assignment by name
      existingAssignment = findAssignmentByName doc parsed.name parsed.replacesName

      assignmentAction = case existingAssignment of
        Nothing -> Create (makeNewAssignment parsed)
        Just existing ->
          let updated = updateAssignment existing parsed
           in if assignmentEquals existing updated
                then NoChange existing
                else Update existing updated

      -- Match embedded tasks
      taskPreviews = map (matchSingleTask doc) parsed.tasks
   in AssignmentImportPreview
        { assignmentAction = assignmentAction
        , taskPreviews = taskPreviews
        }

-- | Find assignment by name, checking both current and replacement names
findAssignmentByName :: Document -> Text -> Maybe Text -> Maybe Assignment
findAssignmentByName doc name mReplaces =
  let byReplaces = case mReplaces of
        Just origName ->
          find (\a -> let AssignmentName n = a.name in normalizeText n == normalizeText origName) $
            Ix.toList doc.assignments
        Nothing -> Nothing
      byName =
        find (\a -> let AssignmentName n = a.name in normalizeText n == normalizeText name) $
          Ix.toList doc.assignments
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
    , description = parsed.description
    , assignmentDate = parsed.assignmentDate
    , activityType = parsed.activityType
    , studentIds = Set.empty -- Will be set during import
    , tasks = [] -- Will be filled with task IDs after tasks are created
    }

-- | Update existing assignment with parsed data
updateAssignment :: Assignment -> ParsedAssignment -> Assignment
updateAssignment existing parsed =
  Assignment
    { id = existing.id
    , name = AssignmentName parsed.name
    , description = parsed.description
    , assignmentDate = parsed.assignmentDate
    , activityType = parsed.activityType
    , studentIds = existing.studentIds -- Preserve students
    , tasks = existing.tasks -- Will be updated with new task IDs after matching
    }

-- | Check if two assignments are equal (for detecting changes)
assignmentEquals :: Assignment -> Assignment -> Bool
assignmentEquals a b =
  a.name == b.name
    && a.description == b.description
    && a.assignmentDate == b.assignmentDate
    && a.activityType == b.activityType

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Normalize text for comparison (trim whitespace, lowercase)
normalizeText :: Text -> Text
normalizeText = T.toLower . T.strip
