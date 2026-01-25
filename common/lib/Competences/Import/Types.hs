{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.Types
-- Description : Types for bulk import of competence grids, tasks, and assignments
--
-- This module defines the intermediate types used for parsing import formats
-- and generating diff previews before applying changes.
module Competences.Import.Types
  ( -- * Parsed Grid Types
    ParsedGrid (..)
  , ParsedCompetence (..)

    -- * Parsed Task Types
  , ParsedTask (..)
  , ParsedSolution (..)

    -- * Parsed Assignment Types
  , ParsedAssignment (..)

    -- * Import Actions (Diff/Preview)
  , ImportAction (..)
  , GridImportPreview (..)
  , CompetenceImportAction (..)
  , TaskImportPreview (..)
  , CompetenceMatch (..)
  , AssignmentImportPreview (..)

    -- * Utilities
  , levelFromGerman
  , levelToGerman
  , activityTypeFromGerman
  , activityTypeToGerman
  )
where

import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (Assignment)
import Competences.Document.Competence
  ( Competence
  , CompetenceLevelId
  , Level (..)
  )
import Competences.Document.CompetenceGrid (CompetenceGrid)
import Competences.Document.Solution (Solution, SolutionType (..))
import Competences.Document.Task (Task, TaskIdentifier (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- ============================================================================
-- Parsed Grid Types
-- ============================================================================

-- | A parsed competence grid from import format
data ParsedGrid = ParsedGrid
  { title :: !Text
  -- ^ Grid title (from # heading)
  , competences :: ![ParsedCompetence]
  -- ^ List of competences in this grid
  }
  deriving (Eq, Show, Generic)

-- | A parsed competence from import format
data ParsedCompetence = ParsedCompetence
  { description :: !Text
  -- ^ Competence description (from ## heading)
  , replacesDescription :: !(Maybe Text)
  -- ^ Original description if renaming (from "Ersetzt:" clause)
  , levels :: !(Map Level Text)
  -- ^ Level descriptions (Wesentlich, Mittelstufe, Fortgeschritten)
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Parsed Task Types
-- ============================================================================

-- | A parsed task from import format
data ParsedTask = ParsedTask
  { identifier :: !TaskIdentifier
  -- ^ Task identifier (from # heading)
  , replacesIdentifier :: !(Maybe TaskIdentifier)
  -- ^ Original identifier if renaming (from "Ersetzt:" clause)
  , content :: !Text
  -- ^ Task content (from ## Angabe section)
  , solutions :: ![ParsedSolution]
  -- ^ Solutions (Hinweis, Ergebnis, Komplettlösung)
  , competenceRefs :: ![(Text, Text, Level)]
  -- ^ Competence references (gridName, description, level) from ## Kompetenzen
  }
  deriving (Eq, Show, Generic)

-- | A parsed solution from import format
data ParsedSolution = ParsedSolution
  { solutionType :: !SolutionType
  -- ^ Type of solution (Hint, Results, Complete)
  , content :: !Text
  -- ^ Solution content
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Parsed Assignment Types
-- ============================================================================

-- | A parsed assignment from import format
-- Assignments contain embedded task definitions
data ParsedAssignment = ParsedAssignment
  { name :: !Text
  -- ^ Assignment name (from # heading)
  , replacesName :: !(Maybe Text)
  -- ^ Original name if renaming (from "Ersetzt:" clause)
  , description :: !Text
  -- ^ Assignment description (from ## Beschreibung section)
  , assignmentDate :: !Day
  -- ^ Date of the assignment (from ## Angaben section)
  , activityType :: !ActivityType
  -- ^ Type of activity (from ## Angaben section)
  , tasks :: ![ParsedTask]
  -- ^ Embedded task definitions (from ### headings)
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Import Actions (Diff/Preview)
-- ============================================================================

-- | An action to take during import
data ImportAction a
  = -- | Create a new entity
    Create !a
  | -- | Update an existing entity (old, new)
    Update !a !a
  | -- | Entity exists and is unchanged
    NoChange !a
  deriving (Eq, Show, Generic)

-- | Preview of grid import changes
data GridImportPreview = GridImportPreview
  { gridAction :: !(ImportAction CompetenceGrid)
  -- ^ Action for the grid itself
  , competenceActions :: ![CompetenceImportAction]
  -- ^ Actions for each competence in the grid
  , competencesToDelete :: ![Competence]
  -- ^ Existing competences not in import (will be deleted)
  }
  deriving (Eq, Show, Generic)

-- | Action for a single competence during import
data CompetenceImportAction = CompetenceImportAction
  { action :: !(ImportAction Competence)
  -- ^ The action (Create/Update/NoChange)
  , parsedCompetence :: !ParsedCompetence
  -- ^ The parsed data that led to this action
  }
  deriving (Eq, Show, Generic)

-- | Preview of task import changes
data TaskImportPreview = TaskImportPreview
  { taskAction :: !(ImportAction Task)
  -- ^ Action for the task itself
  , solutionActions :: ![ImportAction Solution]
  -- ^ Actions for solutions
  , competenceMatches :: ![CompetenceMatch]
  -- ^ Competence matching results
  }
  deriving (Eq, Show, Generic)

-- | Result of matching a competence reference
data CompetenceMatch = CompetenceMatch
  { gridName :: !Text
  -- ^ The grid name from import
  , description :: !Text
  -- ^ The description text from import
  , level :: !Level
  -- ^ The level from import
  , matched :: !(Maybe CompetenceLevelId)
  -- ^ The matched competence level ID, if found
  }
  deriving (Eq, Show, Generic)

-- | Preview of assignment import changes
data AssignmentImportPreview = AssignmentImportPreview
  { assignmentAction :: !(ImportAction Assignment)
  -- ^ Action for the assignment itself
  , taskPreviews :: ![TaskImportPreview]
  -- ^ Actions for each embedded task
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Level Utilities
-- ============================================================================

-- | Parse German level name to Level
levelFromGerman :: Text -> Maybe Level
levelFromGerman "Wesentlich" = Just BasicLevel
levelFromGerman "Mittelstufe" = Just IntermediateLevel
levelFromGerman "Fortgeschritten" = Just AdvancedLevel
levelFromGerman _ = Nothing

-- | Convert Level to German name
levelToGerman :: Level -> Text
levelToGerman BasicLevel = "Wesentlich"
levelToGerman IntermediateLevel = "Mittelstufe"
levelToGerman AdvancedLevel = "Fortgeschritten"

-- | Parse German activity type name
activityTypeFromGerman :: Text -> Maybe ActivityType
activityTypeFromGerman "Gespräch" = Just Conversation
activityTypeFromGerman "Gespraech" = Just Conversation
activityTypeFromGerman "Prüfung" = Just Exam
activityTypeFromGerman "Pruefung" = Just Exam
activityTypeFromGerman "Schulübung" = Just SchoolExercise
activityTypeFromGerman "Schuluebung" = Just SchoolExercise
activityTypeFromGerman "Hausübung" = Just HomeExercise
activityTypeFromGerman "Hausuebung" = Just HomeExercise
-- Also accept English names for convenience
activityTypeFromGerman "Conversation" = Just Conversation
activityTypeFromGerman "Exam" = Just Exam
activityTypeFromGerman "SchoolExercise" = Just SchoolExercise
activityTypeFromGerman "HomeExercise" = Just HomeExercise
activityTypeFromGerman _ = Nothing

-- | Convert ActivityType to German name
activityTypeToGerman :: ActivityType -> Text
activityTypeToGerman Conversation = "Gespräch"
activityTypeToGerman Exam = "Prüfung"
activityTypeToGerman SchoolExercise = "Schulübung"
activityTypeToGerman HomeExercise = "Hausübung"
