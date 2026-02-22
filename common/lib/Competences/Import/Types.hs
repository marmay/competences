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

    -- * Parsed Resource Types
  , ParsedResource (..)

    -- * Parsed Lesson Types
  , ParsedLesson (..)
  , ParsedLessonPhase (..)

    -- * Import Actions (Diff/Preview)
  , ImportAction (..)
  , GridImportPreview (..)
  , CompetenceImportAction (..)
  , TaskImportPreview (..)
  , CompetenceMatch (..)
  , AssignmentImportPreview (..)
  , ResourceImportPreview (..)
  , LessonImportPreview (..)

    -- * Utilities
  , levelFromGerman
  , levelToGerman
  , activityTypeFromGerman
  , activityTypeToGerman
  , socialFormFromGerman
  , socialFormToGerman
  , actionFormFromGerman
  , actionFormToGerman
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
import Competences.Document.Lesson (ActionForm (..), Lesson, TeachingSocialForm (..))
import Competences.Document.Resource (Resource)
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

-- | Preview of resource import changes
data ResourceImportPreview = ResourceImportPreview
  { resourceAction :: !(ImportAction Resource)
  -- ^ Action for the resource itself
  , competenceMatches :: ![CompetenceMatch]
  -- ^ Competence matching results
  }
  deriving (Eq, Show, Generic)

-- | Preview of lesson import changes
data LessonImportPreview = LessonImportPreview
  { lessonAction :: !(ImportAction Lesson)
  -- ^ Action for the lesson itself
  , competenceMatches :: ![CompetenceMatch]
  -- ^ Competence matching results
  , parsedPhases :: ![ParsedLessonPhase]
  -- ^ Parsed phases for preview display
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Parsed Resource Types
-- ============================================================================

-- | A parsed resource from import format
data ParsedResource = ParsedResource
  { identifier :: !Text
  -- ^ Resource identifier (from # heading)
  , replacesIdentifier :: !(Maybe Text)
  -- ^ Original identifier if renaming (from "Ersetzt:" clause)
  , content :: !Text
  -- ^ Inline content (from ## Inhalt section)
  , competenceRefs :: ![(Text, Text, Level)]
  -- ^ Competence references (gridName, description, level)
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Parsed Lesson Types
-- ============================================================================

-- | A parsed lesson from import format
data ParsedLesson = ParsedLesson
  { title :: !Text
  -- ^ Lesson title (from # heading)
  , replacesTitle :: !(Maybe Text)
  -- ^ Original title if renaming (from "Ersetzt:" clause)
  , description :: !Text
  -- ^ Description (from ## Beschreibung section)
  , date :: !(Maybe Day)
  -- ^ Date (from ## Angaben section)
  , competenceRefs :: ![(Text, Text, Level)]
  -- ^ Competence references (from ## Kompetenzen section)
  , resourceIdentifiers :: ![Text]
  -- ^ Resource identifiers (from ## Materialien section)
  , assignmentNames :: ![Text]
  -- ^ Assignment names (from ## Aufgaben section)
  , phases :: ![ParsedLessonPhase]
  -- ^ Lesson phases (from ## Phasen section)
  , notes :: !Text
  -- ^ Notes (from ## Notizen section)
  }
  deriving (Eq, Show, Generic)

-- | A parsed lesson phase
data ParsedLessonPhase = ParsedLessonPhase
  { title :: !Text
  , socialForm :: !TeachingSocialForm
  , actionForm :: !ActionForm
  , duration :: !Int
  -- ^ Duration in minutes
  , notes :: !Text
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

-- ============================================================================
-- Social Form Utilities
-- ============================================================================

-- | Parse German social form name
socialFormFromGerman :: Text -> Maybe TeachingSocialForm
socialFormFromGerman "Plenum" = Just WholeClass
socialFormFromGerman "Gruppenarbeit" = Just SmallGroups
socialFormFromGerman "Partnerarbeit" = Just PairWork
socialFormFromGerman "Einzelarbeit" = Just IndividualWork
socialFormFromGerman _ = Nothing

-- | Convert TeachingSocialForm to German name
socialFormToGerman :: TeachingSocialForm -> Text
socialFormToGerman WholeClass = "Plenum"
socialFormToGerman SmallGroups = "Gruppenarbeit"
socialFormToGerman PairWork = "Partnerarbeit"
socialFormToGerman IndividualWork = "Einzelarbeit"

-- ============================================================================
-- Action Form Utilities
-- ============================================================================

-- | Parse German action form name
actionFormFromGerman :: Text -> Maybe ActionForm
actionFormFromGerman "Darbietend" = Just Presenting
actionFormFromGerman "Zusammenwirkend" = Just Collaborating
actionFormFromGerman "Aufgebend" = Just Assigning
actionFormFromGerman _ = Nothing

-- | Convert ActionForm to German name
actionFormToGerman :: ActionForm -> Text
actionFormToGerman Presenting = "Darbietend"
actionFormToGerman Collaborating = "Zusammenwirkend"
actionFormToGerman Assigning = "Aufgebend"
