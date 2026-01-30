{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.Export
-- Description : Export competence grids and assignments to import format
--
-- Exports entities to the same markdown-like format used by the import parsers,
-- enabling round-trip compatibility for AI-assisted editing workflows.
module Competences.Import.Export
  ( -- * Grid Export
    exportCompetenceGrid

    -- * Assignment Export
  , exportAssignment
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Competence
  ( Competence (..)
  , CompetenceLevelId
  , Level (..)
  , LevelInfo (..)
  , allLevels
  )
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Order (Order)
import Competences.Document.Solution (Solution (..), SolutionType (..))
import Competences.Document.Task
  ( Task (..)
  , TaskAttributes (..)
  , TaskAttributesOverride (..)
  , TaskId
  , TaskIdentifier (..)
  , TaskType (..)
  )
import Competences.Import.Types (activityTypeToGerman, levelToGerman)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Competences.TaskContent.RichContent (toRawText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)

-- ============================================================================
-- Grid Export
-- ============================================================================

-- | Export a competence grid to markdown format
--
-- Format:
-- @
-- # Grid Title
--
-- ## Competence Description
-- - Wesentlich: Level description
-- - Mittelstufe: Level description
-- - Fortgeschritten: Level description
-- @
exportCompetenceGrid :: Document -> CompetenceGrid -> Text
exportCompetenceGrid doc grid =
  let competences =
        Ix.toAscList (Proxy @Order) $
          doc.competences Ix.@= grid.id
      header = "# " <> grid.title <> "\n"
      competenceLines = T.intercalate "\n" $ map exportCompetence competences
   in header <> "\n" <> competenceLines

-- | Export a single competence
exportCompetence :: Competence -> Text
exportCompetence c =
  let header = "## " <> c.description <> "\n"
      levelLines = T.intercalate "\n" $ mapMaybe (exportLevel c.levels) allLevels
   in header <> levelLines <> "\n"

-- | Export a level (only if it has a description)
exportLevel :: Map.Map Level LevelInfo -> Level -> Maybe Text
exportLevel levels lvl =
  case Map.lookup lvl levels of
    Just info
      | not (T.null info.description) ->
          Just $ "- " <> levelToGerman lvl <> ": " <> T.strip info.description
    _ -> Nothing

-- ============================================================================
-- Assignment Export
-- ============================================================================

-- | Export an assignment with embedded tasks to markdown format
--
-- Format:
-- @
-- # Assignment Name
--
-- ## Beschreibung
-- Assignment description...
--
-- ## Angaben
-- Date: 2026-01-25
-- Type: Hausübung
--
-- ### Task-Identifier
--
-- #### Angabe
-- Task content...
--
-- #### Kompetenzen
-- - GridName / CompetenceDesc / Wesentlich
--
-- #### Hinweis
-- Hint solution...
-- @
exportAssignment :: Document -> Assignment -> Text
exportAssignment doc assignment =
  let AssignmentName name = assignment.name
      header = "# " <> name <> "\n"
      descSection = "\n## Beschreibung\n" <> T.strip (toRawText assignment.description) <> "\n"
      metaSection =
        "\n## Angaben\n"
          <> "Date: "
          <> formatDay assignment.assignmentDate
          <> "\n"
          <> "Type: "
          <> activityTypeToGerman assignment.activityType
          <> "\n"
      tasks = sortBy (comparing (.identifier)) $ mapMaybe (lookupTask doc) assignment.tasks
      taskSections = T.intercalate "\n" $ map (exportTaskAsSubsection doc) tasks
   in header <> descSection <> metaSection <> "\n" <> taskSections

-- | Format a Day as ISO date string
formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- | Look up a task by ID
lookupTask :: Document -> TaskId -> Maybe Task
lookupTask doc tid = Ix.getOne $ doc.tasks Ix.@= tid

-- | Export a task as a subsection (### level) within an assignment
exportTaskAsSubsection :: Document -> Task -> Text
exportTaskAsSubsection doc task =
  let TaskIdentifier ident = task.identifier
      header = "### " <> ident <> "\n"
      contentSection = case task.content of
        Just c | let raw = toRawText c, not (T.null (T.strip raw)) -> "\n#### Angabe\n" <> T.strip raw <> "\n"
        _ -> ""
      -- Get competence references for this task
      competenceSection = exportTaskCompetences doc task
      -- Get solutions for this task
      solutions = Ix.toList $ doc.solutions Ix.@= task.id
      solutionSections = T.concat $ map exportSolution solutions
   in header <> contentSection <> competenceSection <> solutionSections

-- | Export competence references for a task
-- Note: This requires looking up the competence and grid from the stored IDs
exportTaskCompetences :: Document -> Task -> Text
exportTaskCompetences doc task =
  let -- Get primary competences from task attributes
      competenceIds = getTaskCompetenceIds task
      refs = mapMaybe (formatCompetenceRef doc) competenceIds
   in if null refs
        then ""
        else "\n#### Kompetenzen\n" <> T.intercalate "\n" refs <> "\n"

-- | Get competence level IDs from a task
getTaskCompetenceIds :: Task -> [CompetenceLevelId]
getTaskCompetenceIds task = case task.taskType of
  SelfContained attrs -> attrs.primary <> attrs.secondary
  SubTask _ override ->
    -- For subtasks, we only export the override values (not inherited ones)
    -- Since we don't have access to the group here, just export what we can
    maybe [] id override.primary <> maybe [] id override.secondary

-- | Format a competence reference as "GridName / Description / Level"
formatCompetenceRef :: Document -> CompetenceLevelId -> Maybe Text
formatCompetenceRef doc (compId, level) = do
  comp <- Ix.getOne $ doc.competences Ix.@= compId
  grid <- Ix.getOne $ doc.competenceGrids Ix.@= comp.competenceGridId
  pure $ "- " <> grid.title <> " / " <> comp.description <> " / " <> levelToGerman level

-- | Export a solution
exportSolution :: Solution -> Text
exportSolution sol =
  let sectionName = case sol.solutionType of
        Hint -> "Hinweis"
        Results -> "Ergebnis"
        Complete -> "Komplettlösung"
      raw = toRawText sol.content
   in if T.null (T.strip raw)
        then ""
        else "\n#### " <> sectionName <> "\n" <> T.strip raw <> "\n"
