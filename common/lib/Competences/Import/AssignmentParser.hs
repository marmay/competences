-- |
-- Module      : Competences.Import.AssignmentParser
-- Description : Parser for assignment import format with embedded tasks
--
-- Parses the markdown assignment import format via two-stage parsing:
-- first parse as markdown, then extract structured data from the AST.
--
-- @
-- # Assignment Name (Ersetzt: Original Name)
--
-- ## Beschreibung
-- Assignment description text...
--
-- ## Angaben
-- Date: 2026-01-25
-- Type: HomeExercise
--
-- ### Task-Identifier-1 (Ersetzt: Original ID)
--
-- #### Angabe
-- Task content...
--
-- #### Kompetenzen
-- - GridName / CompetenceDesc / Wesentlich
--
-- #### Hinweis
-- Hint solution...
--
-- #### Ergebnis
-- Result solution...
--
-- #### Komplettlösung
-- Complete solution...
-- @
module Competences.Import.AssignmentParser
  ( -- * Parsing
    parseAssignmentImport
  , ParseError
  )
where

import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Competence (Level)
import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Import.ASTExtract
  ( blocksToText
  , bulletListItemTexts
  , groupByHeading
  )
import Competences.Import.ParserUtils (ParseError, parseReplacesClause)
import Competences.Import.Types
  ( ParsedAssignment (..)
  , ParsedSolution (..)
  , ParsedTask (..)
  , activityTypeFromGerman
  , levelFromGerman
  )
import Competences.Markdown.AST (Block (..), Document (..))
import Competences.Markdown.Parser (parseMarkdown)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)

-- | Parse assignment import format
--
-- Parses one or more assignments from the input text.
-- Each assignment starts with a # heading.
parseAssignmentImport :: Text -> Either ParseError [ParsedAssignment]
parseAssignmentImport input
  | T.null (T.strip input) = Right []
  | otherwise = case parseMarkdown input of
      Left err -> Left (show err)
      Right (Document blocks) ->
        Right $ map parseAssignment (groupByHeading 1 blocks)

-- | Extract a ParsedAssignment from a heading-1 section
parseAssignment :: (Text, [Block]) -> ParsedAssignment
parseAssignment (headingText, blocks) =
  let (name, replacesName) = parseReplacesClause headingText
      -- Split blocks into ## sections (before ###) and ### tasks
      (sectionBlocks, taskBlocks) = splitAtHeading 3 blocks
      sections = groupByHeading 2 sectionBlocks
      description = fromMaybe "" $ findSection "Beschreibung" sections
      angaben = fromMaybe "" $ findSection "Angaben" sections
      (date, actType) = parseAngaben angaben
      tasks = map parseAssignmentTask (groupByHeading 3 taskBlocks)
   in ParsedAssignment
        { name = name
        , replacesName = replacesName
        , description = description
        , assignmentDate = date
        , activityType = actType
        , tasks = tasks
        }

-- | Split blocks into those before the first heading of the given level, and the rest
splitAtHeading :: Int -> [Block] -> ([Block], [Block])
splitAtHeading level = span (not . isHeadingOfLevel)
  where
    isHeadingOfLevel (Heading n _) = n == level
    isHeadingOfLevel _ = False

-- | Extract a ParsedTask from a heading-3 section (within an assignment)
parseAssignmentTask :: (Text, [Block]) -> ParsedTask
parseAssignmentTask (headingText, blocks) =
  let (ident, replaces) = parseReplacesClause headingText
      sections = groupByHeading 4 blocks
      contentSection = findSection "Angabe" sections
      solutions = extractSolutions sections
      competenceRefs = extractCompetences sections
   in ParsedTask
        { identifier = TaskIdentifier ident
        , replacesIdentifier = TaskIdentifier <$> replaces
        , content = fromMaybe "" contentSection
        , solutions = solutions
        , competenceRefs = competenceRefs
        }

-- | Find a section by name and return its content as text
findSection :: Text -> [(Text, [Block])] -> Maybe Text
findSection name sections =
  case filter (\(n, _) -> n == name) sections of
    ((_, blocks) : _) -> Just (blocksToText blocks)
    [] -> Nothing

-- | Parse the ## Angaben section to extract date and type
parseAngaben :: Text -> (Day, ActivityType)
parseAngaben content =
  let lines' = T.lines content
      dateStr = findKeyValue "Date" lines'
      typeStr = findKeyValue "Type" lines'
      date = case dateStr >>= parseDate of
        Just d -> d
        Nothing -> defaultDate
      actType = case typeStr >>= activityTypeFromGerman of
        Just t -> t
        Nothing -> SchoolExercise -- Default
   in (date, actType)
  where
    defaultDate = read "2000-01-01" -- Fallback date

    parseDate :: Text -> Maybe Day
    parseDate txt = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack $ T.strip txt)

    findKeyValue :: Text -> [Text] -> Maybe Text
    findKeyValue key = go
      where
        go [] = Nothing
        go (line : rest) =
          case T.breakOn ":" line of
            (k, v)
              | T.strip k == key -> Just (T.strip $ T.drop 1 v)
              | otherwise -> go rest

-- | Extract solutions from sections
extractSolutions :: [(Text, [Block])] -> [ParsedSolution]
extractSolutions = concatMap toSolution
  where
    toSolution (name, blocks)
      | name == "Hinweis" = [ParsedSolution Hint (blocksToText blocks)]
      | name == "Ergebnis" = [ParsedSolution Results (blocksToText blocks)]
      | name == "Komplettlösung" = [ParsedSolution Complete (blocksToText blocks)]
      | otherwise = []

-- | Extract competence references from #### Kompetenzen section
extractCompetences :: [(Text, [Block])] -> [(Text, Text, Level)]
extractCompetences sections =
  case findSectionBlocks "Kompetenzen" sections of
    Nothing -> []
    Just blocks -> parseCompetenceList blocks

-- | Find a section by name and return its raw blocks
findSectionBlocks :: Text -> [(Text, [Block])] -> Maybe [Block]
findSectionBlocks name sections =
  case filter (\(n, _) -> n == name) sections of
    ((_, blocks) : _) -> Just blocks
    [] -> Nothing

-- | Parse competence list from blocks containing a BulletList
parseCompetenceList :: [Block] -> [(Text, Text, Level)]
parseCompetenceList blocks =
  let itemTexts = concatMap getBulletItemTexts blocks
   in mapMaybe parseCompetenceLine itemTexts

-- | Extract bullet list item texts from a block
getBulletItemTexts :: Block -> [Text]
getBulletItemTexts (BulletList items) = bulletListItemTexts items
getBulletItemTexts _ = []

-- | Parse a single competence line: "Grid / Description / Level"
parseCompetenceLine :: Text -> Maybe (Text, Text, Level)
parseCompetenceLine line =
  let parts = T.splitOn "/" (T.strip line)
   in case parts of
        [grid, desc, levelText] ->
          case levelFromGerman (T.strip levelText) of
            Just level -> Just (T.strip grid, T.strip desc, level)
            Nothing -> Nothing
        _ -> Nothing
