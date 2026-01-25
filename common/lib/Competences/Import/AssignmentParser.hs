{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.AssignmentParser
-- Description : Parser for assignment import format with embedded tasks
--
-- Parses the markdown-like assignment import format:
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
import Competences.Document.Competence (Level (..))
import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Import.Types
  ( ParsedAssignment (..)
  , ParsedSolution (..)
  , ParsedTask (..)
  , activityTypeFromGerman
  , levelFromGerman
  )
import Data.Attoparsec.Text
  ( Parser
  , char
  , endOfInput
  , endOfLine
  , many'
  , option
  , parseOnly
  , peekChar
  , satisfy
  , skipSpace
  , skipWhile
  , string
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)

-- | Parse error message
type ParseError = String

-- | Parse assignment import format
--
-- Parses one or more assignments from the input text.
-- Each assignment starts with a # heading.
parseAssignmentImport :: Text -> Either ParseError [ParsedAssignment]
parseAssignmentImport input
  | T.null (T.strip input) = Right []
  | otherwise = parseOnly (assignmentsP <* endOfInput) input

-- | Parse multiple assignments
assignmentsP :: Parser [ParsedAssignment]
assignmentsP = do
  skipBlankLines
  assignments <- many' assignmentP
  skipBlankLines
  pure assignments

-- | Parse a single assignment
assignmentP :: Parser ParsedAssignment
assignmentP = do
  (name, replacesName) <- assignmentHeaderP
  sections <- many' assignmentSectionP
  tasks <- many' taskP
  skipBlankLines

  -- Extract sections
  let description = fromMaybe "" $ findSection "Beschreibung" sections
      angaben = fromMaybe "" $ findSection "Angaben" sections
      (date, actType) = parseAngaben angaben

  pure
    ParsedAssignment
      { name = name
      , replacesName = replacesName
      , description = description
      , assignmentDate = date
      , activityType = actType
      , tasks = tasks
      }

-- | Parse assignment header: # Name (Ersetzt: Original)
assignmentHeaderP :: Parser (Text, Maybe Text)
assignmentHeaderP = do
  _ <- char '#'
  skipHorizontalSpace
  content <- takeLineContent
  let (name, replaces) = parseReplacesClause (T.strip content)
  skipBlankLines
  pure (name, replaces)

-- | A parsed section (## Heading + content)
data Section = Section
  { sectionName :: !Text
  , sectionContent :: !Text
  }
  deriving (Show)

-- | Parse a ## section (stops at ### or next #)
assignmentSectionP :: Parser Section
assignmentSectionP = do
  _ <- string "##"
  mc <- peekChar
  -- Don't parse ### (task headers) as ## sections
  case mc of
    Just '#' -> fail "task header"
    _ -> pure ()
  skipHorizontalSpace
  name <- takeLineContent
  skipBlankLines
  content <- sectionContentP
  pure Section{sectionName = T.strip name, sectionContent = content}

-- | Parse section content until next # or ## or ### or end
sectionContentP :: Parser Text
sectionContentP = do
  lines' <- many' contentLineP
  pure (T.intercalate "\n" lines')

-- | Parse a content line (not starting with #)
contentLineP :: Parser Text
contentLineP = do
  mc <- peekChar
  case mc of
    Just '#' -> fail "section boundary"
    Nothing -> fail "end of input"
    _ -> takeLineContent

-- | Find section by name
findSection :: Text -> [Section] -> Maybe Text
findSection name sections =
  case filter (\s -> s.sectionName == name) sections of
    (s : _) -> Just s.sectionContent
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

-- ============================================================================
-- Task Parsing (### level)
-- ============================================================================

-- | Parse a single task (### heading + #### sections)
taskP :: Parser ParsedTask
taskP = do
  (ident, replaces) <- taskHeaderP
  sections <- many' taskSectionP
  skipBlankLines

  -- Extract sections
  let contentSection = findSection "Angabe" sections
      solutions = extractSolutions sections
      competenceRefs = extractCompetences sections

  pure
    ParsedTask
      { identifier = TaskIdentifier ident
      , replacesIdentifier = TaskIdentifier <$> replaces
      , content = fromMaybe "" contentSection
      , solutions = solutions
      , competenceRefs = competenceRefs
      }

-- | Parse task header: ### Identifier (Ersetzt: Original)
taskHeaderP :: Parser (Text, Maybe Text)
taskHeaderP = do
  _ <- string "###"
  skipHorizontalSpace
  content <- takeLineContent
  let (ident, replaces) = parseReplacesClause (T.strip content)
  skipBlankLines
  pure (ident, replaces)

-- | Parse a #### section within a task
taskSectionP :: Parser Section
taskSectionP = do
  _ <- string "####"
  skipHorizontalSpace
  name <- takeLineContent
  skipBlankLines
  content <- taskSectionContentP
  pure Section{sectionName = T.strip name, sectionContent = content}

-- | Parse task section content (until next #### or ### or # or end)
taskSectionContentP :: Parser Text
taskSectionContentP = do
  lines' <- many' taskContentLineP
  pure (T.intercalate "\n" lines')

-- | Parse a content line (not starting with # or ##)
taskContentLineP :: Parser Text
taskContentLineP = do
  mc <- peekChar
  case mc of
    Just '#' -> fail "section boundary"
    Nothing -> fail "end of input"
    _ -> takeLineContent

-- | Extract solutions from sections
extractSolutions :: [Section] -> [ParsedSolution]
extractSolutions = concatMap toSolution
  where
    toSolution s
      | s.sectionName == "Hinweis" =
          [ParsedSolution Hint s.sectionContent]
      | s.sectionName == "Ergebnis" =
          [ParsedSolution Results s.sectionContent]
      | s.sectionName == "Komplettlösung" =
          [ParsedSolution Complete s.sectionContent]
      | otherwise = []

-- | Extract competence references from #### Kompetenzen section
extractCompetences :: [Section] -> [(Text, Text, Level)]
extractCompetences sections =
  case findSection "Kompetenzen" sections of
    Nothing -> []
    Just content -> parseCompetenceList content

-- | Parse competence list:
-- - Grid / Description / Level
parseCompetenceList :: Text -> [(Text, Text, Level)]
parseCompetenceList content =
  mapMaybe parseCompetenceLine (T.lines content)
  where
    parseCompetenceLine line =
      let stripped = T.strip $ T.dropWhile (== '-') $ T.strip line
          parts = T.splitOn "/" stripped
       in case parts of
            [grid, desc, levelText] ->
              case levelFromGerman (T.strip levelText) of
                Just level -> Just (T.strip grid, T.strip desc, level)
                Nothing -> Nothing
            _ -> Nothing

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Parse (Ersetzt: original) clause
parseReplacesClause :: Text -> (Text, Maybe Text)
parseReplacesClause input =
  case T.breakOn "(Ersetzt:" input of
    (before, after)
      | T.null after -> (input, Nothing)
      | otherwise ->
          let original = T.strip $ T.dropEnd 1 $ T.drop 9 after
           in (T.strip before, Just original)

-- | Take content until end of line
takeLineContent :: Parser Text
takeLineContent = do
  content <- many' (satisfy (\c -> c /= '\n' && c /= '\r'))
  option () (endOfLine >> pure ())
  pure (T.pack content)

-- | Skip horizontal whitespace
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (\c -> c == ' ' || c == '\t')

-- | Skip blank lines
skipBlankLines :: Parser ()
skipBlankLines = skipSpace
