{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.TaskParser
-- Description : Parser for task import format
--
-- Parses the markdown-like task import format:
--
-- @
-- # TaskIdentifier (Ersetzt: Original Identifier)
--
-- ## Angabe
-- Task content in markdown dialect...
--
-- ## Hinweis
-- Hint content...
--
-- ## Ergebnis
-- Results content...
--
-- ## Komplettlösung
-- Complete solution...
--
-- ## Kompetenzen
-- - Competence description / Wesentlich
-- - Another competence / Fortgeschritten
-- @
module Competences.Import.TaskParser
  ( -- * Parsing
    parseTaskImport
  , ParseError
  )
where

import Competences.Document.Competence (Level (..))
import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Import.Types
  ( ParsedSolution (..)
  , ParsedTask (..)
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
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse error message
type ParseError = String

-- | Parse task import format
--
-- Parses one or more tasks from the input text.
-- Each task starts with a # heading.
parseTaskImport :: Text -> Either ParseError [ParsedTask]
parseTaskImport input
  | T.null (T.strip input) = Right []
  | otherwise = parseOnly (tasksP <* endOfInput) input

-- | Parse multiple tasks
tasksP :: Parser [ParsedTask]
tasksP = do
  skipBlankLines
  tasks <- many' taskP
  skipBlankLines
  pure tasks

-- | Parse a single task
taskP :: Parser ParsedTask
taskP = do
  (ident, replaces) <- taskHeaderP
  sections <- many' sectionP
  skipBlankLines

  -- Extract sections by type
  let contentSection = findSection "Angabe" sections
      solutions = extractSolutions sections
      competenceRefs = extractCompetences sections

  pure
    ParsedTask
      { identifier = TaskIdentifier ident
      , replacesIdentifier = TaskIdentifier <$> replaces
      , content = maybe "" id contentSection
      , solutions = solutions
      , competenceRefs = competenceRefs
      }

-- | Parse task header: # Identifier (Ersetzt: Original)
taskHeaderP :: Parser (Text, Maybe Text)
taskHeaderP = do
  _ <- char '#'
  skipHorizontalSpace
  content <- takeLineContent
  let (ident, replaces) = parseReplacesClause (T.strip content)
  skipBlankLines
  pure (ident, replaces)

-- | A parsed section (## Heading + content)
data Section = Section
  { sectionName :: !Text
  , sectionContent :: !Text
  }
  deriving (Show)

-- | Parse a ## section
sectionP :: Parser Section
sectionP = do
  _ <- string "##"
  skipHorizontalSpace
  name <- takeLineContent
  skipBlankLines
  content <- sectionContentP
  pure Section{sectionName = T.strip name, sectionContent = content}

-- | Parse section content until next # or ## or end
sectionContentP :: Parser Text
sectionContentP = do
  lines' <- many' contentLineP
  pure (T.intercalate "\n" lines')

-- | Parse a content line (not starting with # or ##)
contentLineP :: Parser Text
contentLineP = do
  -- Peek to check if we're at a heading or end of input
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

-- | Extract solutions from sections
extractSolutions :: [Section] -> [ParsedSolution]
extractSolutions sections = concatMap toSolution sections
  where
    toSolution s
      | s.sectionName == "Hinweis" =
          [ParsedSolution Hint s.sectionContent]
      | s.sectionName == "Ergebnis" =
          [ParsedSolution Results s.sectionContent]
      | s.sectionName == "Komplettlösung" =
          [ParsedSolution Complete s.sectionContent]
      | otherwise = []

-- | Extract competence references from ## Kompetenzen section
extractCompetences :: [Section] -> [(Text, Text, Level)]
extractCompetences sections =
  case findSection "Kompetenzen" sections of
    Nothing -> []
    Just content -> parseCompetenceList content

-- | Parse competence list:
-- - Grid / Description / Level
-- - Another Grid / Another Description / Level
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
  option () (endOfLine *> pure ())
  pure (T.pack content)

-- | Skip horizontal whitespace
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (\c -> c == ' ' || c == '\t')

-- | Skip blank lines
skipBlankLines :: Parser ()
skipBlankLines = skipSpace
