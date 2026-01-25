{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.CompetenceGridParser
-- Description : Parser for competence grid import format
--
-- Parses the markdown-like competence grid import format:
--
-- @
-- # Grid Title
--
-- ## Competence description (Ersetzt: Original description)
-- - Wesentlich: Level description
-- - Mittelstufe: Level description
-- - Fortgeschritten: Level description
-- @
module Competences.Import.CompetenceGridParser
  ( -- * Parsing
    parseGridImport
  , ParseError
  )
where

import Competences.Document.Competence (Level (..))
import Competences.Import.Types
  ( ParsedCompetence (..)
  , ParsedGrid (..)
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
  , satisfy
  , skipSpace
  , skipWhile
  , string
  , takeWhile1
  )
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse error message
type ParseError = String

-- | Parse competence grid import format
--
-- Parses one or more grids from the input text.
-- Each grid starts with a # heading and contains ## competences.
parseGridImport :: Text -> Either ParseError [ParsedGrid]
parseGridImport input
  | T.null (T.strip input) = Right []
  | otherwise = parseOnly (gridsP <* endOfInput) input

-- | Parse multiple grids
gridsP :: Parser [ParsedGrid]
gridsP = do
  skipBlankLines
  grids <- many' gridP
  skipBlankLines
  pure grids

-- | Parse a single grid (# heading followed by ## competences)
gridP :: Parser ParsedGrid
gridP = do
  title <- h1P
  skipBlankLines
  competences <- many' competenceP
  pure
    ParsedGrid
      { title = title
      , competences = competences
      }

-- | Parse # heading (grid title)
h1P :: Parser Text
h1P = do
  _ <- char '#'
  skipHorizontalSpace
  title <- takeLineContent
  skipBlankLines
  pure (T.strip title)

-- | Parse ## heading with optional (Ersetzt: ...) clause
h2P :: Parser (Text, Maybe Text)
h2P = do
  _ <- string "##"
  skipHorizontalSpace
  content <- takeLineContent
  let (desc, replaces) = parseReplacesClause (T.strip content)
  skipBlankLines
  pure (desc, replaces)

-- | Parse a single competence (## heading + level items)
competenceP :: Parser ParsedCompetence
competenceP = do
  (desc, replaces) <- h2P
  levels <- levelsP
  skipBlankLines
  pure
    ParsedCompetence
      { description = desc
      , replacesDescription = replaces
      , levels = levels
      }

-- | Parse level items (- Wesentlich: ..., etc.)
levelsP :: Parser (Map Level Text)
levelsP = do
  items <- many' levelItemP
  pure (Map.fromList items)

-- | Parse a single level item: - LevelName: Description
levelItemP :: Parser (Level, Text)
levelItemP = do
  skipHorizontalSpace
  _ <- char '-'
  skipHorizontalSpace
  levelName <- takeWhile1 (\c -> c /= ':' && c /= '\n')
  _ <- char ':'
  skipHorizontalSpace
  desc <- takeLineContent
  -- Collect continuation lines (indented)
  continuations <- many' continuationLine
  let fullDesc = T.intercalate " " (T.strip desc : map T.strip continuations)
  skipBlankLines
  case levelFromGerman (T.strip levelName) of
    Just level -> pure (level, fullDesc)
    Nothing -> fail $ "Unknown level: " <> T.unpack levelName

-- | Parse continuation line (starts with whitespace, not - or #)
continuationLine :: Parser Text
continuationLine = do
  skipHorizontalSpace
  c <- satisfy (\x -> not (isSpace x) && x /= '-' && x /= '#')
  rest <- takeLineContent
  pure (T.cons c rest)

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Parse (Ersetzt: original) clause from description
-- Returns (new description, Maybe original)
parseReplacesClause :: Text -> (Text, Maybe Text)
parseReplacesClause input =
  case T.breakOn "(Ersetzt:" input of
    (before, after)
      | T.null after -> (input, Nothing)
      | otherwise ->
          let -- Remove "(Ersetzt:" prefix and trailing ")"
              original = T.strip $ T.dropEnd 1 $ T.drop 9 after
           in (T.strip before, Just original)

-- | Take content until end of line
takeLineContent :: Parser Text
takeLineContent = do
  content <- many' (satisfy (\c -> c /= '\n' && c /= '\r'))
  option () (endOfLine *> pure ())
  pure (T.pack content)

-- | Skip horizontal whitespace (spaces and tabs, not newlines)
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (\c -> c == ' ' || c == '\t')

-- | Skip blank lines and whitespace
skipBlankLines :: Parser ()
skipBlankLines = skipSpace
