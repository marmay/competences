-- |
-- Module      : Competences.Import.TaskParser
-- Description : Parser for task import format
--
-- Parses the markdown task import format via two-stage parsing:
-- first parse as markdown, then extract structured data from the AST.
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
  ( ParsedSolution (..)
  , ParsedTask (..)
  , levelFromGerman
  )
import Competences.Markdown.AST (Block (..), Document (..))
import Competences.Markdown.Parser (parseMarkdown)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse task import format
--
-- Parses one or more tasks from the input text.
-- Each task starts with a # heading.
parseTaskImport :: Text -> Either ParseError [ParsedTask]
parseTaskImport input
  | T.null (T.strip input) = Right []
  | otherwise = case parseMarkdown input of
      Left err -> Left (show err)
      Right (Document blocks) ->
        Right $ map parseTask (groupByHeading 1 blocks)

-- | Extract a ParsedTask from a heading-1 section
parseTask :: (Text, [Block]) -> ParsedTask
parseTask (headingText, blocks) =
  let (ident, replaces) = parseReplacesClause headingText
      sections = groupByHeading 2 blocks
      contentSection = findSection "Angabe" sections
      solutions = extractSolutions sections
      competenceRefs = extractCompetences sections
   in ParsedTask
        { identifier = TaskIdentifier ident
        , replacesIdentifier = TaskIdentifier <$> replaces
        , content = maybe "" id contentSection
        , solutions = solutions
        , competenceRefs = competenceRefs
        }

-- | Find a section by name and return its content as text
findSection :: Text -> [(Text, [Block])] -> Maybe Text
findSection name sections =
  case filter (\(n, _) -> n == name) sections of
    ((_, blocks) : _) -> Just (blocksToText blocks)
    [] -> Nothing

-- | Extract solutions from sections
extractSolutions :: [(Text, [Block])] -> [ParsedSolution]
extractSolutions = concatMap toSolution
  where
    toSolution (name, blocks)
      | name == "Hinweis" = [ParsedSolution Hint (blocksToText blocks)]
      | name == "Ergebnis" = [ParsedSolution Results (blocksToText blocks)]
      | name == "Komplettlösung" = [ParsedSolution Complete (blocksToText blocks)]
      | otherwise = []

-- | Extract competence references from ## Kompetenzen section
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
