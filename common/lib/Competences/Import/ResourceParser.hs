-- |
-- Module      : Competences.Import.ResourceParser
-- Description : Parser for resource import format
--
-- Parses the markdown resource import format via two-stage parsing:
-- first parse as markdown, then extract structured data from the AST.
--
-- @
-- # Resource Identifier (Ersetzt: Original Identifier)
--
-- ## Inhalt
-- Rich text content with $math$ support...
--
-- ## Kompetenzen
-- - Grid / Competence / Wesentlich
-- @
module Competences.Import.ResourceParser
  ( -- * Parsing
    parseResourceImport
  , ParseError
  )
where

import Competences.Document.Competence (Level)
import Competences.Import.ASTExtract
  ( blocksToText
  , bulletListItemTexts
  , groupByHeading
  )
import Competences.Import.ParserUtils (ParseError, parseReplacesClause)
import Competences.Import.Types
  ( ParsedResource (..)
  , levelFromGerman
  )
import Competences.Markdown.AST (Block (..), Document (..))
import Competences.Markdown.Parser (parseMarkdown)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse resource import format
--
-- Parses one or more resources from the input text.
-- Each resource starts with a # heading.
parseResourceImport :: Text -> Either ParseError [ParsedResource]
parseResourceImport input
  | T.null (T.strip input) = Right []
  | otherwise = case parseMarkdown input of
      Left err -> Left (show err)
      Right (Document blocks) ->
        Right $ map parseResource (groupByHeading 1 blocks)

-- | Extract a ParsedResource from a heading-1 section
parseResource :: (Text, [Block]) -> ParsedResource
parseResource (headingText, blocks) =
  let (ident, replaces) = parseReplacesClause headingText
      sections = groupByHeading 2 blocks
      content = fromMaybe "" $ findSection "Inhalt" sections
      competenceRefs = extractCompetences sections
   in ParsedResource
        { identifier = ident
        , replacesIdentifier = replaces
        , content = content
        , competenceRefs = competenceRefs
        }

-- | Find a section by name and return its content as text
findSection :: Text -> [(Text, [Block])] -> Maybe Text
findSection name sections =
  case filter (\(n, _) -> n == name) sections of
    ((_, bs) : _) -> Just (blocksToText bs)
    [] -> Nothing

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
    ((_, bs) : _) -> Just bs
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
