-- |
-- Module      : Competences.Import.CompetenceGridParser
-- Description : Parser for competence grid import format
--
-- Parses the markdown competence grid import format via two-stage parsing:
-- first parse as markdown, then extract structured data from the AST.
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

import Competences.Document.Competence (Level)
import Competences.Import.ASTExtract
  ( groupByHeading
  , inlinesToText
  )
import Competences.Import.ParserUtils (ParseError, parseReplacesClause)
import Competences.Import.Types
  ( ParsedCompetence (..)
  , ParsedGrid (..)
  , levelFromGerman
  )
import Competences.Markdown.AST (Block (..), Document (..), Inline)
import Competences.Markdown.Parser (parseMarkdown)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse competence grid import format
--
-- Parses one or more grids from the input text.
-- Each grid starts with a # heading and contains ## competences.
parseGridImport :: Text -> Either ParseError [ParsedGrid]
parseGridImport input
  | T.null (T.strip input) = Right []
  | otherwise = case parseMarkdown input of
      Left err -> Left (show err)
      Right (Document blocks) ->
        Right $ map parseGrid (groupByHeading 1 blocks)

-- | Extract a ParsedGrid from a heading-1 section
parseGrid :: (Text, [Block]) -> ParsedGrid
parseGrid (title, blocks) =
  ParsedGrid
    { title = title
    , competences = map parseCompetence (groupByHeading 2 blocks)
    }

-- | Extract a ParsedCompetence from a heading-2 section
parseCompetence :: (Text, [Block]) -> ParsedCompetence
parseCompetence (headingText, blocks) =
  let (desc, replaces) = parseReplacesClause headingText
      levels = extractLevels blocks
   in ParsedCompetence
        { description = desc
        , replacesDescription = replaces
        , levels = levels
        }

-- | Extract level descriptions from blocks (expecting a BulletList)
extractLevels :: [Block] -> Map Level Text
extractLevels blocks =
  let items = concatMap getBulletItems blocks
      parsed = mapMaybe parseLevelItem items
   in Map.fromList parsed

-- | Extract bullet list items from a block
getBulletItems :: Block -> [[Block]]
getBulletItems (BulletList items) = items
getBulletItems _ = []

-- | Parse a level item text: "Wesentlich: Description text"
parseLevelItem :: [Block] -> Maybe (Level, Text)
parseLevelItem itemBlocks =
  let text = inlinesToText $ concatMap extractInlines itemBlocks
   in case T.breakOn ":" text of
        (levelName, rest)
          | not (T.null rest) ->
              case levelFromGerman (T.strip levelName) of
                Just level -> Just (level, T.strip (T.drop 1 rest))
                Nothing -> Nothing
        _ -> Nothing

-- | Extract inlines from paragraph blocks
extractInlines :: Block -> [Inline]
extractInlines (Paragraph inlines) = inlines
extractInlines _ = []
