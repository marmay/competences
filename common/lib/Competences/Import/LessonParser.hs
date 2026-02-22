-- |
-- Module      : Competences.Import.LessonParser
-- Description : Parser for lesson import format
--
-- Parses the markdown lesson import format via two-stage parsing:
-- first parse as markdown, then extract structured data from the AST.
--
-- @
-- # Lesson Title (Ersetzt: Original Title)
--
-- ## Angaben
-- Date: 2026-03-15
--
-- ## Beschreibung
-- Lesson description text...
--
-- ## Kompetenzen
-- - Grid / Competence / Wesentlich
--
-- ## Materialien
-- - Buch S.42
-- - Video: Gleichungen
--
-- ## Aufgaben
-- - Mathematik-Test 3a
--
-- ## Phasen
-- - Einstieg / Plenum / Darbietend / 10 min
--   Wiederholung der letzten Stunde.
-- - Erarbeitung / Gruppenarbeit / Zusammenwirkend / 20 min
--   Schüler lösen Aufgaben in Kleingruppen.
--
-- ## Notizen
-- Teacher notes...
-- @
module Competences.Import.LessonParser
  ( -- * Parsing
    parseLessonImport
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
  ( ParsedLesson (..)
  , ParsedLessonPhase (..)
  , actionFormFromGerman
  , levelFromGerman
  , socialFormFromGerman
  )
import Competences.Markdown.AST (Block (..), Document (..))
import Competences.Markdown.Parser (parseMarkdown)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)

-- | Parse lesson import format
--
-- Parses one or more lessons from the input text.
-- Each lesson starts with a # heading.
parseLessonImport :: Text -> Either ParseError [ParsedLesson]
parseLessonImport input
  | T.null (T.strip input) = Right []
  | otherwise = case parseMarkdown input of
      Left err -> Left (show err)
      Right (Document blocks) ->
        Right $ map parseLesson (groupByHeading 1 blocks)

-- | Extract a ParsedLesson from a heading-1 section
parseLesson :: (Text, [Block]) -> ParsedLesson
parseLesson (headingText, blocks) =
  let (title, replaces) = parseReplacesClause headingText
      sections = groupByHeading 2 blocks
      description = fromMaybe "" $ findSection "Beschreibung" sections
      angaben = fromMaybe "" $ findSection "Angaben" sections
      date = parseAngaben angaben
      competenceRefs = extractCompetences sections
      resourceIdents = extractListItems "Materialien" sections
      assignmentNames = extractListItems "Aufgaben" sections
      phases = extractPhases sections
      notes = fromMaybe "" $ findSection "Notizen" sections
   in ParsedLesson
        { title = title
        , replacesTitle = replaces
        , description = description
        , date = date
        , competenceRefs = competenceRefs
        , resourceIdentifiers = resourceIdents
        , assignmentNames = assignmentNames
        , phases = phases
        , notes = notes
        }

-- | Find a section by name and return its content as text
findSection :: Text -> [(Text, [Block])] -> Maybe Text
findSection name sections =
  case filter (\(n, _) -> n == name) sections of
    ((_, bs) : _) -> Just (blocksToText bs)
    [] -> Nothing

-- | Parse the ## Angaben section to extract date
parseAngaben :: Text -> Maybe Day
parseAngaben content =
  let lines' = T.lines content
   in findKeyValue "Date" lines' >>= parseDate
  where
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

-- | Extract bullet list items as plain text from a named section
extractListItems :: Text -> [(Text, [Block])] -> [Text]
extractListItems sectionName sections =
  case findSectionBlocks sectionName sections of
    Nothing -> []
    Just blocks -> concatMap getBulletItemTexts blocks

-- | Extract phases from ## Phasen section
--
-- Phase format: @- Title / SocialForm / ActionForm / Duration min@
-- followed by optional indented notes on subsequent lines.
-- Slashes in the title can be escaped as @\/@.
extractPhases :: [(Text, [Block])] -> [ParsedLessonPhase]
extractPhases sections =
  case findSectionBlocks "Phasen" sections of
    Nothing -> []
    Just blocks ->
      let items = concatMap getBulletListItems blocks
       in mapMaybe parsePhaseItem items

-- | Extract BulletList items as [[Block]]
getBulletListItems :: Block -> [[Block]]
getBulletListItems (BulletList items) = items
getBulletListItems _ = []

-- | Parse a phase item from bullet list item blocks
--
-- First line: "Title / SocialForm / ActionForm / Duration min"
-- Subsequent lines: notes
parsePhaseItem :: [Block] -> Maybe ParsedLessonPhase
parsePhaseItem blocks =
  let text = blocksToText blocks
      lines' = T.lines text
   in case lines' of
        [] -> Nothing
        (firstLine : restLines) ->
          parsePhaseLine firstLine (T.intercalate "\n" restLines)

-- | Parse a phase line: "Title / SocialForm / ActionForm / Duration min"
-- with escaped slashes (\\/) in the title
parsePhaseLine :: Text -> Text -> Maybe ParsedLessonPhase
parsePhaseLine line notesText =
  let -- Split on unescaped slashes
      parts = splitOnUnescapedSlash (T.strip line)
   in case parts of
        [titlePart, socialPart, actionPart, durationPart] -> do
          let phaseTitle = T.strip $ T.replace "\\/" "/" titlePart
          socialForm <- socialFormFromGerman (T.strip socialPart)
          actionForm <- actionFormFromGerman (T.strip actionPart)
          duration <- parseDuration (T.strip durationPart)
          Just
            ParsedLessonPhase
              { title = phaseTitle
              , socialForm = socialForm
              , actionForm = actionForm
              , duration = duration
              , notes = T.strip notesText
              }
        _ -> Nothing

-- | Split text on unescaped slashes (not preceded by backslash)
splitOnUnescapedSlash :: Text -> [Text]
splitOnUnescapedSlash = go ""
  where
    go acc txt
      | T.null txt = [acc]
      | Just rest <- T.stripPrefix "\\/" txt = go (acc <> "\\/") rest
      | Just rest <- T.stripPrefix "/" txt = acc : go "" rest
      | otherwise =
          let (c, rest) = (T.take 1 txt, T.drop 1 txt)
           in go (acc <> c) rest

-- | Parse duration: "10 min" or "10"
parseDuration :: Text -> Maybe Int
parseDuration txt =
  let stripped = T.strip $ fromMaybe txt (T.stripSuffix "min" txt)
   in case reads (T.unpack (T.strip stripped)) of
        [(n, "")] | n > 0 -> Just n
        _ -> Nothing
