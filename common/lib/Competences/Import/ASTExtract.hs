-- |
-- Module      : Competences.Import.ASTExtract
-- Description : Utilities for extracting structured data from markdown ASTs
--
-- Reusable functions for the two-stage import parsing approach:
-- parse markdown → extract structured data from AST.
module Competences.Import.ASTExtract
  ( -- * Section grouping
    groupByHeading

    -- * Text extraction
  , inlinesToText
  , blocksToText
  , bulletListItemTexts
  )
where

import Competences.Markdown.AST
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Group blocks into sections delimited by headings of a given level.
-- Blocks before the first matching heading are discarded.
-- Returns @[(headingText, bodyBlocks)]@.
groupByHeading :: Int -> [Block] -> [(Text, [Block])]
groupByHeading level = go Nothing []
  where
    go Nothing _ [] = []
    go (Just title) acc [] = [(title, reverse acc)]
    go mTitle acc (Heading n inlines : rest)
      | n == level =
          let section = case mTitle of
                Nothing -> []
                Just title -> [(title, reverse acc)]
           in section ++ go (Just (inlinesToText inlines)) [] rest
    go mTitle acc (block : rest) =
      go mTitle (block : acc) rest

-- | Flatten @[Inline]@ to plain text (strips formatting).
inlinesToText :: [Inline] -> Text
inlinesToText = T.concat . map go
  where
    go = \case
      Plain t -> t
      Emph inlines -> inlinesToText inlines
      Strong inlines -> inlinesToText inlines
      Code t -> t
      MathInline t -> "$" <> t <> "$"
      Link _ inlines _ -> inlinesToText inlines
      SoftLineBreak -> " "
      HardLineBreak -> "\n"

-- | Serialize blocks back to markdown text (for content fields stored as Text).
blocksToText :: [Block] -> Text
blocksToText [] = ""
blocksToText blocks = T.intercalate "\n\n" (map blockToText blocks)

blockToText :: Block -> Text
blockToText = \case
  Paragraph inlines -> inlinesToMarkdown inlines
  Heading n inlines -> T.replicate n "#" <> " " <> inlinesToMarkdown inlines
  FencedCodeBlock mInfo body ->
    "```" <> fromMaybe "" mInfo <> "\n" <> body <> "\n```"
  OrderedList start items ->
    T.intercalate "\n" $
      zipWith (\i item -> T.pack (show i) <> ". " <> blocksToText item) [start ..] items
  BulletList items ->
    T.intercalate "\n" $ map (\item -> "- " <> blocksToText item) items
  LetterList items ->
    T.intercalate "\n" $
      zipWith (\c item -> T.singleton c <> ". " <> blocksToText item) ['a' ..] items
  MathBlock tex -> "$$" <> tex <> "$$"
  ThematicBreak -> "---"
  Admonition _adType mTitle bodyBlocks ->
    let titlePart = maybe "" (\inlines -> " " <> inlinesToMarkdown inlines) mTitle
        bodyLines = T.lines (blocksToText bodyBlocks)
     in T.intercalate "\n" $ ("> [!remark]" <> titlePart) : map ("> " <>) bodyLines

-- | Serialize inlines back to markdown (preserves formatting).
inlinesToMarkdown :: [Inline] -> Text
inlinesToMarkdown = T.concat . map go
  where
    go = \case
      Plain t -> t
      Emph inlines -> "*" <> inlinesToMarkdown inlines <> "*"
      Strong inlines -> "**" <> inlinesToMarkdown inlines <> "**"
      Code t -> "`" <> t <> "`"
      MathInline t -> "$" <> t <> "$"
      Link url inlines mTitle ->
        "[" <> inlinesToMarkdown inlines <> "](" <> url
          <> maybe "" (\title -> " \"" <> title <> "\"") mTitle
          <> ")"
      SoftLineBreak -> "\n"
      HardLineBreak -> "\\\n"

-- | Extract plain text from each item in a BulletList's @[[Block]]@.
bulletListItemTexts :: [[Block]] -> [Text]
bulletListItemTexts = map (blocksToText' . concatMap extractInlines)
  where
    extractInlines = \case
      Paragraph inlines -> inlines
      _ -> []

    blocksToText' = inlinesToText
