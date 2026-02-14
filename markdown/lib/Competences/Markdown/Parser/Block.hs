-- |
-- Module      : Competences.Markdown.Parser.Block
-- Description : Block-level markdown parsers
--
-- Parses block elements: paragraphs, headings, fenced code blocks,
-- ordered lists, lettered lists, math blocks, and thematic breaks.
module Competences.Markdown.Parser.Block
  ( documentP
  , blockP
  )
where

import Competences.Markdown.AST
import Competences.Markdown.Parser.Inline (inlinesP)
import Control.Monad (guard, void)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Parse an entire markdown document
documentP :: Parser Document
documentP = do
  skipBlankLines
  blocks <- sepEndBy blockP skipBlankLines
  eof
  pure $ Document blocks

-- | Parse a single block element
blockP :: Parser Block
blockP =
  choice
    [ thematicBreakP
    , headingP
    , fencedCodeBlockP
    , mathBlockP
    , letterListP
    , orderedListP
    , admonitionP
    , paragraphP
    ]

-- | Thematic break: three or more -, *, or _ on a line (possibly with spaces)
thematicBreakP :: Parser Block
thematicBreakP = ThematicBreak <$ try go
  where
    go = do
      c <- oneOf ("-*_" :: String)
      rest <- takeWhileP Nothing (\ch -> ch == c || ch == ' ')
      -- Need at least 3 of the character
      let charCount = T.length (T.filter (== c) rest) + 1
      guard (charCount >= 3)
      -- Must be followed by newline or eof
      void (lookAhead newline) <|> eof

-- | Heading: # through ######
headingP :: Parser Block
headingP = try $ do
  hashes <- takeWhile1P (Just "heading marker") (== '#')
  let level = min 6 (T.length hashes)
  _ <- hspace1
  content <- inlinesP
  pure $ Heading level content

-- | Fenced code block: ``` or ~~~
fencedCodeBlockP :: Parser Block
fencedCodeBlockP = try $ do
  fence <- backtickFence <|> tildeFence
  let (fenceChar, fenceLen) = fence
  info <- optional (hspace *> takeWhile1P (Just "info string") (\c -> c /= '\n' && c /= '`'))
  _ <- newline
  body <- manyTill anySingle (try $ closingFence fenceChar fenceLen)
  pure $ FencedCodeBlock (T.strip <$> info) (T.pack body)
  where
    backtickFence = do
      ticks <- takeWhile1P (Just "backtick fence") (== '`')
      guard (T.length ticks >= 3)
      pure ('`', T.length ticks)

    tildeFence = do
      tildes <- takeWhile1P (Just "tilde fence") (== '~')
      guard (T.length tildes >= 3)
      pure ('~', T.length tildes)

    closingFence fenceChar minLen = do
      _ <- newline <|> pure ' ' -- may not have trailing newline at eof
      fence' <- takeWhile1P Nothing (== fenceChar)
      guard (T.length fence' >= minLen)
      hspace
      void (lookAhead newline) <|> eof

-- | Display math block: $$...$$ or \[...\]
mathBlockP :: Parser Block
mathBlockP = dollarMathBlockP <|> bracketMathBlockP

dollarMathBlockP :: Parser Block
dollarMathBlockP = do
  _ <- try (string "$$")
  content <- manyTill anySingle (string "$$")
  pure $ MathBlock (T.strip $ T.pack content)

bracketMathBlockP :: Parser Block
bracketMathBlockP = do
  _ <- try (string "\\[")
  content <- manyTill anySingle (string "\\]")
  pure $ MathBlock (T.strip $ T.pack content)

-- | Lettered list: a. b. c. etc.
letterListP :: Parser Block
letterListP = do
  firstItem <- try $ letterListItemP
  restItems <- many (try $ blankLinesBetweenItems *> letterListItemP)
  pure $ LetterList (firstItem : restItems)
  where
    blankLinesBetweenItems = do
      _ <- newline
      skipBlankLines

-- | Single lettered list item
letterListItemP :: Parser [Block]
letterListItemP = do
  _ <- letterMarkerP
  hspace
  firstLine <- inlinesP
  continuations <- many (try continuationLine)
  let allInlines = firstLine ++ concatMap addSoftBreak continuations
  pure [Paragraph allInlines]
  where
    addSoftBreak inlines = SoftLineBreak : inlines

    continuationLine = do
      _ <- newline
      _ <- hspace1 -- indented continuation
      -- Not a new list marker
      notFollowedBy (try letterMarkerP)
      notFollowedBy (try numberMarkerP)
      inlinesP

-- | Ordered list: 1. 2. 3. etc.
orderedListP :: Parser Block
orderedListP = do
  (startNum, firstItem) <- try $ orderedListItemP
  restItems <- many (try $ blankLinesBetweenItems *> fmap snd orderedListItemP)
  pure $ OrderedList startNum (firstItem : restItems)
  where
    blankLinesBetweenItems = do
      _ <- newline
      skipBlankLines

-- | Single ordered list item, returns (marker number, blocks)
orderedListItemP :: Parser (Int, [Block])
orderedListItemP = do
  num <- numberMarkerP
  hspace
  firstLine <- inlinesP
  continuations <- many (try continuationLine)
  let allInlines = firstLine ++ concatMap addSoftBreak continuations
  pure (num, [Paragraph allInlines])
  where
    addSoftBreak inlines = SoftLineBreak : inlines

    continuationLine = do
      _ <- newline
      _ <- hspace1
      notFollowedBy (try letterMarkerP)
      notFollowedBy (try numberMarkerP)
      inlinesP

-- | Parse letter marker: a. b. c. etc. (lowercase only)
letterMarkerP :: Parser Char
letterMarkerP = do
  c <- satisfy isLower
  _ <- char '.'
  _ <- lookAhead (char ' ')
  pure c

-- | Parse number marker: 1. 2. 3. etc. Returns the number.
numberMarkerP :: Parser Int
numberMarkerP = do
  digits <- takeWhile1P (Just "digit") (\c -> c >= '0' && c <= '9')
  _ <- char '.'
  _ <- lookAhead (char ' ')
  pure $ read (T.unpack digits)

-- | Admonition block: > [!type] optional title
admonitionP :: Parser Block
admonitionP = try $ do
  _ <- char '>'
  hspace
  _ <- string "[!"
  typeName <- takeWhile1P (Just "admonition type") (\c -> c /= ']' && c /= '\n')
  _ <- char ']'
  -- Optional title: rest of first line, parsed as inlines
  titleText <- optional (hspace1 *> restOfLine)
  -- Body lines: each starts with > on a new line
  bodyLines <- many $ try $ do
    _ <- newline
    _ <- char '>'
    line <- takeWhileP Nothing (/= '\n')
    -- Strip one leading space if present ("> text" -> "text", ">" -> "")
    pure $ fromMaybe line (T.stripPrefix " " line)
  -- Parse collected body as blocks (recursive)
  let adType = parseAdmonitionType typeName
      bodyText = T.intercalate "\n" bodyLines
      bodyBlocks = fromMaybe [] $ do
        Document blocks <- parseMaybe documentP bodyText
        pure blocks
  -- Parse title inlines (if present)
  let titleInlines = case titleText of
        Nothing -> Nothing
        Just t | T.null (T.strip t) -> Nothing
        Just t -> case parseMaybe inlinesP t of
          Nothing -> Nothing
          Just inlines -> Just inlines
  pure $ Admonition adType titleInlines bodyBlocks

-- | Consume rest of line without the newline
restOfLine :: Parser Text
restOfLine = takeWhile1P (Just "title text") (/= '\n')

-- | Map type name string to AdmonitionType (case-insensitive, with German aliases)
parseAdmonitionType :: Text -> AdmonitionType
parseAdmonitionType t = case T.toLower (T.strip t) of
  "definition" -> Definition
  "theorem" -> Theorem
  "satz" -> Theorem
  "lemma" -> Lemma
  "proof" -> Proof
  "beweis" -> Proof
  "remark" -> Remark
  "bemerkung" -> Remark
  "merksatz" -> Merksatz
  "remember" -> Merksatz
  "example" -> Example
  "beispiel" -> Example
  _ -> Remark -- fallback

-- | Paragraph: inline content terminated by blank line or eof
paragraphP :: Parser Block
paragraphP = Paragraph <$> inlinesP

-- | Skip blank lines (zero or more)
skipBlankLines :: Parser ()
skipBlankLines = skipMany (try blankLine)
  where
    blankLine = hspace *> newline
