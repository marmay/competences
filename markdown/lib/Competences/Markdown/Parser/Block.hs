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
import Competences.Markdown.Parser.Inline (inlinesP, lineInlinesP)
import Control.Monad (guard, void)
import Data.Char (isLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (unfoldr)

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
    , notesGridP
    , taskBlockP
    , fencedCodeBlockP
    , mathBlockP
    , letterListP
    , orderedListP
    , bulletListP
    , admonitionP
    , vspaceP
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
  content <- lineInlinesP
  pure $ Heading level content

-- | Match a closing fence line for a fenced block.
closingFence :: Char -> Int -> Parser ()
closingFence fenceChar minLen = do
  _ <- newline <|> pure ' ' -- may not have trailing newline at eof
  fence' <- takeWhile1P Nothing (== fenceChar)
  guard (T.length fence' >= minLen)
  hspace
  void (lookAhead newline) <|> eof

-- | Parse backtick fence opener (at least 3 backticks)
backtickFence :: Parser (Char, Int)
backtickFence = do
  ticks <- takeWhile1P (Just "backtick fence") (== '`')
  guard (T.length ticks >= 3)
  pure ('`', T.length ticks)

-- | Parse tilde fence opener (at least 3 tildes)
tildeFence :: Parser (Char, Int)
tildeFence = do
  tildes <- takeWhile1P (Just "tilde fence") (== '~')
  guard (T.length tildes >= 3)
  pure ('~', T.length tildes)

-- | Collect body lines of a fenced block, tracking nested fences of the
-- same character so that inner fenced code blocks don't terminate the outer.
-- Returns lines between the opening fence (already consumed) and closing fence.
nestedFencedBodyP :: Char -> Int -> Parser [Text]
nestedFencedBodyP fenceChar minLen = go 0 []
  where
    go :: Int -> [Text] -> Parser [Text]
    go !depth acc = do
      line <- takeWhileP Nothing (/= '\n')
      if depth == 0 && isClosingLine line
        then do
          void (lookAhead newline) <|> eof
          pure (reverse acc)
        else do
          let !depth' = adjustDepth depth line
          (newline >> go depth' (line : acc)) <|> pure (reverse (line : acc))

    isClosingLine line =
      let stripped = T.stripStart line
          fencePart = T.takeWhile (== fenceChar) stripped
          rest = T.strip (T.drop (T.length fencePart) stripped)
       in T.length fencePart >= minLen && T.null rest

    isOpeningLine line =
      let stripped = T.stripStart line
          fencePart = T.takeWhile (== fenceChar) stripped
          rest = T.strip (T.drop (T.length fencePart) stripped)
       in T.length fencePart >= 3 && not (T.null rest)

    adjustDepth d line
      | isOpeningLine line = d + 1
      | isClosingLine line && d > 0 = d - 1
      | otherwise = d

-- | BTC notes grid: ```btc:notes-grid ... ```
notesGridP :: Parser Block
notesGridP = try $ do
  fence <- backtickFence
  let (fenceChar, fenceLen) = fence
  _ <- hspace
  _ <- string "btc:notes-grid"
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '`')
  _ <- newline
  bodyLines <- nestedFencedBodyP fenceChar fenceLen
  let bodyText = T.intercalate "\n" bodyLines
      cells = splitCells bodyText
      parsed = map parseCell (take 4 cells)
      padded = parsed ++ replicate (4 - length parsed) []
  case padded of
    [c1, c2, c3, c4] -> pure $ NotesGrid c1 c2 c3 c4
    _ -> pure $ NotesGrid [] [] [] [] -- unreachable
  where
    splitCells :: Text -> [Text]
    splitCells txt =
      let ls = T.lines txt
       in map (T.intercalate "\n") $ splitOn isSeparator ls

    isSeparator :: Text -> Bool
    isSeparator line =
      let stripped = T.strip line
       in T.length stripped >= 3 && T.all (== '-') stripped

    splitOn :: (a -> Bool) -> [a] -> [[a]]
    splitOn p = unfoldr $ \xs ->
      case xs of
        [] -> Nothing
        _ -> let (seg, rest) = break p xs
              in Just (seg, drop 1 rest)

    parseCell :: Text -> [Block]
    parseCell t =
      let trimmed = T.strip t
       in if T.null trimmed
            then []
            else case parseMaybe documentP trimmed of
              Just (Document blocks) -> blocks
              Nothing -> [Paragraph [Plain trimmed]]

-- | Task format block: ```task:cloze, ```task:singlechoice, etc.
-- Unknown task:* formats fall back to FencedCodeBlock.
taskBlockP :: Parser Block
taskBlockP = try $ do
  (fenceChar, fenceLen) <- backtickFence
  _ <- hspace
  _ <- string "task:"
  format <- takeWhile1P (Just "task format") (\c -> c /= '\n' && c /= '`' && c /= ' ')
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '`')
  _ <- newline
  bodyLines <- nestedFencedBodyP fenceChar fenceLen
  case format of
    "cloze" -> parseClozeBody bodyLines
    "singlechoice" -> parseChoiceBody SingleChoice bodyLines
    "multiplechoice" -> parseChoiceBody MultipleChoice bodyLines
    "mapping" -> parseMappingBody bodyLines
    _ -> pure $ FencedCodeBlock (Just ("task:" <> format)) (T.intercalate "\n" bodyLines)
  where
    parseClozeBody bodyLines =
      let segments = splitOnSeparator isDashSeparator bodyLines
       in case segments of
            [] -> pure $ ClozeBlock [] ClozeNoOptions
            [textSeg] ->
              pure $ ClozeBlock (parseCell (T.intercalate "\n" textSeg)) ClozeNoOptions
            (textSeg : optionSegs) ->
              let textBlocks = parseCell (T.intercalate "\n" textSeg)
                  options = case optionSegs of
                    [oneGroup] -> ClozeWordBank (parseCell (T.intercalate "\n" oneGroup))
                    multiple -> ClozePerBlankOptions
                      (map (\seg -> parseCell (T.intercalate "\n" seg)) multiple)
               in pure $ ClozeBlock textBlocks options

    parseChoiceBody choiceType bodyLines =
      let segments = splitOnSeparator isDashSeparator bodyLines
          items = map (\seg -> parseCell (T.intercalate "\n" seg)) segments
       in pure $ ChoiceBlock choiceType items

    parseMappingBody bodyLines =
      let halves = splitOnSeparator isPlusSeparator bodyLines
       in case halves of
            [leftLines, rightLines] ->
              let leftItems = map (\seg -> parseCell (T.intercalate "\n" seg))
                    (splitOnSeparator isDashSeparator leftLines)
                  rightItems = map (\seg -> parseCell (T.intercalate "\n" seg))
                    (splitOnSeparator isDashSeparator rightLines)
               in pure $ MappingBlock leftItems rightItems
            _ ->
              -- Malformed: treat as code block
              pure $ FencedCodeBlock (Just "task:mapping") (T.intercalate "\n" bodyLines)

    isDashSeparator :: Text -> Bool
    isDashSeparator line =
      let stripped = T.strip line
       in T.length stripped >= 3 && T.all (== '-') stripped

    isPlusSeparator :: Text -> Bool
    isPlusSeparator line =
      let stripped = T.strip line
       in T.length stripped >= 3 && T.all (== '+') stripped

    splitOnSeparator :: (Text -> Bool) -> [Text] -> [[Text]]
    splitOnSeparator p = unfoldr $ \xs ->
      case xs of
        [] -> Nothing
        _ -> let (seg, rest) = break p xs
              in Just (seg, drop 1 rest)

    parseCell :: Text -> [Block]
    parseCell t =
      let trimmed = T.strip t
       in if T.null trimmed
            then []
            else case parseMaybe documentP trimmed of
              Just (Document blocks) -> blocks
              Nothing -> [Paragraph [Plain trimmed]]

-- | Fenced code block: ``` or ~~~
fencedCodeBlockP :: Parser Block
fencedCodeBlockP = try $ do
  fence <- backtickFence <|> tildeFence
  let (fenceChar, fenceLen) = fence
  info <- optional (hspace *> takeWhile1P (Just "info string") (\c -> c /= '\n' && c /= '`'))
  _ <- newline
  body <- manyTill anySingle (try $ closingFence fenceChar fenceLen)
  pure $ FencedCodeBlock (T.strip <$> info) (T.pack body)

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

-- | Bullet list: - item, * item, + item
bulletListP :: Parser Block
bulletListP = do
  firstItem <- try bulletListItemP
  restItems <- many (try $ blankLinesBetweenItems *> bulletListItemP)
  pure $ BulletList (firstItem : restItems)
  where
    blankLinesBetweenItems = do
      _ <- newline
      skipBlankLines

-- | Single bullet list item
bulletListItemP :: Parser [Block]
bulletListItemP = do
  _ <- bulletMarkerP
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
      notFollowedBy (try bulletMarkerP)
      notFollowedBy (try letterMarkerP)
      notFollowedBy (try numberMarkerP)
      inlinesP

-- | Parse bullet marker: -, *, or + followed by space
bulletMarkerP :: Parser Char
bulletMarkerP = do
  c <- oneOf ("-*+" :: String)
  _ <- lookAhead (char ' ')
  pure c

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

-- | Vertical space: {{vspace:VALUE}}
vspaceP :: Parser Block
vspaceP = try $ do
  _ <- string "{{vspace:"
  val <- takeWhile1P (Just "vspace value") (/= '}')
  _ <- string "}}"
  _ <- optional eol
  pure (VSpace val)

-- | Paragraph: inline content terminated by blank line or eof
paragraphP :: Parser Block
paragraphP = Paragraph <$> inlinesP

-- | Skip blank lines (zero or more)
skipBlankLines :: Parser ()
skipBlankLines = skipMany (try blankLine)
  where
    blankLine = hspace *> newline
