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
import Data.List (unfoldr)
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
    , notesGridP
    , columnsP
    , taskBlockP
    , fencedCodeBlockP
    , mathBlockP
    , letterListP
    , orderedListP
    , bulletListP
    , admonitionP
    , vspaceP
    , tableP
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

-- | GFM-style pipe table: header line, separator line, zero or more body rows.
--   Commits to table parsing only after the second line parses as a valid
--   separator; otherwise backtracks so paragraphP can handle the lines.
tableP :: Parser Block
tableP = try $ do
  headerLine <- takeWhile1P Nothing (/= '\n')
  guard (T.any (== '|') headerLine)
  _ <- newline
  sepLine <- takeWhile1P Nothing (/= '\n')
  alignments <- case parseSeparator sepLine of
    Just as -> pure as
    Nothing -> fail "not a table separator"
  let headerCells = splitRow headerLine
  guard (length headerCells == length alignments)
  bodyRows <- many tableBodyRowP
  let headerInlines = map parseCellInlines headerCells
      bodyInlines = map (map parseCellInlines) bodyRows
  pure $ Table alignments headerInlines bodyInlines
  where
    tableBodyRowP = try $ do
      _ <- newline
      line <- takeWhile1P Nothing (/= '\n')
      guard (T.any (== '|') line)
      -- Must not look like a separator line — that would be a degenerate
      -- two-table input; treat the second separator as the start of a new
      -- block instead. (Conservative: fail and let outer parsing decide.)
      case parseSeparator line of
        Just _ -> fail "row looks like separator"
        Nothing -> pure ()
      pure (splitRow line)

    parseCellInlines :: Text -> [Inline]
    parseCellInlines t
      | T.null t = []
      | otherwise = fromMaybe [Plain t] (parseMaybe lineInlinesP t)

-- | Parse the separator row of a table. Each cell must be of the form
-- @:?-{3,}:?@ (optionally wrapped in whitespace and surrounding pipes).
-- Returns one 'Alignment' per cell, or 'Nothing' if the line isn't a valid
-- separator.
parseSeparator :: Text -> Maybe [Alignment]
parseSeparator line = case splitRow line of
  [] -> Nothing
  cells -> traverse alignFromCell cells
  where
    alignFromCell :: Text -> Maybe Alignment
    alignFromCell s =
      let (hasL, s1) = case T.uncons s of
            Just (':', rest) -> (True, rest)
            _ -> (False, s)
          dashesLen = T.length (T.takeWhile (== '-') s1)
          afterDashes = T.drop dashesLen s1
          hasR = afterDashes == ":"
          validRest = T.null afterDashes || hasR
       in if dashesLen >= 3 && validRest
            then Just $ case (hasL, hasR) of
              (False, False) -> AlignDefault
              (True, False) -> AlignLeft
              (False, True) -> AlignRight
              (True, True) -> AlignCenter
            else Nothing

-- | Split a pipe-delimited row into its cells. Handles:
--
-- * @\\|@ as a literal pipe within a cell.
-- * Pipes inside @$...$@, @\\(...\\)@, and @`...`@ spans are not separators.
-- * Optional leading and trailing pipes are decorative (stripped).
-- * Inner whitespace around cell content is trimmed.
splitRow :: Text -> [Text]
splitRow input = map T.strip $ dropEdgePipes $ scan (T.unpack (T.strip input)) "" [] Normal
  where
    dropEdgePipes cells =
      let cells' = case cells of
            (c : rest) | T.null (T.strip c) -> rest
            _ -> cells
       in case reverse cells' of
            (c : rest) | T.null (T.strip c) -> reverse rest
            _ -> cells'

    finalize cur cells = reverse (T.pack (reverse cur) : cells)

    scan :: String -> String -> [Text] -> CellState -> [Text]
    scan [] cur cells _ = finalize cur cells
    -- Escape: \| → literal | (only at top level)
    scan ('\\' : '|' : rest) cur cells Normal = scan rest ('|' : cur) cells Normal
    -- \( opens paren-math
    scan ('\\' : '(' : rest) cur cells Normal = scan rest ('(' : '\\' : cur) cells InParenMath
    -- \) closes paren-math
    scan ('\\' : ')' : rest) cur cells InParenMath = scan rest (')' : '\\' : cur) cells Normal
    -- $ toggles dollar-math (only at top level)
    scan ('$' : rest) cur cells Normal = scan rest ('$' : cur) cells InMath
    scan ('$' : rest) cur cells InMath = scan rest ('$' : cur) cells Normal
    -- ` toggles code span
    scan ('`' : rest) cur cells Normal = scan rest ('`' : cur) cells InCode
    scan ('`' : rest) cur cells InCode = scan rest ('`' : cur) cells Normal
    -- | splits only at top level
    scan ('|' : rest) cur cells Normal = scan rest "" (T.pack (reverse cur) : cells) Normal
    -- Any other char (including | inside math/code) is part of current cell
    scan (c : rest) cur cells st = scan rest (c : cur) cells st

-- | State machine for 'splitRow': tracks whether the scanner is inside a
-- math span, paren-math span, or code span (within which @|@ is literal).
data CellState
  = Normal
  | InMath
  | InParenMath
  | InCode
  deriving (Eq)

-- | Side-by-side columns: ```columns or ```columns N:M:... fenced block.
--   Cells separated by @+++@. Each cell parsed recursively as @[Block]@.
columnsP :: Parser Block
columnsP = try $ do
  (fenceChar, fenceLen) <- backtickFence
  _ <- hspace
  _ <- string "columns"
  ratios <- option [] (try $ hspace1 *> ratioListP)
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '`')
  _ <- newline
  bodyLines <- nestedFencedBodyP fenceChar fenceLen
  let cellSegments = splitOnPlus bodyLines
      cells = map (parseCellBlocks . T.intercalate "\n") cellSegments
      finalRatios = take (length cells) (ratios ++ repeat 1)
  pure $ Columns finalRatios cells
  where
    ratioListP :: Parser [Int]
    ratioListP = do
      first <- ratioNumP
      rest <- many (try (char ':' *> ratioNumP))
      pure (first : rest)

    ratioNumP :: Parser Int
    ratioNumP = do
      digits <- takeWhile1P (Just "ratio digit") (\c -> c >= '0' && c <= '9')
      let n = read (T.unpack digits)
      guard (n > 0)
      pure n

    splitOnPlus :: [Text] -> [[Text]]
    splitOnPlus = unfoldr $ \xs ->
      case xs of
        [] -> Nothing
        _ -> let (seg, rest) = break isPlusLine xs
              in Just (seg, drop 1 rest)

    isPlusLine :: Text -> Bool
    isPlusLine line =
      let stripped = T.strip line
       in T.length stripped >= 3 && T.all (== '+') stripped

    parseCellBlocks :: Text -> [Block]
    parseCellBlocks t =
      let trimmed = T.strip t
       in if T.null trimmed
            then []
            else case parseMaybe documentP trimmed of
              Just (Document blocks) -> blocks
              Nothing -> [Paragraph [Plain trimmed]]

-- | Paragraph: inline content terminated by blank line or eof
paragraphP :: Parser Block
paragraphP = Paragraph <$> inlinesP

-- | Skip blank lines (zero or more)
skipBlankLines :: Parser ()
skipBlankLines = skipMany (try blankLine)
  where
    blankLine = hspace *> newline
