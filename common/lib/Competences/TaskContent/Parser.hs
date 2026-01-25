{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.TaskContent.Parser
-- Description : Parser for task content markup language
--
-- Parses the task content markup language using attoparsec.
-- See "Competences.TaskContent.AST" for the AST types.
module Competences.TaskContent.Parser
  ( -- * Parsing
    parseTaskContent
  , ParseError
  )
where

import Competences.TaskContent.AST
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser
  , anyChar
  , char
  , endOfInput
  , many'
  , many1'
  , manyTill'
  , parseOnly
  , peekChar
  , satisfy
  , string
  , takeWhile1
  )
import Data.Char (isDigit, isLower, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

-- | Parse error message
type ParseError = String

-- | Parse task content markup to AST
--
-- Returns 'Left' with error message on parse failure,
-- 'Right' with parsed 'TaskContent' on success.
--
-- Example:
--
-- @
-- parseTaskContent "Solve $x + 1 = 2$"
-- -- Right (TaskContent [Paragraph [Plain "Solve ", MathInline "x + 1 = 2"]])
-- @
parseTaskContent :: Text -> Either ParseError TaskContent
parseTaskContent input
  | T.null (T.strip input) = Right (TaskContent [])
  | otherwise = parseOnly (taskContentP <* endOfInput) input

-- | Parse entire document as a sequence of blocks
taskContentP :: Parser TaskContent
taskContentP = TaskContent <$> blocksP

-- | Parse multiple blocks separated by blank lines
blocksP :: Parser [Block]
blocksP = do
  skipSpaces
  first <- blockP
  rest <- many' (blankLine *> blockP)
  skipSpaces
  _ <- many' (satisfy (== '\n'))  -- Consume trailing newlines
  pure (first : rest)
  where
    blankLine = many1' (satisfy (== '\n')) *> skipSpaces

-- | Parse a single block (tries heading, math, list, then paragraph)
blockP :: Parser Block
blockP = headingP <|> mathBlockP <|> listBlockP <|> paragraphP

-- | Parse heading: # ... (1-6 hashes at start of line)
headingP :: Parser Block
headingP = do
  hashes <- takeWhile1 (== '#')
  let level = min 6 (T.length hashes)
  skipSpaces
  content <- inlineListP
  pure $ Heading level content

-- | Parse display math block: $$...$$ or \[...\]
mathBlockP :: Parser Block
mathBlockP = dollarBlockP <|> bracketBlockP

-- | Parse display math with $$ delimiters
dollarBlockP :: Parser Block
dollarBlockP = do
  _ <- string "$$"
  latex <- manyTill' anyChar (string "$$")
  pure $ MathBlock (T.pack latex)

-- | Parse display math with \[...\] delimiters
bracketBlockP :: Parser Block
bracketBlockP = do
  _ <- string "\\["
  latex <- manyTill' anyChar (string "\\]")
  pure $ MathBlock (T.pack latex)

-- | Try to parse a list block (subtask or subquestion)
listBlockP :: Parser Block
listBlockP = do
  firstItem <- listItemP
  restItems <- many' (char '\n' *> skipSpaces *> listItemP)
  let items = firstItem : restItems
  pure $ case firstItem.marker of
    m | isLetterMarker m -> SubTaskList items
    _ -> SubQuestionList items
  where
    isLetterMarker m = T.length m == 2 && isLower (T.head m)

-- | Parse a single list item (either letter or number marker)
-- Content is wrapped in a Paragraph block for the [Block] type
-- Multi-line items use indentation-based continuation
listItemP :: Parser ListItem
listItemP = do
  marker <- letterMarkerP <|> numberMarkerP
  skipSpaces
  firstLine <- inlineListP
  -- Collect continuation lines (indented content)
  continuations <- many' continuationLineP
  let allInlines = firstLine ++ concat continuations
  -- Wrap all inlines in a single Paragraph for now
  -- TODO: Support multiple paragraphs with blank line + indentation
  pure $ ListItem marker [Paragraph allInlines]

-- | Parse a continuation line (starts with whitespace after newline)
continuationLineP :: Parser [Inline]
continuationLineP = do
  _ <- char '\n'
  _ <- many1' (satisfy (\c -> c == ' ' || c == '\t')) -- At least one space/tab
  inlineListP

-- | Parse letter marker: a. b. c. etc. (lowercase only)
letterMarkerP :: Parser Text
letterMarkerP = do
  c <- satisfy isLower
  _ <- char '.'
  pure $ T.pack [c, '.']

-- | Parse number marker: 1. 2. 3. etc.
numberMarkerP :: Parser Text
numberMarkerP = do
  digits <- takeWhile1 isDigit
  _ <- char '.'
  pure $ digits <> "."

-- | Parse a paragraph (inline content)
paragraphP :: Parser Block
paragraphP = Paragraph <$> inlineListP

-- | Parse list of inline elements
inlineListP :: Parser [Inline]
inlineListP = many1' inlineP

-- | Parse a single inline element
inlineP :: Parser Inline
inlineP = strongP <|> emphP <|> mathInlineP <|> plainP

-- | Parse strong text: **...**
strongP :: Parser Inline
strongP = do
  _ <- string "**"
  content <- manyTill' inlineInDelimP (string "**")
  pure $ Strong content

-- | Parse emphasized text: *...*
emphP :: Parser Inline
emphP = do
  _ <- char '*'
  -- Make sure it's not ** (strong)
  next <- peekChar
  case next of
    Just '*' -> fail "not emph"
    _ -> pure ()
  content <- manyTill' inlineInDelimP (char '*')
  pure $ Emph content

-- | Inline parser inside delimiters (strong/emph)
inlineInDelimP :: Parser Inline
inlineInDelimP = mathInlineP <|> plainInDelimP

-- | Plain text inside delimiters (no *, $, or \ allowed)
plainInDelimP :: Parser Inline
plainInDelimP = Plain <$> takeWhile1 (\c -> c /= '*' && c /= '$' && c /= '\\' && c /= '\n')

-- | Parse inline math: $...$ or \(...\)
mathInlineP :: Parser Inline
mathInlineP = dollarInlineP <|> parenInlineP

-- | Parse inline math with $ delimiters
dollarInlineP :: Parser Inline
dollarInlineP = do
  _ <- char '$'
  -- Make sure it's not $$ (block math)
  next <- peekChar
  case next of
    Just '$' -> fail "not inline math"
    _ -> pure ()
  latex <- manyTill' anyChar (char '$')
  pure $ MathInline (T.pack latex)

-- | Parse inline math with \(...\) delimiters
parenInlineP :: Parser Inline
parenInlineP = do
  _ <- string "\\("
  latex <- manyTill' anyChar (string "\\)")
  pure $ MathInline (T.pack latex)

-- | Parse plain text (everything that's not a special marker)
plainP :: Parser Inline
plainP = Plain <$> (chunk <|> singleChar)
  where
    -- Take multiple non-special characters
    chunk = takeWhile1 isPlainChar
    -- Or take a single character if it's not a delimiter start
    singleChar = do
      c <- peekChar
      case c of
        Just '*' -> fail "not plain"
        Just '$' -> fail "not plain"
        Just '\\' -> fail "not plain"
        Just '\n' -> fail "not plain"
        _ -> T.singleton <$> anyChar

    isPlainChar c = not (c `elem` ("*$\\\n" :: String)) && not (isSpace c && c /= ' ')

-- | Skip horizontal spaces (not newlines)
skipSpaces :: Parser ()
skipSpaces = () <$ many' (satisfy (\c -> isSpace c && c /= '\n'))
