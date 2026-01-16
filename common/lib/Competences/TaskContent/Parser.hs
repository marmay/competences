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
  pure (first : rest)
  where
    blankLine = many1' (satisfy (== '\n')) *> skipSpaces

-- | Parse a single block (tries list markers first, then paragraph)
blockP :: Parser Block
blockP = mathBlockP <|> listBlockP <|> paragraphP

-- | Parse display math block: $$...$$
mathBlockP :: Parser Block
mathBlockP = do
  _ <- string "$$"
  latex <- manyTill' anyChar (string "$$")
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
listItemP :: Parser ListItem
listItemP = do
  marker <- letterMarkerP <|> numberMarkerP
  skipSpaces
  content <- inlineListP
  pure $ ListItem marker content

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

-- | Plain text inside delimiters (no * allowed)
plainInDelimP :: Parser Inline
plainInDelimP = Plain <$> takeWhile1 (\c -> c /= '*' && c /= '$' && c /= '\n')

-- | Parse inline math: $...$
mathInlineP :: Parser Inline
mathInlineP = do
  _ <- char '$'
  -- Make sure it's not $$ (block math)
  next <- peekChar
  case next of
    Just '$' -> fail "not inline math"
    _ -> pure ()
  latex <- manyTill' anyChar (char '$')
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
        Just '\n' -> fail "not plain"
        _ -> T.singleton <$> anyChar

    isPlainChar c = not (c `elem` ("*$\n" :: String)) && not (isSpace c && c /= ' ')

-- | Skip horizontal spaces (not newlines)
skipSpaces :: Parser ()
skipSpaces = () <$ many' (satisfy (\c -> isSpace c && c /= '\n'))
