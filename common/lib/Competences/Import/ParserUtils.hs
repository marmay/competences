{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Import.ParserUtils
-- Description : Shared parser utilities for import format parsers
--
-- Provides common megaparsec-based combinators used by all import parsers
-- (CompetenceGridParser, AssignmentParser, TaskParser).
module Competences.Import.ParserUtils
  ( -- * Types
    Parser
  , ParseError

    -- * Running parsers
  , runParser'

    -- * Combinators
  , takeLineContent
  , skipHorizontalSpace
  , skipBlankLines

    -- * Text utilities
  , parseReplacesClause
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError, runParser')
import Text.Megaparsec.Char (eol, space)

-- | Parser type for import format parsers
type Parser = Parsec Void Text

-- | Parse error message
type ParseError = String

-- | Run a parser, converting megaparsec errors to String
runParser' :: Parser a -> Text -> Either ParseError a
runParser' p input = case parse p "" input of
  Left err -> Left (errorBundlePretty err)
  Right x -> Right x

-- | Take content until end of line (consumes the newline)
takeLineContent :: Parser Text
takeLineContent = do
  content <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  _ <- optional eol
  pure content

-- | Skip horizontal whitespace (spaces and tabs, not newlines)
skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipMany (satisfy (\c -> c == ' ' || c == '\t'))

-- | Skip blank lines and whitespace
skipBlankLines :: Parser ()
skipBlankLines = space

-- | Parse (Ersetzt: original) clause from description
-- Returns (new description, Maybe original)
parseReplacesClause :: Text -> (Text, Maybe Text)
parseReplacesClause input =
  case T.breakOn "(Ersetzt:" input of
    (before, after)
      | T.null after -> (input, Nothing)
      | otherwise ->
          let -- Remove "(Ersetzt:" prefix and trailing ")"
              original = T.strip $ T.dropEnd 1 $ T.drop 9 after
           in (T.strip before, Just original)
