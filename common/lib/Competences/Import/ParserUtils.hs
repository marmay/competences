-- |
-- Module      : Competences.Import.ParserUtils
-- Description : Shared utilities for import format parsers
--
-- Provides text utilities used by all import parsers
-- (CompetenceGridParser, AssignmentParser, TaskParser).
module Competences.Import.ParserUtils
  ( -- * Types
    ParseError

    -- * Text utilities
  , parseReplacesClause
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Parse error message
type ParseError = String

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
