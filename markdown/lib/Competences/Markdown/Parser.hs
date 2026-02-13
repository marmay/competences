-- |
-- Module      : Competences.Markdown.Parser
-- Description : Public API for the markdown parser
--
-- Main entry point for parsing markdown text into an AST.
--
-- @
-- import Competences.Markdown.Parser (parseMarkdown)
-- import Competences.Markdown.AST (Document(..))
--
-- case parseMarkdown "# Hello" of
--   Right doc -> render doc
--   Left err -> showError err
-- @
module Competences.Markdown.Parser
  ( -- * Parsing
    parseMarkdown

    -- * Error formatting
  , ParseError
  , formatParseError
  )
where

import Competences.Markdown.AST (Document (..))
import Competences.Markdown.Parser.Block (documentP)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

-- | Parse error type
type ParseError = ParseErrorBundle Text Void

-- | Parse markdown text into a 'Document' AST.
--
-- Returns 'Left' with a parse error on failure,
-- 'Right' with parsed 'Document' on success.
--
-- Empty or whitespace-only input produces an empty document.
parseMarkdown :: Text -> Either ParseError Document
parseMarkdown input
  | T.null (T.strip input) = Right (Document [])
  | otherwise = parse documentP "markdown" input

-- | Format a parse error into a user-friendly string
formatParseError :: ParseError -> Text
formatParseError = T.pack . errorBundlePretty
