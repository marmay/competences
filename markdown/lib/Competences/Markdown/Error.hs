-- |
-- Module      : Competences.Markdown.Error
-- Description : Custom error types for the markdown parser
--
-- Provides user-friendly error formatting for parse errors.
module Competences.Markdown.Error
  ( formatError
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- | Format a parse error bundle into a user-friendly error message
formatError :: ParseErrorBundle Text Void -> Text
formatError = T.pack . errorBundlePretty
