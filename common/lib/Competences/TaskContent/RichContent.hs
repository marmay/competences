{-# LANGUAGE CPP #-}

-- | A newtype for raw text that is expected to be parseable as TaskContent markup.
--
-- This encodes a weak guarantee: the value was either produced from a
-- RichContent AST (serialised back to text) or read from a trusted
-- serialisation source.  It does /not/ require that parsing has actually
-- been performed — it merely records the /expectation/ that
-- 'Competences.Markdown.Parser.parseMarkdown' will succeed.
--
-- The constructor is intentionally hidden.  All creation paths are:
--
-- 1. 'validateRichContent' — parse-checks the text, wraps on success.
-- 2. 'fromTrustedInput'    — wraps unconditionally (frontend editor input).
-- 3. 'mempty' / '<>'       — empty default and composition.
-- 4. Trusted deserialization ('FromJSON', 'ToJSON', 'Binary' via newtype).
module Competences.TaskContent.RichContent
  ( RichContent -- type only, constructor hidden
  , toRawText
  , validateRichContent
  , fromTrustedInput
  ) where

import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.Parser (ParseError)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Raw markup text that is expected to be parseable as 'TaskContent'.
newtype RichContent = RichContent {unRichContent :: Text}
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

instance Semigroup RichContent where
  RichContent a <> RichContent b = RichContent (a <> b)

instance Monoid RichContent where
  mempty = RichContent ""

-- | Extract the raw 'Text' from a 'RichContent' value.
toRawText :: RichContent -> Text
toRawText (RichContent t) = t

-- | Validate that the given text is parseable as markdown.
-- Uses the new megaparsec-based parser for better error messages.
-- Returns 'Left' with a parse error on failure, or wraps into 'RichContent' on success.
validateRichContent :: Text -> Either ParseError RichContent
validateRichContent t = case Markdown.parseMarkdown t of
  Left err -> Left (T.unpack $ Markdown.formatParseError err)
  Right _ -> Right (RichContent t)

-- | Wrap text from a trusted input source (e.g. a frontend textarea)
-- without validation.  The rendering layer handles parse failures gracefully.
fromTrustedInput :: Text -> RichContent
fromTrustedInput = RichContent
