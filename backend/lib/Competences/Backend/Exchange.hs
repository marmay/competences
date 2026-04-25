-- | Pure server-side codec between the structured 'ExchangeDoc' IR
-- (Binary on the wire) and YAML text. Holds no document state — the
-- frontend builds 'ExchangeDoc' values from its own document and posts
-- them here for translation.
module Competences.Backend.Exchange
  ( exchangeToYaml
  , exchangeFromYaml
  )
where

import Competences.Exchange.Types (ExchangeDoc)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml qualified as Yaml

-- | YAML-encode an 'ExchangeDoc' via its 'ToJSON' instance.
exchangeToYaml :: ExchangeDoc -> Text
exchangeToYaml = decodeUtf8 . Yaml.encode

-- | Parse YAML text into an 'ExchangeDoc'. Returns a short, user-facing
-- error message on failure.
exchangeFromYaml :: Text -> Either Text ExchangeDoc
exchangeFromYaml txt =
  case Yaml.decodeEither' (encodeUtf8 txt) of
    Left err -> Left ("YAML parse error: " <> T.pack (Yaml.prettyPrintParseException err))
    Right xdoc -> Right xdoc
