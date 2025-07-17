module Competences.Frontend.Translate
  ( TranslationData
  , Label (..)
  , extend
  , labelOf
  , loadTranslations
  , merge
  , saveTranslations
  , translate
  , trim
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Miso.String (MisoString, fromMisoString, ms)
import Prelude hiding (readFile, writeFile)
import Control.Exception (SomeException, catch)

newtype TranslationData = TranslationData
  { unTranslationData :: M.Map ByteString MisoString
  }
  deriving (Eq, Show)

instance ToJSON TranslationData where
  toJSON = toJSON . map encodeTranslation . M.toList . (.unTranslationData)
    where
      encodeTranslation :: (ByteString, MisoString) -> (Text, Text)
      encodeTranslation (k, v) = (decodeUtf8 k, fromMisoString @Text v)

instance FromJSON TranslationData where
  parseJSON = fmap (TranslationData . M.fromList . map decodeTranslation) . parseJSON
    where
      decodeTranslation :: (Text, Text) -> (ByteString, MisoString)
      decodeTranslation (k, v) = (encodeUtf8 k, ms v)

data Label
  = LblEdit
  | LblDelete
  deriving (Bounded, Eq, Enum, Ord, Show)

labelOf :: Label -> ByteString
labelOf = \case
  LblEdit -> "edit"
  LblDelete -> "delete"

missingStringOf :: Label -> ByteString
missingStringOf l = "MISSING: " <> labelOf l

missingTranslationData :: TranslationData
missingTranslationData =
  TranslationData $
    M.fromList $
      map (\l -> (labelOf l, ms (missingStringOf l))) [minBound .. maxBound]

labelStrings :: S.Set ByteString
labelStrings = S.fromList $ map labelOf [minBound .. maxBound]

trim :: TranslationData -> TranslationData
trim = TranslationData . M.filterWithKey (\k _ -> k `S.member` labelStrings) . (.unTranslationData)

extend :: TranslationData -> TranslationData
extend a = merge a missingTranslationData

merge :: TranslationData -> TranslationData -> TranslationData
merge a b = TranslationData $ M.union a.unTranslationData b.unTranslationData

translate :: TranslationData -> Label -> MisoString
translate td l = td.unTranslationData M.! labelOf l

loadTranslations :: FilePath -> IO TranslationData
loadTranslations p =
  loadTranslations' p
  `catch` \e -> do
    putStrLn $ "When reading " <> p <> ": " <> show (e :: SomeException)
    putStrLn "Using default translations."
    saveTranslations p missingTranslationData
    pure missingTranslationData

loadTranslations' :: FilePath -> IO TranslationData
loadTranslations' p = decode <$> readFile p >>= \case
  Nothing -> error $ "When reading " <> p <> ": failed to parse translations!"
  Just t -> pure $ extend t

saveTranslations :: FilePath -> TranslationData -> IO ()
saveTranslations p t = writeFile p (encode t)
