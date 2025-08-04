module Competences.Frontend.Common.Translate
  ( TranslationData
  , Label (..)
  , extend
  , labelOf
  , loadTranslations
  , merge
  , saveTranslations
  , translate
  , translate'
  , trim
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Records (HasField)
import Miso.String (MisoString, fromMisoString, ms)
import Prelude hiding (readFile, writeFile)

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
  | LblApplyChange
  | LblCancelChange
  | LblMove
  | LblInsertBefore
  | LblInsertAfter
  | LblInsertAtTop
  | LblInsertAtBottom
  | LblCompetenceDescription
  | LblCompetenceBasicLevelDescription
  | LblCompetenceIntermediateLevelDescription
  | LblCompetenceAdvancedLevelDescription
  | LblEditCompetence
  | LblAddNewCompetence
  deriving (Bounded, Eq, Enum, Ord, Show)

defaultTranslations :: [(Label, Text)]
defaultTranslations =
  [ (LblEdit, "Bearbeiten")
  , (LblDelete, "Löschen")
  , (LblApplyChange, "Übernehmen")
  , (LblCancelChange, "Abbrechen")
  , (LblMove, "Verschieben")
  , (LblInsertBefore, "Davor einfügen")
  , (LblInsertAfter, "Danach einfügen")
  , (LblInsertAtTop, "Am Anfang einfügen")
  , (LblInsertAtBottom, "Am Ende einfügen")
  , (LblCompetenceDescription, "Beschreibung")
  , (LblCompetenceBasicLevelDescription, "Wesentlich")
  , (LblCompetenceIntermediateLevelDescription, "Mittelstufe")
  , (LblCompetenceAdvancedLevelDescription, "Fortgeschritten")
  , (LblEditCompetence, "Kompetenz bearbeiten")
  , (LblAddNewCompetence, "Neue Kompetenz hinzufügen")
  ]

labelOf :: Label -> ByteString
labelOf = pack . show

defaultTranslationData :: TranslationData
defaultTranslationData =
  TranslationData $
    M.fromList $
      map (\l -> (labelOf l, defaultTranslation l)) [minBound .. maxBound]
  where
    defaultTranslationsMap :: M.Map Label Text
    defaultTranslationsMap = Map.fromList $ defaultTranslations
    defaultTranslation :: Label -> Text
    defaultTranslation l = ms $ fromMaybe (missingStringOf l) $ Map.lookup l defaultTranslationsMap
    missingStringOf :: Label -> Text
    missingStringOf l = "MISSING: " <> decodeUtf8 (labelOf l)

labelStrings :: S.Set ByteString
labelStrings = S.fromList $ map labelOf [minBound .. maxBound]

trim :: TranslationData -> TranslationData
trim = TranslationData . M.filterWithKey (\k _ -> k `S.member` labelStrings) . (.unTranslationData)

extend :: TranslationData -> TranslationData
extend a = merge a defaultTranslationData

merge :: TranslationData -> TranslationData -> TranslationData
merge a b = TranslationData $ M.union a.unTranslationData b.unTranslationData

translate' :: TranslationData -> Label -> MisoString
translate' td l = td.unTranslationData M.! labelOf l

translate
  :: forall s
   . (HasField "translationData" s TranslationData)
  => s -> Label -> MisoString
translate s = translate' s.translationData

loadTranslations :: FilePath -> IO TranslationData
loadTranslations p =
  loadTranslations' p
    `catch` \e -> do
      putStrLn $ "When reading " <> p <> ": " <> show (e :: SomeException)
      putStrLn "Using default translations."
      saveTranslations p defaultTranslationData
      pure defaultTranslationData

loadTranslations' :: FilePath -> IO TranslationData
loadTranslations' p =
  decode <$> readFile p >>= \case
    Nothing -> error $ "When reading " <> p <> ": failed to parse translations!"
    Just t -> pure $ extend t

saveTranslations :: FilePath -> TranslationData -> IO ()
saveTranslations p t = writeFile p (encode t)

data DefaultTranslation = DefaultTranslation
  { label :: !Label
  , translation :: !Text
  }
  deriving (Eq, Show)
