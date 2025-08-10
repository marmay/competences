module Competences.Frontend.Common.Translate
  ( TranslationData
  , Label (..)
  , Language (..)
  , addLanguage
  , extend
  , labelOf
  , loadTranslations
  , merge
  , saveTranslations
  , setCurrentLanguage
  , translate
  , translate'
  , trim
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.IORef (IORef, newIORef, modifyIORef, writeIORef, readIORef)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Miso.String (MisoString, fromMisoString, ms)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (readFile, writeFile)

data Language
  = De
  | En
  deriving (Eq, Ord, Show)

newtype TranslationData = TranslationData
  { unTranslationData :: M.Map Label MisoString
  }
  deriving (Eq, Show)

instance ToJSON TranslationData where
  toJSON = toJSON . map encodeTranslation . M.toList . (.unTranslationData)
    where
      encodeTranslation :: (Label, MisoString) -> (Text, Text)
      encodeTranslation (k, v) = (T.pack (show k), fromMisoString @Text v)

instance FromJSON TranslationData where
  parseJSON = fmap (TranslationData . M.fromList . mapMaybe decodeTranslation) . parseJSON
    where
      decodeTranslation :: (Text, Text) -> Maybe (Label, MisoString)
      decodeTranslation (k, v) = do
        l <- decodeLabel k
        pure (l, ms v)

data Label
  = LblEdit
  | LblDelete
  | LblApply
  | LblCancel
  | LblMove
  | LblInsertBefore
  | LblInsertAfter
  | LblInsertAtTop
  | LblInsertAtBottom
  | LblCompetenceDescription
  | LblCompetenceBasicLevelDescription
  | LblCompetenceIntermediateLevelDescription
  | LblCompetenceAdvancedLevelDescription
  | LblCompetenceBasicLevelPlaceholder
  | LblCompetenceIntermediateLevelPlaceholder
  | LblCompetenceAdvancedLevelPlaceholder
  | LblEditCompetence
  | LblAddNewCompetence
  deriving (Bounded, Eq, Enum, Ord, Show)

defaultLanguage :: Language
defaultLanguage = De

defaultTranslation :: Label -> MisoString
defaultTranslation LblEdit = "Bearbeiten"
defaultTranslation LblDelete = "Löschen"
defaultTranslation LblApply = "Übernehmen"
defaultTranslation LblCancel = "Abbrechen"
defaultTranslation LblMove = "Verschieben"
defaultTranslation LblInsertBefore = "Davor einfügen"
defaultTranslation LblInsertAfter = "Danach einfügen"
defaultTranslation LblInsertAtTop = "Am Anfang einfügen"
defaultTranslation LblInsertAtBottom = "Am Ende einfügen"
defaultTranslation LblCompetenceDescription = "Beschreibung"
defaultTranslation LblCompetenceBasicLevelDescription = "Wesentlich"
defaultTranslation LblCompetenceIntermediateLevelDescription = "Mittelstufe"
defaultTranslation LblCompetenceAdvancedLevelDescription = "Fortgeschritten"
defaultTranslation LblCompetenceBasicLevelPlaceholder = "..."
defaultTranslation LblCompetenceIntermediateLevelPlaceholder = "..."
defaultTranslation LblCompetenceAdvancedLevelPlaceholder = "..."
defaultTranslation LblEditCompetence = "Kompetenz bearbeiten"
defaultTranslation LblAddNewCompetence = "Neue Kompetenz hinzufügen"

currentLanguage :: IORef Language
currentLanguage = unsafePerformIO $ newIORef defaultLanguage
{-# NOINLINE currentLanguage #-}

languages :: IORef (M.Map Language TranslationData)
languages = unsafePerformIO $ newIORef $ M.fromList [(defaultLanguage, defaultTranslationData)]
{-# NOINLINE languages #-}

addLanguage :: Language -> TranslationData -> IO ()
addLanguage l td =
  modifyIORef languages $ M.insert l td

setCurrentLanguage :: Language -> IO ()
setCurrentLanguage = writeIORef currentLanguage

translate :: Language -> Label -> IO MisoString
translate l k = do
  ls <- readIORef languages
  pure $ fromMaybe (defaultTranslation k) $ do
    lang <- ls M.!? l
    lang.unTranslationData M.!? k

translate' :: Label -> MisoString
translate' k = unsafePerformIO $ do
  l <- readIORef currentLanguage
  translate l k

labelOf :: Label -> Text
labelOf = T.pack . show

decodeLabel :: Text -> Maybe Label
decodeLabel t = textToLabelMap M.!? t

trim :: TranslationData -> TranslationData
trim = TranslationData . M.filterWithKey (\k _ -> k `S.member` labels) . (.unTranslationData)

extend :: TranslationData -> TranslationData
extend a = merge a defaultTranslationData

merge :: TranslationData -> TranslationData -> TranslationData
merge a b = TranslationData $ M.union a.unTranslationData b.unTranslationData

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

labels :: S.Set Label
labels = S.fromList [minBound .. maxBound]

textToLabelMap :: M.Map Text Label
textToLabelMap = M.fromList $ map (\l -> (labelOf l, l)) [minBound .. maxBound]

defaultTranslationData :: TranslationData
defaultTranslationData =
  TranslationData $
    M.fromList $
      map (\l -> (l, ms (defaultTranslation l))) [minBound .. maxBound]
