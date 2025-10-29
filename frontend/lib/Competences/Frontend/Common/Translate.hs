module Competences.Frontend.Common.Translate
  ( TranslationData
  , Label (..)
  , Language (..)
  , addLanguage
  , extend
  , formatDay
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

import Competences.Document (Level (..))
import Competences.Document.Evidence (Ability (..), ActivityType (..), SocialForm (..), abilities, socialForms)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
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

-- If you change anything here, make sure to also add it to labels' an
-- defaultTranslation.
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
  | LblCompetenceLevelDescription !Level
  | LblCompetenceLevelPlaceholder !Level
  | LblEditCompetence
  | LblEditEvidence
  | LblAddNewCompetence
  | LblAddCompetenceGrid
  | LblAddEvidence
  | LblUserList
  | LblUserName
  | LblUserRole
  | LblAddUser
  | LblInitializing
  | LblCreateEvidence
  | LblPageTitle
  | LblViewCompetenceGrid
  | LblEditCompetenceGrid
  | LblManageUsers
  | LblEvidences
  | LblCollapseEvidenceSelector
  | LblExpandEvidenceSelector
  | LblSocialForm !SocialForm
  | LblAbility !Ability
  | LblGridTitle
  | LblGridDescription
  | LblToday
  | LblThisWeek
  | LblAllTime
  | LblSelectEvidences
  | LblEvidenceDate
  | LblActivityType
  | LblActivityTypeDescription !ActivityType
  | LblActivityTasks
  | LblStudents
  | LblPleaseSelectItem
  | LblNoUser
  | LblPleaseCompleteObservation
  | LblNoMatchingAlternatives
  | LblActivityObservations
  | LblSelectCompetenceGrids
  | LblCompetenceGridTitle
  | LblCompetenceGridDescription
  | LblCompetenceGrid
  deriving (Eq, Ord, Show)

labels' :: [Label]
labels' =
  [ LblEdit
  , LblDelete
  , LblApply
  , LblCancel
  , LblMove
  , LblInsertBefore
  , LblInsertAfter
  , LblInsertAtTop
  , LblInsertAtBottom
  , LblCompetenceDescription
  , LblCompetenceLevelDescription BasicLevel
  , LblCompetenceLevelDescription IntermediateLevel
  , LblCompetenceLevelDescription AdvancedLevel
  , LblCompetenceLevelPlaceholder BasicLevel
  , LblCompetenceLevelPlaceholder IntermediateLevel
  , LblCompetenceLevelPlaceholder AdvancedLevel
  , LblEditCompetence
  , LblEditEvidence
  , LblAddNewCompetence
  , LblAddCompetenceGrid
  , LblAddEvidence
  , LblUserList
  , LblUserName
  , LblUserRole
  , LblAddUser
  , LblInitializing
  , LblCreateEvidence
  , LblPageTitle
  , LblViewCompetenceGrid
  , LblEditCompetenceGrid
  , LblManageUsers
  , LblEvidences
  , LblCollapseEvidenceSelector
  , LblExpandEvidenceSelector
  , LblGridTitle
  , LblGridDescription
  , LblToday
  , LblThisWeek
  , LblAllTime
  , LblSelectEvidences
  , LblEvidenceDate
  , LblActivityType
  , LblActivityTypeDescription Supervised
  , LblActivityTypeDescription SemiSupervised
  , LblActivityTypeDescription Unsupervised
  , LblActivityTasks
  , LblStudents
  , LblPleaseSelectItem
  , LblNoUser
  , LblPleaseCompleteObservation
  , LblNoMatchingAlternatives
  , LblActivityObservations
  , LblSelectCompetenceGrids
  , LblCompetenceGridTitle
  , LblCompetenceGridDescription
  , LblCompetenceGrid
  ]
    <> map LblSocialForm socialForms
    <> map LblAbility abilities

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
defaultTranslation (LblCompetenceLevelDescription BasicLevel) = "Wesentlich"
defaultTranslation (LblCompetenceLevelDescription IntermediateLevel) = "Mittelstufe"
defaultTranslation (LblCompetenceLevelDescription AdvancedLevel) = "Fortgeschritten"
defaultTranslation (LblCompetenceLevelPlaceholder BasicLevel) = "..."
defaultTranslation (LblCompetenceLevelPlaceholder IntermediateLevel) = "..."
defaultTranslation (LblCompetenceLevelPlaceholder AdvancedLevel) = "..."
defaultTranslation LblEditCompetence = "Kompetenz bearbeiten"
defaultTranslation LblEditEvidence = "Aufzeichnung bearbeiten"
defaultTranslation LblAddNewCompetence = "Neue Kompetenz hinzufügen"
defaultTranslation LblAddCompetenceGrid = "Kompetenzraster hinzufügen"
defaultTranslation LblAddEvidence = "Beobachtung hinzufügen"
defaultTranslation LblUserList = "Liste aller Benutzer"
defaultTranslation LblUserName = "Benutzername"
defaultTranslation LblUserRole = "Benutzerrolle"
defaultTranslation LblAddUser = "Benutzer hinzufügen"
defaultTranslation LblInitializing = "Initialisiere ..."
defaultTranslation LblCreateEvidence = "Aufzeichnung erstellen"
defaultTranslation LblPageTitle = "Kompetenzerfassung"
defaultTranslation LblViewCompetenceGrid = "Kompetenzraster anzeigen"
defaultTranslation LblEditCompetenceGrid = "Kompetenzraster bearbeiten"
defaultTranslation LblManageUsers = "Benutzer verwalten"
defaultTranslation LblEvidences = "Aufzeichnungen"
defaultTranslation LblCollapseEvidenceSelector = "Aufzeichnungen einklappen"
defaultTranslation LblExpandEvidenceSelector = "Aufzeichungen ausklappen"
defaultTranslation (LblSocialForm Individual) = "Individuell"
defaultTranslation (LblSocialForm Group) = "Gruppe"
defaultTranslation (LblAbility SelfReliant) = "selbstständig"
defaultTranslation (LblAbility SelfReliantWithSillyMistakes) = "dumme Fehler"
defaultTranslation (LblAbility WithSupport) = "mit Unterstützung"
defaultTranslation (LblAbility NotYet) = "noch nicht"
defaultTranslation LblGridTitle = "Titel"
defaultTranslation LblGridDescription = "Beschreibung"
defaultTranslation LblToday = "Heute"
defaultTranslation LblThisWeek = "Diese Woche"
defaultTranslation LblAllTime = "Gesamt"
defaultTranslation LblSelectEvidences = "Aufzeichnungen"
defaultTranslation LblEvidenceDate = "Datum"
defaultTranslation LblActivityType = "Art der Aktivität"
defaultTranslation (LblActivityTypeDescription Supervised) = "Beaufsichtigt"
defaultTranslation (LblActivityTypeDescription SemiSupervised) = "Betreut"
defaultTranslation (LblActivityTypeDescription Unsupervised) = "Selbstständig"
defaultTranslation LblActivityTasks = "Bearbeitete Aufgaben"
defaultTranslation LblActivityObservations = "Gemachte Beobachtungen"
defaultTranslation LblStudents = "Schüler"
defaultTranslation LblPleaseSelectItem = "Bitte wählen Sie ein zu bearbeitendes Element aus!"
defaultTranslation LblNoUser = "Kein Benutzer"
defaultTranslation LblPleaseCompleteObservation = "Bitte vervollständige die Beobachtung zuerst!"
defaultTranslation LblNoMatchingAlternatives = "Bitte wähle eine gültige Alternative aus!"
defaultTranslation LblSelectCompetenceGrids = "Kompetenzraster"
defaultTranslation LblCompetenceGridTitle = "Titel"
defaultTranslation LblCompetenceGridDescription = "Beschreibung"
defaultTranslation LblCompetenceGrid = "Kompetenzraster"

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

formatDay :: Day -> MisoString
formatDay d = ms $ formatTime defaultTimeLocale "%d.%m.%Y" d

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
  readFile p
    >>= ( \case
            Nothing -> error $ "When reading " <> p <> ": failed to parse translations!"
            Just t -> pure $ extend t
        )
      . decode

saveTranslations :: FilePath -> TranslationData -> IO ()
saveTranslations p t = writeFile p (encode t)

labels :: S.Set Label
labels = S.fromList labels'

textToLabelMap :: M.Map Text Label
textToLabelMap = M.fromList $ map (\l -> (labelOf l, l)) labels'

defaultTranslationData :: TranslationData
defaultTranslationData =
  TranslationData $
    M.fromList $
      map (\l -> (l, ms (defaultTranslation l))) labels'
