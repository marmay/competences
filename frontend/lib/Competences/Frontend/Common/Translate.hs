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
import Competences.Document.Task (TaskPurpose (..), taskPurposes)
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
  | LblNotAchieved
  | LblEditCompetence
  | LblEditEvidence
  | LblAddNewCompetence
  | LblAddCompetenceGrid
  | LblAddEvidence
  | LblUserList
  | LblUserName
  | LblUserRole
  | LblUserEmail
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
  | LblStudent
  | LblPleaseSelectItem
  | LblPleaseSelectItemShort
  | LblNoUser
  | LblPleaseCompleteObservation
  | LblNoMatchingAlternatives
  | LblActivityObservations
  | LblSelectCompetenceGrids
  | LblCompetenceGridTitle
  | LblCompetenceGridDescription
  | LblCompetenceGrid
  | LblStatistics
  | LblAnalytics
  | LblStatisticsOverview
  | LblStatisticsIndividual
  | LblTotalExercises
  | LblTotalObservations
  | LblSelfContainedTasks
  | LblNewTask
  | LblNewTaskGroup
  | LblTasksAndGroups
  | LblEditTaskGroup
  | LblEditSubTask
  | LblEditSelfContainedTask
  | LblTaskGroupContentBefore
  | LblTaskGroupContentAfter
  | LblSubTasks
  | LblAddSubTask
  | LblNoSubTasks
  | LblFilterTasks
  | LblFilterAssignments
  | LblTaskIdentifier
  | LblTaskContent
  | LblTaskPrimaryCompetences
  | LblTaskSecondaryCompetences
  | LblTaskPurposeLabel
  | LblTaskDisplayInResources
  | LblInherit
  | LblYes
  | LblNo
  | LblOverrideCompetences
  | LblNoCompetences
  | LblTaskPurpose !TaskPurpose
  | LblAssignments
  | LblNewAssignment
  | LblEditAssignment
  | LblEvaluateAssignments
  | LblAssignmentName
  | LblAssignmentDate
  | LblAssignmentTasks
  | LblNoStudentsSelected
  | LblNoStudentSelected
  | LblNoTasksSelected
  | LblSelectAll
  | LblDeselectAll
  | LblView
  | LblEvaluate
  | LblAssess
  | LblFocusedStudent
  | LblNoStudentFocused
  | LblAllStudents
    -- Connection status
  | LblConnected
  | LblDisconnected
  | LblReconnecting !Int
  | LblPendingChanges !Int
  | LblUnsavedChanges !Int
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
  , LblNotAchieved
  , LblEditCompetence
  , LblEditEvidence
  , LblAddNewCompetence
  , LblAddCompetenceGrid
  , LblAddEvidence
  , LblUserList
  , LblUserName
  , LblUserRole
  , LblUserEmail
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
  , LblActivityTypeDescription Conversation
  , LblActivityTypeDescription Exam
  , LblActivityTypeDescription SchoolExercise
  , LblActivityTypeDescription HomeExercise
  , LblActivityTasks
  , LblStudents
  , LblStudent
  , LblPleaseSelectItem
  , LblPleaseSelectItemShort
  , LblNoUser
  , LblPleaseCompleteObservation
  , LblNoMatchingAlternatives
  , LblActivityObservations
  , LblSelectCompetenceGrids
  , LblCompetenceGridTitle
  , LblCompetenceGridDescription
  , LblCompetenceGrid
  , LblStatistics
  , LblAnalytics
  , LblStatisticsOverview
  , LblStatisticsIndividual
  , LblTotalExercises
  , LblTotalObservations
  , LblSelfContainedTasks
  , LblNewTask
  , LblNewTaskGroup
  , LblTasksAndGroups
  , LblEditTaskGroup
  , LblEditSubTask
  , LblEditSelfContainedTask
  , LblTaskGroupContentBefore
  , LblTaskGroupContentAfter
  , LblSubTasks
  , LblAddSubTask
  , LblNoSubTasks
  , LblFilterTasks
  , LblFilterAssignments
  , LblTaskIdentifier
  , LblTaskContent
  , LblTaskPrimaryCompetences
  , LblTaskSecondaryCompetences
  , LblTaskPurposeLabel
  , LblTaskDisplayInResources
  , LblInherit
  , LblYes
  , LblNo
  , LblOverrideCompetences
  , LblNoCompetences
  , LblAssignments
  , LblNewAssignment
  , LblEditAssignment
  , LblEvaluateAssignments
  , LblAssignmentName
  , LblAssignmentDate
  , LblAssignmentTasks
  , LblNoStudentsSelected
  , LblNoStudentSelected
  , LblNoTasksSelected
  , LblSelectAll
  , LblDeselectAll
  , LblView
  , LblEvaluate
  , LblAssess
  , LblFocusedStudent
  , LblNoStudentFocused
  , LblAllStudents
    -- Connection status
  , LblConnected
  , LblDisconnected
  , LblReconnecting 0
  , LblPendingChanges 0
  , LblUnsavedChanges 0
  ]
    <> map LblSocialForm socialForms
    <> map LblAbility abilities
    <> map LblTaskPurpose taskPurposes

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
defaultTranslation LblNotAchieved = "Nicht erreicht"
defaultTranslation LblEditCompetence = "Kompetenz bearbeiten"
defaultTranslation LblEditEvidence = "Aufzeichnung bearbeiten"
defaultTranslation LblAddNewCompetence = "Neue Kompetenz hinzufügen"
defaultTranslation LblAddCompetenceGrid = "Kompetenzraster hinzufügen"
defaultTranslation LblAddEvidence = "Beobachtung hinzufügen"
defaultTranslation LblUserList = "Liste aller Benutzer"
defaultTranslation LblUserName = "Benutzername"
defaultTranslation LblUserRole = "Benutzerrolle"
defaultTranslation LblUserEmail = "E-Mail"
defaultTranslation LblAddUser = "Benutzer hinzufügen"
defaultTranslation LblInitializing = "Initialisiere ..."
defaultTranslation LblCreateEvidence = "Aufzeichnung erstellen"
defaultTranslation LblPageTitle = "Meine Kompetenzen"
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
defaultTranslation (LblActivityTypeDescription Conversation) = "Gespräch"
defaultTranslation (LblActivityTypeDescription Exam) = "Test"
defaultTranslation (LblActivityTypeDescription SchoolExercise) = "Schulübung"
defaultTranslation (LblActivityTypeDescription HomeExercise) = "Hausübung"
defaultTranslation LblActivityTasks = "Bearbeitete Aufgaben"
defaultTranslation LblActivityObservations = "Gemachte Beobachtungen"
defaultTranslation LblStudents = "Schüler"
defaultTranslation LblStudent = "Schüler"
defaultTranslation LblPleaseSelectItem = "Bitte wählen Sie ein zu bearbeitendes Element aus!"
defaultTranslation LblPleaseSelectItemShort = "Bitte Element auswählen!"
defaultTranslation LblNoUser = "Kein Benutzer"
defaultTranslation LblPleaseCompleteObservation = "Bitte vervollständige die Beobachtung zuerst!"
defaultTranslation LblNoMatchingAlternatives = "Bitte wähle eine gültige Alternative aus!"
defaultTranslation LblSelectCompetenceGrids = "Kompetenzraster"
defaultTranslation LblCompetenceGridTitle = "Titel"
defaultTranslation LblCompetenceGridDescription = "Beschreibung"
defaultTranslation LblCompetenceGrid = "Kompetenzraster"
defaultTranslation LblStatistics = "Statistik"
defaultTranslation LblAnalytics = "Analytics"
defaultTranslation LblStatisticsOverview = "Statistik-Überblick"
defaultTranslation LblStatisticsIndividual = "Meine Statistik"
defaultTranslation LblTotalExercises = "Gesamtanzahl Übungen"
defaultTranslation LblTotalObservations = "Gesamtanzahl Beobachtungen"
defaultTranslation LblSelfContainedTasks = "Aufgaben"
defaultTranslation LblNewTask = "Neue Aufgabe"
defaultTranslation LblNewTaskGroup = "Neue Aufgabengruppe"
defaultTranslation LblTasksAndGroups = "Aufgaben"
defaultTranslation LblEditTaskGroup = "Aufgabengruppe bearbeiten"
defaultTranslation LblEditSubTask = "Unteraufgabe bearbeiten"
defaultTranslation LblEditSelfContainedTask = "Aufgabe bearbeiten"
defaultTranslation LblTaskGroupContentBefore = "Inhalt davor"
defaultTranslation LblTaskGroupContentAfter = "Inhalt danach"
defaultTranslation LblSubTasks = "Unteraufgaben"
defaultTranslation LblAddSubTask = "+ Unteraufgabe"
defaultTranslation LblNoSubTasks = "Keine Unteraufgaben"
defaultTranslation LblFilterTasks = "Nach Bezeichnung filtern..."
defaultTranslation LblFilterAssignments = "Nach Name filtern..."
defaultTranslation LblTaskIdentifier = "Bezeichnung"
defaultTranslation LblTaskContent = "Inhalt"
defaultTranslation LblTaskPrimaryCompetences = "Primäre Kompetenzen"
defaultTranslation LblTaskSecondaryCompetences = "Sekundäre Kompetenzen"
defaultTranslation LblTaskPurposeLabel = "Zweck"
defaultTranslation LblTaskDisplayInResources = "In Ressourcen anzeigen"
defaultTranslation LblInherit = "Von Gruppe erben"
defaultTranslation LblYes = "Ja"
defaultTranslation LblNo = "Nein"
defaultTranslation LblOverrideCompetences = "Kompetenzen überschreiben"
defaultTranslation LblNoCompetences = "Keine Kompetenzen"
defaultTranslation (LblTaskPurpose Practice) = "Übung"
defaultTranslation (LblTaskPurpose Assessment) = "Beurteilung"
defaultTranslation LblAssignments = "Aufträge"
defaultTranslation LblNewAssignment = "+ Neuer Auftrag"
defaultTranslation LblEditAssignment = "Auftrag bearbeiten"
defaultTranslation LblEvaluateAssignments = "Aufträge auswerten"
defaultTranslation LblAssignmentName = "Name"
defaultTranslation LblAssignmentDate = "Datum"
defaultTranslation LblAssignmentTasks = "Aufgaben"
defaultTranslation LblNoStudentsSelected = "Keine Schüler ausgewählt"
defaultTranslation LblNoStudentSelected = "Kein Schüler ausgewählt"
defaultTranslation LblNoTasksSelected = "Keine Aufgaben ausgewählt"
defaultTranslation LblSelectAll = "Alle auswählen"
defaultTranslation LblDeselectAll = "Alle abwählen"
defaultTranslation LblView = "Anzeigen"
defaultTranslation LblEvaluate = "Auswerten"
defaultTranslation LblAssess = "Beurteilen"
defaultTranslation LblFocusedStudent = "Fokussierter Schüler"
defaultTranslation LblNoStudentFocused = "Kein Schüler fokussiert"
defaultTranslation LblAllStudents = "Alle Schüler"
-- Connection status
defaultTranslation LblConnected = "Verbunden"
defaultTranslation LblDisconnected = "Getrennt"
defaultTranslation (LblReconnecting n) = "Verbinde... (Versuch " <> ms (show n) <> ")"
defaultTranslation (LblPendingChanges n) = ms (show n) <> " Änderungen werden gesendet"
defaultTranslation (LblUnsavedChanges n) = ms (show n) <> " ungespeicherte Änderungen"

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
