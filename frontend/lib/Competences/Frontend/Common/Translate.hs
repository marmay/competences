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
import Competences.Document.LessonPlan (ActionForm (..), TeachingSocialForm (..))
import Competences.Document.Solution (SolutionType (..), solutionTypes)
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
  | LblPreview
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
  | LblStudentOverview
  | LblAddToSelectedStudents
  | LblPleaseSelectItem
  | LblPleaseSelectItemShort
  | LblNoUser
  | LblPleaseCompleteObservation
  | LblNoMatchingAlternatives
  | LblActivityObservations
  | LblSelectCompetenceGrids
  | LblCompetenceGridTitle
  | LblCompetenceGridDescription
  | LblCompetenceGridDateFrom
  | LblCompetenceGridDateTo
  | LblCompetenceGridExpectedLessons
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
  | LblAssignmentDescription
  | LblAssignmentDate
  | LblAssignmentTasks
  | LblNoStudentsSelected
  | LblNoStudentSelected
  | LblNoTasksSelected
  | LblNoAssignmentSelected
  | LblSelectAssignment
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
  | LblLockedLevels
    -- Grid grading
  | LblGrade
  | LblGradeHistory
  | LblEnterGrade
  | LblGradeComment
    -- Bulk evidence editor
  | LblNewEvidence
  | LblBulkEntry
  | LblBulkEvidenceEntry
  | LblAddObservation
  | LblSave
  | LblSaveAll
  | LblNoObservationsYet
  | LblCompetence
  | LblLevel
    -- Solutions
  | LblSolutions
  | LblFilterSolutions
  | LblEditSolution
  | LblSolutionTask
  | LblSolutionTypeLabel
  | LblSolutionContent
  | LblSolutionType !SolutionType
  | LblAddSolution
  | LblNoSolutions
    -- Resources
  | LblResources
  | LblMaterials
  | LblTasks
  | LblLearningResources
  | LblNoTasksAvailable
    -- Resource Editor
  | LblEditResource
  | LblResourceIdentifier
  | LblResourceCompetenceLevels
  | LblResourceContent
  | LblInlineContent
  | LblWebLink
  | LblVideoLink
  | LblAddResource
  | LblNoResources
  | LblManageResources
  | LblUrl
  | LblDescription
    -- Import
  | LblCreate
  | LblImportCompetenceGrids
  | LblImportTasks
  | LblImportAssignments
  | LblExport
    -- Analytics
  | LblMasteryStreakTwoPlus
  | LblMasteryOneSuccess
  | LblMasteryOnlySillyMistakes
  | LblMasteryNotYet
  | LblMasteryNotTried
    -- Meso Planning
  | LblMesoPlanning
    -- Lesson Planning
  | LblLessonPlan
  | LblLessonPlanDate
  | LblLessonPlanAssignments
  | LblLessonPlanResources
  | LblLessonPlanPhases
  | LblLessonPlanNotes
  | LblCreateLessonPlan
  | LblNoLessonPlan
  | LblNoNotes
  | LblPhaseTitle
  | LblPhaseSocialForm
  | LblPhaseDuration
  | LblPhaseActionForm
  | LblPhaseNotes
  | LblAddPhase
  | LblNoPhases
  | LblTeachingSocialForm !TeachingSocialForm
  | LblActionForm !ActionForm
  | LblMesoPlanTitle
  | LblMesoPlanEntry
  | LblMesoPlanEntryTitle
  | LblMesoPlanEntryDescription
  | LblMesoPlanEntryCompetences
  | LblAddMesoPlanEntry
  | LblNoMesoPlanEntries
  | LblCreateMesoPlan
  deriving (Eq, Ord, Show)

labels' :: [Label]
labels' =
  [ LblEdit
  , LblDelete
  , LblApply
  , LblCancel
  , LblPreview
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
  , LblStudentOverview
  , LblAddToSelectedStudents
  , LblPleaseSelectItem
  , LblPleaseSelectItemShort
  , LblNoUser
  , LblPleaseCompleteObservation
  , LblNoMatchingAlternatives
  , LblActivityObservations
  , LblSelectCompetenceGrids
  , LblCompetenceGridTitle
  , LblCompetenceGridDescription
  , LblCompetenceGridDateFrom
  , LblCompetenceGridDateTo
  , LblCompetenceGridExpectedLessons
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
  , LblAssignmentDescription
  , LblAssignmentDate
  , LblAssignmentTasks
  , LblNoStudentsSelected
  , LblNoStudentSelected
  , LblNoTasksSelected
  , LblNoAssignmentSelected
  , LblSelectAssignment
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
  , LblLockedLevels
    -- Grid grading
  , LblGrade
  , LblGradeHistory
  , LblEnterGrade
  , LblGradeComment
    -- Bulk evidence editor
  , LblNewEvidence
  , LblBulkEntry
  , LblBulkEvidenceEntry
  , LblAddObservation
  , LblSave
  , LblSaveAll
  , LblNoObservationsYet
  , LblCompetence
  , LblLevel
    -- Solutions
  , LblSolutions
  , LblFilterSolutions
  , LblEditSolution
  , LblSolutionTask
  , LblSolutionTypeLabel
  , LblSolutionContent
  , LblAddSolution
  , LblNoSolutions
    -- Resources
  , LblResources
  , LblMaterials
  , LblTasks
  , LblLearningResources
  , LblNoTasksAvailable
    -- Resource Editor
  , LblEditResource
  , LblResourceIdentifier
  , LblResourceCompetenceLevels
  , LblResourceContent
  , LblInlineContent
  , LblWebLink
  , LblVideoLink
  , LblAddResource
  , LblNoResources
  , LblManageResources
  , LblUrl
  , LblDescription
    -- Import
  , LblCreate
  , LblImportCompetenceGrids
  , LblImportTasks
  , LblImportAssignments
  , LblExport
    -- Analytics
  , LblMasteryStreakTwoPlus
  , LblMasteryOneSuccess
  , LblMasteryOnlySillyMistakes
  , LblMasteryNotYet
  , LblMasteryNotTried
    -- Meso Planning
  , LblMesoPlanning
  , LblMesoPlanTitle
  , LblMesoPlanEntry
  , LblMesoPlanEntryTitle
  , LblMesoPlanEntryDescription
  , LblMesoPlanEntryCompetences
  , LblAddMesoPlanEntry
  , LblNoMesoPlanEntries
  , LblCreateMesoPlan
    -- Lesson Planning
  , LblLessonPlan
  , LblLessonPlanDate
  , LblLessonPlanAssignments
  , LblLessonPlanResources
  , LblLessonPlanPhases
  , LblLessonPlanNotes
  , LblCreateLessonPlan
  , LblNoLessonPlan
  , LblNoNotes
  , LblPhaseTitle
  , LblPhaseSocialForm
  , LblPhaseDuration
  , LblPhaseActionForm
  , LblPhaseNotes
  , LblAddPhase
  , LblNoPhases
  , LblTeachingSocialForm WholeClass
  , LblTeachingSocialForm SmallGroups
  , LblTeachingSocialForm PairWork
  , LblTeachingSocialForm IndividualWork
  , LblActionForm Presenting
  , LblActionForm Collaborating
  , LblActionForm Assigning
  ]
    <> map LblSocialForm socialForms
    <> map LblAbility abilities
    <> map LblTaskPurpose taskPurposes
    <> map LblSolutionType solutionTypes

defaultLanguage :: Language
defaultLanguage = De

defaultTranslation :: Label -> MisoString
defaultTranslation LblEdit = "Bearbeiten"
defaultTranslation LblDelete = "Löschen"
defaultTranslation LblApply = "Übernehmen"
defaultTranslation LblCancel = "Abbrechen"
defaultTranslation LblPreview = "Vorschau"
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
defaultTranslation LblStudentOverview = "Schülerübersicht"
defaultTranslation LblAddToSelectedStudents = "Zu ausgewählten Schülern hinzufügen"
defaultTranslation LblPleaseSelectItem = "Bitte wählen Sie ein zu bearbeitendes Element aus!"
defaultTranslation LblPleaseSelectItemShort = "Bitte Element auswählen!"
defaultTranslation LblNoUser = "Kein Benutzer"
defaultTranslation LblPleaseCompleteObservation = "Bitte vervollständige die Beobachtung zuerst!"
defaultTranslation LblNoMatchingAlternatives = "Bitte wähle eine gültige Alternative aus!"
defaultTranslation LblSelectCompetenceGrids = "Kompetenzraster"
defaultTranslation LblCompetenceGridTitle = "Titel"
defaultTranslation LblCompetenceGridDescription = "Beschreibung"
defaultTranslation LblCompetenceGridDateFrom = "Beginn"
defaultTranslation LblCompetenceGridDateTo = "Ende"
defaultTranslation LblCompetenceGridExpectedLessons = "Erwartete Stunden"
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
defaultTranslation LblAddSubTask = "Unteraufgabe"
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
defaultTranslation LblNewAssignment = "Neuer Auftrag"
defaultTranslation LblEditAssignment = "Auftrag bearbeiten"
defaultTranslation LblEvaluateAssignments = "Aufträge auswerten"
defaultTranslation LblAssignmentName = "Name"
defaultTranslation LblAssignmentDescription = "Beschreibung"
defaultTranslation LblAssignmentDate = "Datum"
defaultTranslation LblAssignmentTasks = "Aufgaben"
defaultTranslation LblNoStudentsSelected = "Keine Schüler ausgewählt"
defaultTranslation LblNoStudentSelected = "Kein Schüler ausgewählt"
defaultTranslation LblNoTasksSelected = "Keine Aufgaben ausgewählt"
defaultTranslation LblNoAssignmentSelected = "Kein Auftrag ausgewählt"
defaultTranslation LblSelectAssignment = "Auftrag auswählen..."
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
defaultTranslation LblLockedLevels = "Gesperrt"
-- Grid grading
defaultTranslation LblGrade = "Benotung"
defaultTranslation LblGradeHistory = "Notenhistorie"
defaultTranslation LblEnterGrade = "Note eingeben"
defaultTranslation LblGradeComment = "Kommentar..."
-- Bulk evidence editor
defaultTranslation LblNewEvidence = "Neue Aufzeichnung"
defaultTranslation LblBulkEntry = "Sammelerfassung"
defaultTranslation LblBulkEvidenceEntry = "Sammelerfassung"
defaultTranslation LblAddObservation = "Beobachtung hinzufügen"
defaultTranslation LblSave = "Speichern"
defaultTranslation LblSaveAll = "Alle speichern"
defaultTranslation LblNoObservationsYet = "Noch keine Beobachtungen"
defaultTranslation LblCompetence = "Kompetenz"
defaultTranslation LblLevel = "Stufe"
-- Solutions
defaultTranslation LblSolutions = "Lösungen"
defaultTranslation LblFilterSolutions = "Nach Aufgabe filtern..."
defaultTranslation LblEditSolution = "Lösung bearbeiten"
defaultTranslation LblSolutionTask = "Aufgabe"
defaultTranslation LblSolutionTypeLabel = "Lösungstyp"
defaultTranslation LblSolutionContent = "Inhalt"
defaultTranslation (LblSolutionType Hint) = "Hinweis"
defaultTranslation (LblSolutionType Results) = "Ergebnis"
defaultTranslation (LblSolutionType Complete) = "Vollständig"
defaultTranslation LblAddSolution = "Lösung hinzufügen"
defaultTranslation LblNoSolutions = "Keine Lösungen"
-- Resources
defaultTranslation LblResources = "Ressourcen"
defaultTranslation LblMaterials = "Materialien"
defaultTranslation LblTasks = "Aufgaben"
defaultTranslation LblLearningResources = "Lernmaterial"
defaultTranslation LblNoTasksAvailable = "Keine Aufgaben verfügbar"
-- Resource Editor
defaultTranslation LblEditResource = "Ressource bearbeiten"
defaultTranslation LblResourceIdentifier = "Bezeichnung"
defaultTranslation LblResourceCompetenceLevels = "Kompetenzstufen"
defaultTranslation LblResourceContent = "Inhalt"
defaultTranslation LblInlineContent = "Inline-Inhalt"
defaultTranslation LblWebLink = "Web-Link"
defaultTranslation LblVideoLink = "Video-Link"
defaultTranslation LblAddResource = "Ressource hinzufügen"
defaultTranslation LblNoResources = "Keine Ressourcen"
defaultTranslation LblManageResources = "Ressourcen verwalten"
defaultTranslation LblUrl = "URL"
defaultTranslation LblDescription = "Beschreibung"
-- Import
defaultTranslation LblCreate = "Erstellen"
defaultTranslation LblImportCompetenceGrids = "Kompetenzraster importieren"
defaultTranslation LblImportTasks = "Aufgaben importieren"
defaultTranslation LblImportAssignments = "Aufgaben importieren"
defaultTranslation LblExport = "Exportieren"
-- Analytics
defaultTranslation LblMasteryStreakTwoPlus = "Sicher (2+ Erfolge)"
defaultTranslation LblMasteryOneSuccess = "Erfolgreich (1 Erfolg)"
defaultTranslation LblMasteryOnlySillyMistakes = "Dumme Fehler"
defaultTranslation LblMasteryNotYet = "Noch nicht"
defaultTranslation LblMasteryNotTried = "Nicht versucht"
-- Meso Planning
defaultTranslation LblMesoPlanning = "Planung"
defaultTranslation LblMesoPlanTitle = "Titel"
defaultTranslation LblMesoPlanEntry = "Unterrichtseinheit"
defaultTranslation LblMesoPlanEntryTitle = "Titel"
defaultTranslation LblMesoPlanEntryDescription = "Beschreibung"
defaultTranslation LblMesoPlanEntryCompetences = "Kompetenzstufen"
defaultTranslation LblAddMesoPlanEntry = "Einheit hinzufügen"
defaultTranslation LblNoMesoPlanEntries = "Keine Einheiten"
defaultTranslation LblCreateMesoPlan = "Meso-Plan erstellen"
-- Lesson Planning
defaultTranslation LblLessonPlan = "Unterrichtsplan"
defaultTranslation LblLessonPlanDate = "Datum"
defaultTranslation LblLessonPlanAssignments = "Aufträge"
defaultTranslation LblLessonPlanResources = "Ressourcen"
defaultTranslation LblLessonPlanPhases = "Unterrichtsphasen"
defaultTranslation LblLessonPlanNotes = "Notizen"
defaultTranslation LblCreateLessonPlan = "Unterrichtsplan erstellen"
defaultTranslation LblNoLessonPlan = "Kein Unterrichtsplan"
defaultTranslation LblNoNotes = "Keine Notizen"
defaultTranslation LblPhaseTitle = "Phase"
defaultTranslation LblPhaseSocialForm = "Sozialform"
defaultTranslation LblPhaseDuration = "Dauer (Min.)"
defaultTranslation LblPhaseActionForm = "Aktionsform"
defaultTranslation LblPhaseNotes = "Notizen"
defaultTranslation LblAddPhase = "Phase hinzufügen"
defaultTranslation LblNoPhases = "Keine Phasen"
defaultTranslation (LblTeachingSocialForm WholeClass) = "Plenum"
defaultTranslation (LblTeachingSocialForm SmallGroups) = "Gruppenarbeit"
defaultTranslation (LblTeachingSocialForm PairWork) = "Partnerarbeit"
defaultTranslation (LblTeachingSocialForm IndividualWork) = "Einzelarbeit"
defaultTranslation (LblActionForm Presenting) = "Darbietend"
defaultTranslation (LblActionForm Collaborating) = "Zusammenwirkend"
defaultTranslation (LblActionForm Assigning) = "Aufgebend"

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
