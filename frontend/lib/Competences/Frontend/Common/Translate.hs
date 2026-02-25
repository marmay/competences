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
import Competences.Document.Evidence (Ability (..), ActivityType (..), SocialForm (..), TaskRemark (..), abilities, socialForms, taskRemarks)
import Competences.Document.Lesson (ActionForm (..), TeachingSocialForm (..))
import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationType (..))
import Competences.Document.Solution (SolutionType (..), solutionTypes)
import Competences.Document.Task (TaskPurpose (..), taskPurposes)
import Competences.Query.TaskStatus (TaskStatusGroup (..), taskStatusGroups)
import Data.Maybe (fromMaybe)
import Miso.String (MisoString, ms)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import System.IO.Unsafe (unsafePerformIO)

data Language
  = De
  | En
  deriving (Eq, Ord, Show)

newtype TranslationData = TranslationData
  { unTranslationData :: M.Map Label MisoString
  }
  deriving (Eq, Show)

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
  | LblFilterAllAssignments
  | LblFilterNotGraded
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
  | LblTaskCompletionAsOf
  | LblNoStudentsSelected
  | LblNoStudentSelected
  | LblNoTasksSelected
  | LblNoAssignmentSelected
  | LblSelectAssignment
  | LblSelectAssignments
  | LblSelectResources
  | LblSelectLessonNotes
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
  | LblTaskStatusGroup !TaskStatusGroup
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
  | LblOtherResources
  | LblRelevant
  | LblManageResources
  | LblFilterResources
  | LblUrl
  | LblDescription
    -- Import
  | LblCreate
  | LblImportCompetenceGrids
  | LblImportTasks
  | LblImportAssignments
  | LblImportResources
  | LblImportLessons
  | LblExport
    -- Analytics
  | LblMasteryStreakTwoAssessed
  | LblMasteryStreakTwoPlus
  | LblMasteryOneSuccess
  | LblMasteryOnlySillyMistakes
  | LblMasteryNotYet
  | LblMasteryNotTried
    -- Mastery badges (short labels for grid cell badges)
  | LblMasteryBadgeAuto
  | LblMasteryBadgeChecked
  | LblMasteryBadgeStreak
  | LblMasteryBadgeFirstSuccess
  | LblMasteryBadgeSillyMistakes
  | LblMasteryBadgeNotYet
    -- Meso Planning
  | LblMesoPlanning
  | LblMesoPlanTitle
  | LblMesoPlanDateFrom
  | LblMesoPlanDateTo
  | LblCreateMesoPlan
  | LblEditMesoPlan
  | LblFilterMesoPlans
  | LblMesoPlans
  | LblSelectMesoPlan
    -- Lessons (unified entity)
  | LblLesson
  | LblLessonTitle
  | LblLessonDescription
  | LblLessonCompetences
  | LblLessonDate
  | LblLessonAssignments
  | LblLessonResources
  | LblLessonPhases
  | LblTeachingNotes
  | LblAddLesson
  | LblNoLessons
  | LblNoLesson
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
    -- Assignment evaluator
  | LblEvaluateAssignment
  | LblAssignmentNoTasks
  | LblNSelected !Int
  | LblReset
  | LblLoadEvidence
  | LblTaskNotFound
  | LblTaskPrefix
  | LblIncludeTask
  | LblExcludeTask
  | LblTaskStatement
  | LblPleaseSelectStudents
  | LblAggregatedResults
  | LblAggregationStale
  | LblComputeAggregation
  | LblComputeAggregationHint
  | LblContributingTasks
  | LblSaveEvidences
  | LblCreateEvidencesAction
  | LblStudentsSelected !Int
  | LblEvidencesWillBeEdited
  | LblEvidencesBasedOn
  | LblWillBeEdited
    -- Lesson evaluator
  | LblAbsent
  | LblParticipationType !ParticipationType
  | LblParticipationLevel !ParticipationType !ParticipationLevel
  | LblNoEvidence
  | LblNoObservations
  | LblLessonNoTasks
  | LblManualObservations
  | LblNoManualObservations
  | LblLessonEvaluation
  | LblDeleteEvidence
  | LblAddTask
  | LblSelectTask
  | LblAdd
  | LblBack
    -- Print
  | LblPrint
  | LblPrintPreview
  | LblFormat
  | LblContents
  | LblDescriptionToggle
  | LblAnswerGrid
  | LblPresetAufgabenblatt
  | LblPresetArbeitsblatt
  | LblPresetLoesungsblatt
  | LblPresetMusteraufgaben
  | LblPageSize
  | LblOrientation
  | LblPortrait
  | LblLandscape
  | LblFontSize
  | LblLayout
  | LblContinuous
  | LblGrid
  | LblRows
  | LblColumns
  | LblGroupedCopies
  | LblTotalCopies
  | LblShowTitle
  | LblShowHeader
  | LblShowFooter
  | LblShowNameField
  | LblStudentName
  | LblTaskHeaderStyle
  | LblHeaderNumber
  | LblHeaderTitle
  | LblHeaderBoth
  | LblTaskWord
    -- Lesson Notes
  | LblLessonNotesEntries
  | LblFilterLessonNotes
  | LblNewLessonNotes
  | LblLessonNotesDate
  | LblLessonNotesTitle
  | LblLessonNotesResources
  | LblLessonNotesItems
  | LblSelectTasks
    -- Task remarks
  | LblTaskRemarks
  | LblTaskRemark !TaskRemark
    -- Assignment references
  | LblUsedInAssignment
  | LblUsedInAssignments
    -- Impersonation
  | LblViewAsStudent
  | LblReturnToTeacher
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
  , LblFilterAllAssignments
  , LblFilterNotGraded
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
  , LblTaskCompletionAsOf
  , LblNoStudentsSelected
  , LblNoStudentSelected
  , LblNoTasksSelected
  , LblNoAssignmentSelected
  , LblSelectAssignment
  , LblSelectAssignments
  , LblSelectResources
  , LblSelectLessonNotes
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
  , LblOtherResources
  , LblRelevant
  , LblManageResources
  , LblFilterResources
  , LblUrl
  , LblDescription
    -- Import
  , LblCreate
  , LblImportCompetenceGrids
  , LblImportTasks
  , LblImportAssignments
  , LblImportResources
  , LblImportLessons
  , LblExport
    -- Analytics
  , LblMasteryStreakTwoAssessed
  , LblMasteryStreakTwoPlus
  , LblMasteryOneSuccess
  , LblMasteryOnlySillyMistakes
  , LblMasteryNotYet
  , LblMasteryNotTried
    -- Mastery badges
  , LblMasteryBadgeAuto
  , LblMasteryBadgeChecked
  , LblMasteryBadgeStreak
  , LblMasteryBadgeFirstSuccess
  , LblMasteryBadgeSillyMistakes
  , LblMasteryBadgeNotYet
    -- Meso Planning
  , LblMesoPlanning
  , LblMesoPlanTitle
  , LblMesoPlanDateFrom
  , LblMesoPlanDateTo
  , LblCreateMesoPlan
  , LblEditMesoPlan
  , LblFilterMesoPlans
  , LblMesoPlans
  , LblSelectMesoPlan
    -- Lessons
  , LblLesson
  , LblLessonTitle
  , LblLessonDescription
  , LblLessonCompetences
  , LblLessonDate
  , LblLessonAssignments
  , LblLessonResources
  , LblLessonPhases
  , LblTeachingNotes
  , LblAddLesson
  , LblNoLessons
  , LblNoLesson
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
    -- Assignment evaluator
  , LblEvaluateAssignment
  , LblAssignmentNoTasks
  , LblNSelected 0
  , LblReset
  , LblLoadEvidence
  , LblTaskNotFound
  , LblTaskPrefix
  , LblIncludeTask
  , LblExcludeTask
  , LblTaskStatement
  , LblPleaseSelectStudents
  , LblAggregatedResults
  , LblAggregationStale
  , LblComputeAggregation
  , LblComputeAggregationHint
  , LblContributingTasks
  , LblSaveEvidences
  , LblCreateEvidencesAction
  , LblStudentsSelected 0
  , LblEvidencesWillBeEdited
  , LblEvidencesBasedOn
  , LblWillBeEdited
    -- Lesson evaluator
  , LblAbsent
  , LblParticipationType Participation
  , LblParticipationType Collaboration
  , LblParticipationType PoorWorkEthic
  , LblParticipationLevel Participation ParticipationLevel1
  , LblParticipationLevel Participation ParticipationLevel2
  , LblParticipationLevel Collaboration ParticipationLevel1
  , LblParticipationLevel Collaboration ParticipationLevel2
  , LblParticipationLevel PoorWorkEthic ParticipationLevel1
  , LblParticipationLevel PoorWorkEthic ParticipationLevel2
  , LblNoEvidence
  , LblNoObservations
  , LblLessonNoTasks
  , LblManualObservations
  , LblNoManualObservations
  , LblLessonEvaluation
  , LblDeleteEvidence
  , LblAddTask
  , LblSelectTask
  , LblAdd
  , LblBack
    -- Print
  , LblPrint
  , LblPrintPreview
  , LblFormat
  , LblContents
  , LblDescriptionToggle
  , LblAnswerGrid
  , LblPresetAufgabenblatt
  , LblPresetArbeitsblatt
  , LblPresetLoesungsblatt
  , LblPresetMusteraufgaben
  , LblPageSize
  , LblOrientation
  , LblPortrait
  , LblLandscape
  , LblFontSize
  , LblLayout
  , LblContinuous
  , LblGrid
  , LblRows
  , LblColumns
  , LblGroupedCopies
  , LblTotalCopies
  , LblShowTitle
  , LblShowHeader
  , LblShowFooter
  , LblShowNameField
  , LblStudentName
  , LblTaskHeaderStyle
  , LblHeaderNumber
  , LblHeaderTitle
  , LblHeaderBoth
  , LblTaskWord
    -- Lesson Notes
  , LblLessonNotesEntries
  , LblFilterLessonNotes
  , LblNewLessonNotes
  , LblLessonNotesDate
  , LblLessonNotesTitle
  , LblLessonNotesResources
  , LblLessonNotesItems
  , LblSelectTasks
    -- Task remarks
  , LblTaskRemarks
    -- Assignment references
  , LblUsedInAssignment
  , LblUsedInAssignments
  , LblViewAsStudent
  , LblReturnToTeacher
  ]
    <> map LblSocialForm socialForms
    <> map LblAbility abilities
    <> map LblTaskPurpose taskPurposes
    <> map LblTaskStatusGroup taskStatusGroups
    <> map LblSolutionType solutionTypes
    <> map LblTaskRemark taskRemarks

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
defaultTranslation LblPleaseSelectItem = "Bitte wähle ein Element!"
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
defaultTranslation LblFilterAllAssignments = "Alle"
defaultTranslation LblFilterNotGraded = "Nicht korrigiert"
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
defaultTranslation LblTaskCompletionAsOf = "Stand:"
defaultTranslation LblNoStudentsSelected = "Keine Schüler ausgewählt"
defaultTranslation LblNoStudentSelected = "Kein Schüler ausgewählt"
defaultTranslation LblNoTasksSelected = "Keine Aufgaben ausgewählt"
defaultTranslation LblNoAssignmentSelected = "Kein Auftrag ausgewählt"
defaultTranslation LblSelectAssignment = "Auftrag auswählen..."
defaultTranslation LblSelectAssignments = "Aufträge auswählen..."
defaultTranslation LblSelectResources = "Ressourcen auswählen..."
defaultTranslation LblSelectLessonNotes = "Unterrichtsnotizen auswählen..."
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
defaultTranslation (LblTaskStatusGroup GroupOpen) = "Offen"
defaultTranslation (LblTaskStatusGroup GroupInProgress) = "In Bearbeitung"
defaultTranslation (LblTaskStatusGroup GroupDone) = "Erledigt"
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
defaultTranslation LblOtherResources = "Weitere Materialien"
defaultTranslation LblRelevant = "Passend"
defaultTranslation LblManageResources = "Ressourcen"
defaultTranslation LblFilterResources = "Ressourcen filtern..."
defaultTranslation LblUrl = "URL"
defaultTranslation LblDescription = "Beschreibung"
-- Import
defaultTranslation LblCreate = "Erstellen"
defaultTranslation LblImportCompetenceGrids = "Kompetenzraster importieren"
defaultTranslation LblImportTasks = "Aufgaben importieren"
defaultTranslation LblImportAssignments = "Aufgaben importieren"
defaultTranslation LblImportResources = "Materialien importieren"
defaultTranslation LblImportLessons = "Stunden importieren"
defaultTranslation LblExport = "Exportieren"
-- Analytics
defaultTranslation LblMasteryStreakTwoAssessed = "Überprüfungsbereit (++2)"
defaultTranslation LblMasteryStreakTwoPlus = "Sicher (2+ Erfolge)"
defaultTranslation LblMasteryOneSuccess = "Erfolgreich (1 Erfolg)"
defaultTranslation LblMasteryOnlySillyMistakes = "Dumme Fehler"
defaultTranslation LblMasteryNotYet = "Noch nicht"
defaultTranslation LblMasteryNotTried = "Nicht versucht"
-- Mastery badges
defaultTranslation LblMasteryBadgeAuto = "Auto"
defaultTranslation LblMasteryBadgeChecked = "Überprüft"
defaultTranslation LblMasteryBadgeStreak = "Streak"
defaultTranslation LblMasteryBadgeFirstSuccess = "Erste Erfolge"
defaultTranslation LblMasteryBadgeSillyMistakes = "Dumme Fehler"
defaultTranslation LblMasteryBadgeNotYet = "Noch nicht"
-- Meso Planning
defaultTranslation LblMesoPlanning = "Planung"
defaultTranslation LblMesoPlanTitle = "Titel"
defaultTranslation LblMesoPlanDateFrom = "Beginn"
defaultTranslation LblMesoPlanDateTo = "Ende"
defaultTranslation LblCreateMesoPlan = "Meso-Plan erstellen"
defaultTranslation LblEditMesoPlan = "Plan bearbeiten"
defaultTranslation LblFilterMesoPlans = "Nach Titel filtern..."
defaultTranslation LblMesoPlans = "Meso-Pläne"
defaultTranslation LblSelectMesoPlan = "Plan auswählen..."
-- Lessons
defaultTranslation LblLesson = "Unterrichtseinheit"
defaultTranslation LblLessonTitle = "Titel"
defaultTranslation LblLessonDescription = "Beschreibung"
defaultTranslation LblLessonCompetences = "Kompetenzstufen"
defaultTranslation LblLessonDate = "Datum"
defaultTranslation LblLessonAssignments = "Aufträge"
defaultTranslation LblLessonResources = "Ressourcen"
defaultTranslation LblLessonPhases = "Unterrichtsphasen"
defaultTranslation LblTeachingNotes = "Lehrnotizen"
defaultTranslation LblAddLesson = "Einheit hinzufügen"
defaultTranslation LblNoLessons = "Keine Einheiten"
defaultTranslation LblNoLesson = "Keine Einheit"
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
-- Assignment evaluator
defaultTranslation LblEvaluateAssignment = "Auftrag auswerten"
defaultTranslation LblAssignmentNoTasks = "Dieser Auftrag hat keine Aufgaben"
defaultTranslation (LblNSelected n) = ms (show n) <> " ausgewählt"
defaultTranslation LblReset = "Zurücksetzen"
defaultTranslation LblLoadEvidence = "Nachweis laden"
defaultTranslation LblTaskNotFound = "Aufgabe nicht gefunden"
defaultTranslation LblTaskPrefix = "Aufgabe: "
defaultTranslation LblIncludeTask = "Einbeziehen"
defaultTranslation LblExcludeTask = "Ausschließen"
defaultTranslation LblTaskStatement = "Aufgabenstellung"
defaultTranslation LblPleaseSelectStudents = "Bitte wählen Sie Schüler zur Auswertung aus"
defaultTranslation LblAggregatedResults = "Aggregierte Ergebnisse"
defaultTranslation LblAggregationStale = "Bewertungen haben sich geändert \x2014 bitte neu berechnen"
defaultTranslation LblComputeAggregation = "Aggregation berechnen"
defaultTranslation LblComputeAggregationHint = "Klicken Sie auf 'Aggregation berechnen', um die Ergebnisse zu aggregieren."
defaultTranslation LblContributingTasks = "Aufgaben: "
defaultTranslation LblSaveEvidences = "Nachweise speichern"
defaultTranslation LblCreateEvidencesAction = "Nachweise erstellen"
defaultTranslation (LblStudentsSelected n) = ms (show n) <> " Schüler ausgewählt"
defaultTranslation LblEvidencesWillBeEdited = "Die Nachweise der folgenden Schüler werden bearbeitet:"
defaultTranslation LblEvidencesBasedOn = "Die Nachweise der folgenden Schüler werden auf Basis des Nachweises für \""
defaultTranslation LblWillBeEdited = "\" bearbeitet: "
-- Lesson evaluator
defaultTranslation LblAbsent = "Abwesend"
defaultTranslation (LblParticipationType Participation) = "Mitarbeit"
defaultTranslation (LblParticipationType Collaboration) = "Kollaboration"
defaultTranslation (LblParticipationType PoorWorkEthic) = "Arbeitshaltung"
defaultTranslation (LblParticipationLevel Participation ParticipationLevel1) = "Gut"
defaultTranslation (LblParticipationLevel Participation ParticipationLevel2) = "Herausragend"
defaultTranslation (LblParticipationLevel Collaboration ParticipationLevel1) = "Gut"
defaultTranslation (LblParticipationLevel Collaboration ParticipationLevel2) = "Herausragend"
defaultTranslation (LblParticipationLevel PoorWorkEthic ParticipationLevel1) = "Unbemüht"
defaultTranslation (LblParticipationLevel PoorWorkEthic ParticipationLevel2) = "Verweigernd"
defaultTranslation LblNoEvidence = "Keine Nachweise"
defaultTranslation LblNoObservations = "Keine Beobachtungen"
defaultTranslation LblLessonNoTasks = "Keine Aufgaben in dieser Einheit"
defaultTranslation LblManualObservations = "Manuelle Beobachtungen"
defaultTranslation LblNoManualObservations = "Keine manuellen Beobachtungen"
defaultTranslation LblLessonEvaluation = "Unterrichtsbeurteilung"
defaultTranslation LblDeleteEvidence = "Nachweis löschen"
defaultTranslation LblAddTask = "Aufgabe hinzufügen"
defaultTranslation LblSelectTask = "Aufgabe auswählen..."
defaultTranslation LblAdd = "Hinzufügen"
defaultTranslation LblBack = "Zurück"
-- Print
defaultTranslation LblPrint = "Drucken"
defaultTranslation LblPrintPreview = "Druckvorschau"
defaultTranslation LblFormat = "Format"
defaultTranslation LblContents = "Inhalt"
defaultTranslation LblDescriptionToggle = "Angabe"
defaultTranslation LblAnswerGrid = "Kästchen"
defaultTranslation LblPresetAufgabenblatt = "Aufgabenblatt"
defaultTranslation LblPresetArbeitsblatt = "Arbeitsblatt"
defaultTranslation LblPresetLoesungsblatt = "Lösungsblatt"
defaultTranslation LblPresetMusteraufgaben = "Musteraufgaben"
defaultTranslation LblPageSize = "Seitengröße"
defaultTranslation LblOrientation = "Ausrichtung"
defaultTranslation LblPortrait = "Hochformat"
defaultTranslation LblLandscape = "Querformat"
defaultTranslation LblFontSize = "Schriftgröße"
defaultTranslation LblLayout = "Layout"
defaultTranslation LblContinuous = "Fortlaufend"
defaultTranslation LblGrid = "Raster"
defaultTranslation LblRows = "Zeilen"
defaultTranslation LblColumns = "Spalten"
defaultTranslation LblGroupedCopies = "Kopien pro Aufgabe"
defaultTranslation LblTotalCopies = "Gesamtkopien"
defaultTranslation LblShowTitle = "Titel"
defaultTranslation LblShowHeader = "Kopfzeile"
defaultTranslation LblShowFooter = "Seitenzahlen"
defaultTranslation LblShowNameField = "Namensfeld"
defaultTranslation LblStudentName = "Name"
defaultTranslation LblTaskHeaderStyle = "Aufgabenüberschrift"
defaultTranslation LblHeaderNumber = "Nummer"
defaultTranslation LblHeaderTitle = "Titel"
defaultTranslation LblHeaderBoth = "Beides"
defaultTranslation LblTaskWord = "Aufgabe "
-- Lesson Notes
defaultTranslation LblLessonNotesEntries = "Unterrichtsnotizen"
defaultTranslation LblFilterLessonNotes = "Unterrichtsnotizen filtern..."
defaultTranslation LblNewLessonNotes = "Neue Unterrichtsnotiz"
defaultTranslation LblLessonNotesDate = "Datum"
defaultTranslation LblLessonNotesTitle = "Titel"
defaultTranslation LblLessonNotesResources = "Ressourcen"
defaultTranslation LblLessonNotesItems = "Materialien"
defaultTranslation LblSelectTasks = "Aufgaben auswählen..."
-- Task remarks
defaultTranslation LblTaskRemarks = "Anmerkungen"
defaultTranslation (LblTaskRemark Exceptional) = "Herausragend"
defaultTranslation (LblTaskRemark Sloppy) = "Schlampig"
defaultTranslation (LblTaskRemark Lacking) = "Lückenhaft"
-- Assignment references
defaultTranslation LblUsedInAssignment = "Verwendet in folgendem Auftrag:"
defaultTranslation LblUsedInAssignments = "Verwendet in folgenden Aufträgen:"
-- Impersonation
defaultTranslation LblViewAsStudent = "Als Schüler anzeigen"
defaultTranslation LblReturnToTeacher = "Zurück zur Lehreransicht"

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

trim :: TranslationData -> TranslationData
trim = TranslationData . M.filterWithKey (\k _ -> k `S.member` labels) . (.unTranslationData)

extend :: TranslationData -> TranslationData
extend a = merge a defaultTranslationData

merge :: TranslationData -> TranslationData -> TranslationData
merge a b = TranslationData $ M.union a.unTranslationData b.unTranslationData

loadTranslations :: FilePath -> IO TranslationData
loadTranslations _ = pure defaultTranslationData

saveTranslations :: FilePath -> TranslationData -> IO ()
saveTranslations _ _ = pure ()

labels :: S.Set Label
labels = S.fromList labels'

defaultTranslationData :: TranslationData
defaultTranslationData =
  TranslationData $
    M.fromList $
      map (\l -> (l, ms (defaultTranslation l))) labels'
