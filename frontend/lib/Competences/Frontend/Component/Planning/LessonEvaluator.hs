module Competences.Frontend.Component.Planning.LessonEvaluator
  ( lessonEvaluatorComponent
  , pinLessonEvaluator
  )
where

import Competences.Command (Command (..), EntityCommand (..), AbsencesCommand (..), ParticipationRecordsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Absence (..)
  , AbsenceIxs
  , Competence (..)
  , Document (..)
  , EvidenceIxs
  , Lesson (..)
  , LessonId
  , User (..)
  , UserId
  , UserRole (..)
  )
import Competences.Document.Id (idToText)
import Competences.Document.Evidence (Ability, Evidence (..), Observation (..))
import Competences.Document.ParticipationRecord
  ( ParticipationLevel (..)
  , ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  , allParticipationLevels
  , allParticipationTypes
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Planning.StudentEvaluatorModal (openStudentEvaluator)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector
  ( ResultView (..)
  , formatCompetenceLevelBadge'
  )
import Competences.Frontend.SyncContext.WindowManager (PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), pinDialog)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button (ButtonDisabled (..))
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color.Ability qualified as Color
import Competences.Frontend.View.Color.Participation qualified as PColor
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tooltip qualified as Tooltip
import Competences.Frontend.View.Typography qualified as Typography
import Control.Monad (when)
import Data.List (find)
import Data.Time (Day)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M

-- ============================================================================
-- MODEL
-- ============================================================================

data LessonEvalModel = LessonEvalModel
  { students :: ![User]
  , participationRecords :: !(Ix.IxSet ParticipationRecordIxs ParticipationRecord)
  , absences :: !(Ix.IxSet AbsenceIxs Absence)
  , studentBadges :: !(Map.Map UserId [ObservationData])
  }
  deriving (Eq, Generic, Show)

data ObservationData = ObservationData
  { resultView :: !ResultView
  , ability :: !Ability
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- ACTIONS
-- ============================================================================

data LessonEvalAction
  = DocumentUpdated !DocumentChange
  | ToggleParticipation !UserId !ParticipationType !ParticipationLevel
  | ToggleAbsence !UserId
  | OpenStudentDetail !UserId
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

data Col
  = NameCol
  | AbsenceCol
  | ParticipationCol !ParticipationType
  | TasksCol
  deriving (Eq, Ord, Show)

-- | Pin the lesson evaluator as a persistent dialog.
pinLessonEvaluator :: SyncContext -> Maybe Day -> Lesson -> IO ()
pinLessonEvaluator r mDateFrom lesson =
  let pinTitle = C.translate' C.LblLessonEvaluation
        <> ": " <> M.ms lesson.title
        <> maybe "" (\d -> ", " <> C.formatDay d) lesson.date
      meta = PinMeta
        { key = "lesson-evaluation-" <> idToText lesson.id
        , category = PinCatLessonEvaluation
        , sortKey = SortKey [SortAtom mDateFrom, SortAtom lesson.order, SortAtom lesson.date, SortAtom lesson.id]
        , context = fmap C.formatDayShort lesson.date
        , isEditor = False
        , followUp = True
        }
   in pinDialog r.windowManager
        meta
        (WindowChrome pinTitle Icon.IcnMesoPlan Nothing)
        (lessonEvaluatorComponent r lesson.id)

lessonEvaluatorComponent
  :: SyncContext -> LessonId -> M.Component WM.Model LessonEvalModel LessonEvalAction
lessonEvaluatorComponent r lessonId =
  (M.component initialModel update viewOverview)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel =
      LessonEvalModel
        { students = []
        , participationRecords = Ix.empty
        , absences = Ix.empty
        , studentBadges = Map.empty
        }

    -- ------------------------------------------------------------------
    -- UPDATE
    -- ------------------------------------------------------------------

    studentBadges :: Document -> Ix.IxSet EvidenceIxs Evidence -> UserId -> [ObservationData]
    studentBadges d lessonEvidences studentId =
      let observations =
            maybe
              []
              (Ix.toList . (.observations))
              (Ix.getOne $ lessonEvidences Ix.@= studentId)
       in mapMaybe toObservationData observations
      where
        toObservationData o = do
          let (competenceId, level) = o.competenceLevelId
          let ability = o.ability
          competence <- Ix.getOne $ d.competences Ix.@= competenceId
          competenceGrid <- Ix.getOne $ d.competenceGrids Ix.@= competence.competenceGridId
          pure
            ObservationData
              { resultView = formatCompetenceLevelBadge' competenceGrid competence level
              , ability = ability
              }

    update (DocumentUpdated dc) = M.modify $ \m ->
      let doc = dc.document
          students = Ix.toAscList (Proxy @T.Text) $ doc.users Ix.@= Student
          badges =
            Map.fromList $ map (\s -> (s.id, studentBadges doc (doc.evidences Ix.@= lessonId) s.id)) students
       in m
            { students = students
            , participationRecords = doc.participationRecords Ix.@= lessonId
            , absences = doc.absences Ix.@= lessonId
            , studentBadges = badges
            }
    update (ToggleParticipation userId pType pLevel) = do
      m <- M.get
      M.io_ $ do
        let existing = m.participationRecords Ix.@= userId Ix.@= pType
        case Ix.getOne existing of
          Just pr -> do
            delete pr.id
            when (pr.level /= pLevel) create
          Nothing -> create
      where
        delete prId =
          modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Delete prId)
        create = do
          prId <- nextId r
          let newPr =
                ParticipationRecord
                  { id = prId
                  , lessonId = lessonId
                  , userId = userId
                  , participationType = pType
                  , level = pLevel
                  , remark = Nothing
                  }
          modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Create newPr)

    update (ToggleAbsence userId) = do
      m <- M.get
      M.io_ $ do
        let existing = m.absences Ix.@= userId
        case Ix.getOne existing of
          Just a ->
            modifySyncDocument r (Absences $ OnAbsences $ Delete a.id)
          Nothing -> do
            aId <- nextId r
            let newAbsence =
                  Absence
                    { id = aId
                    , lessonId = lessonId
                    , userId = userId
                    }
            modifySyncDocument r (Absences $ OnAbsences $ Create newAbsence)

    -- Open student detail as a modal
    update (OpenStudentDetail userId) = do
      m <- M.get
      let userName = maybe "" (.name) $ find (\u -> u.id == userId) m.students
      M.io_ $ openStudentEvaluator r lessonId (M.ms userName) userId

    -- ------------------------------------------------------------------
    -- VIEW
    -- ------------------------------------------------------------------

    viewOverview :: LessonEvalModel -> M.View LessonEvalModel LessonEvalAction
    viewOverview m =
      Table.viewTable
        Table.Table
          { columns = [NameCol, AbsenceCol] <> map ParticipationCol allParticipationTypes <> [TasksCol]
          , rows = m.students
          , columnSpec = \case
              NameCol -> Table.autoSizedLabelCol C.LblStudent
              AbsenceCol -> Table.autoSizedLabelCol C.LblAbsent
              ParticipationCol pType -> Table.autoSizedLabelCol (C.LblParticipationType pType)
              TasksCol -> Table.autoSizedLabelCol C.LblTasks
          , rowContents = Table.cellContents $ \student -> \case
              NameCol -> M.text $ M.ms student.name
              AbsenceCol -> viewAbsenceCell student.id (m.absences Ix.@= student.id)
              ParticipationCol pType ->
                let isAbsent = not $ Ix.null (m.absences Ix.@= student.id)
                 in viewParticipationCell isAbsent student.id (m.participationRecords Ix.@= student.id) pType
              TasksCol ->
                Layout.hFlow'
                  [ viewEvidenceBadges m student.id
                  , Layout.flowSpring
                  , Button.editButton (OpenStudentDetail student.id)
                  ]
          }

    viewAbsenceCell studentId studentAbsences =
      let isAbsent = not $ Ix.null studentAbsences
       in Button.toggleSm
            isAbsent
            (Button.button Icon.IcnSick (ToggleAbsence studentId))

    viewParticipationCell isAbsent studentId participationRecords pType =
      Layout.hFlow' $
        map
          (viewParticipationButton isAbsent studentId participationRecords pType)
          allParticipationLevels

    viewParticipationButton isAbsent userId prs pType pLevel =
      let mRecord = Ix.getOne (prs Ix.@= pType)
          isActive = case mRecord of
            Just pr -> pr.level == pLevel
            Nothing -> False
          icn = PColor.participationLevelIcon pType pLevel
          tooltipText = C.translate' (C.LblParticipationLevel pType pLevel)
          action = if isAbsent then Button.button icn Disabled else Button.button icn (ToggleParticipation userId pType pLevel)
       in Tooltip.withTooltip (Tooltip.PlainTooltip tooltipText) $
            Button.toggleSm
              isActive
              action

    viewEvidenceBadges m userId =
      case m.studentBadges Map.!? userId of
        Just badges -> Layout.hFlow' $ map viewBadge badges
        Nothing -> Typography.placeholder (C.translate' C.LblNoEvidence)

    viewBadge b =
      Tooltip.withTooltip' (Tooltip.PlainTooltip <$> b.resultView.tooltipContent) $
        Badge.badge (Color.abilityPalette b.ability) (Badge.badgeText b.resultView.badgeText)

