module Competences.Frontend.Component.Planning.LessonEvaluator
  ( lessonEvaluatorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ParticipationRecordsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , Document (..)
  , EvidenceIxs
  , LessonId
  , User (..)
  , UserId
  , UserRole (..)
  )
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
import Competences.Frontend.Component.Planning.StudentEvaluatorModal (studentEvaluatorModal)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector
  ( ResultView (..)
  , formatCompetenceLevelBadge'
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), openFramedModal)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , closeModal
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color.Ability qualified as Color
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tooltip qualified as Tooltip
import Competences.Frontend.View.Typography qualified as Typography
import Control.Monad (when)
import Data.List (find)
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
  | OpenStudentDetail !UserId
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

data Col
  = NameCol
  | ParticipationCol !ParticipationType
  | TasksCol
  deriving (Eq, Ord, Show)

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

    -- Open student detail as a modal
    update (OpenStudentDetail userId) = do
      m <- M.get
      let userName = maybe "" (.name) $ find (\u -> u.id == userId) m.students
          cfg = ModalConfig (WindowChrome (M.ms userName) Icon.IcnEvidence) ModalWide ModalFull Nothing
      M.io_ $
        openFramedModal r.windowManager cfg (studentEvaluatorModal r (Just $ closeModal r.windowManager) lessonId userId)

    -- ------------------------------------------------------------------
    -- VIEW
    -- ------------------------------------------------------------------

    viewOverview :: LessonEvalModel -> M.View LessonEvalModel LessonEvalAction
    viewOverview m =
      Table.viewTable
        Table.Table
          { columns = [NameCol] <> map ParticipationCol allParticipationTypes <> [TasksCol]
          , rows = m.students
          , columnSpec = \case
              NameCol -> Table.autoSizedLabelCol C.LblStudent
              ParticipationCol pType -> Table.autoSizedLabelCol (C.LblParticipationType pType)
              TasksCol -> Table.autoSizedLabelCol C.LblTasks
          , rowContents = Table.cellContents $ \student -> \case
              NameCol -> M.text $ M.ms student.name
              ParticipationCol pType -> viewParticipationCell student.id (m.participationRecords Ix.@= student.id) pType
              TasksCol ->
                Layout.hFlow'
                  [ viewEvidenceBadges m student.id
                  , Layout.flowSpring
                  , Button.editButton (OpenStudentDetail student.id)
                  ]
          }

    viewParticipationCell studentId participationRecords pType =
      Layout.hFlow' $
        map
          (viewParticipationButton studentId participationRecords pType)
          allParticipationLevels

    viewParticipationButton userId prs pType pLevel =
      let mRecord = Ix.getOne (prs Ix.@= pType)
          isActive = case mRecord of
            Just pr -> pr.level == pLevel
            Nothing -> False
          icn = participationIcon pType pLevel
          tooltipText = C.translate' (C.LblParticipationLevel pType pLevel)
       in Tooltip.withTooltip (Tooltip.PlainTooltip tooltipText) $
            Button.toggleSm
              isActive
              (Button.button icn (ToggleParticipation userId pType pLevel))

    viewEvidenceBadges m userId =
      case m.studentBadges Map.!? userId of
        Just badges -> Layout.hFlow' $ map viewBadge badges
        Nothing -> Typography.placeholder (C.translate' C.LblNoEvidence)

    viewBadge b =
      Tooltip.withTooltip' (Tooltip.PlainTooltip <$> b.resultView.tooltipContent) $
        Badge.badge (Color.abilityPalette b.ability) (Badge.badgeText b.resultView.badgeText)

    -- \| Icon for participation button based on type and level
    participationIcon :: ParticipationType -> ParticipationLevel -> Icon.Icon
    participationIcon pType pLevel = case (pType, pLevel) of
      (PoorWorkEthic, ParticipationLevel1) -> Icon.IcnMinus
      (PoorWorkEthic, ParticipationLevel2) -> Icon.IcnMinusMinus
      (_, ParticipationLevel1) -> Icon.IcnPlus
      (_, ParticipationLevel2) -> Icon.IcnPlusPlus
