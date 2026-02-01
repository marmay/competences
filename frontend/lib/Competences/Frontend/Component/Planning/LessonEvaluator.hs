module Competences.Frontend.Component.Planning.LessonEvaluator
  ( lessonEvaluatorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ParticipationRecordsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridIxs
  , Document (..)
  , LevelInfo (..)
  , User (..)
  )
import Competences.Document.Competence (CompetenceIxs)
import Competences.Document.Evidence
  ( Ability (..)
  , Evidence (..)
  , Observation (..)
  )
import Competences.Document.Lesson (Lesson (..))
import Competences.Document.ParticipationRecord
  ( ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  )
import Competences.Document.Task
  ( Task
  , TaskGroup
  , TaskGroupIxs
  , TaskId
  , TaskIxs
  )
import Competences.Document.User (UserId, UserIxs)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Planning.StudentEvaluatorModal (studentEvaluatorModal)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (openModal)
import Competences.Frontend.SyncContext.WindowManager qualified as WM
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Lesson qualified as QLesson
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- ============================================================================
-- MODEL
-- ============================================================================

data LessonEvalModel = LessonEvalModel
  { lesson :: !Lesson
  , users :: !(Ix.IxSet UserIxs User)
  , participationRecords :: !(Ix.IxSet ParticipationRecordIxs ParticipationRecord)
  , lessonEvidences :: ![Evidence]
  , lessonTaskIds :: !(Set.Set TaskId)
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- ACTIONS
-- ============================================================================

data LessonEvalAction
  = DocumentUpdated !DocumentChange
  | ToggleParticipation !UserId !ParticipationType
  | OpenStudentDetail !UserId
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

lessonEvaluatorComponent :: SyncContext -> Lesson -> M.Component WM.Model LessonEvalModel LessonEvalAction
lessonEvaluatorComponent r initialLesson =
  (M.component initialModel update view')
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel =
      LessonEvalModel
        { lesson = initialLesson
        , users = Ix.empty
        , participationRecords = Ix.empty
        , lessonEvidences = []
        , lessonTaskIds = Set.empty
        , tasks = Ix.empty
        , taskGroups = Ix.empty
        , competences = Ix.empty
        , competenceGrids = Ix.empty
        }

    -- ------------------------------------------------------------------
    -- UPDATE
    -- ------------------------------------------------------------------

    update (DocumentUpdated dc) = M.modify $ \m ->
      let doc = dc.document
          lesson' = fromMaybe m.lesson $ Ix.getOne (doc.lessons Ix.@= m.lesson.id)
          evs = QLesson.lessonEvidences doc lesson'.id
          tids = QLesson.lessonTaskIds doc lesson'.id
       in m
            { lesson = lesson'
            , users = doc.users
            , participationRecords = doc.participationRecords Ix.@= lesson'.id
            , lessonEvidences = evs
            , lessonTaskIds = tids
            , tasks = doc.tasks
            , taskGroups = doc.taskGroups
            , competences = doc.competences
            , competenceGrids = doc.competenceGrids
            }

    -- Participation toggles: create or delete immediately
    update (ToggleParticipation userId pType) = do
      m <- M.get
      M.io_ $ do
        let existing = m.participationRecords Ix.@= userId Ix.@= pType
        case Ix.getOne existing of
          Just pr ->
            modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Delete pr.id)
          Nothing -> do
            prId <- nextId r
            let pr =
                  ParticipationRecord
                    { id = prId
                    , lessonId = m.lesson.id
                    , userId = userId
                    , participationType = pType
                    , remark = Nothing
                    }
            modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Create pr)

    -- Open student detail as a modal
    update (OpenStudentDetail userId) = do
      m <- M.get
      M.io_ $ do
        let mEvidence = findStudentEvidence m userId
            userName = case Ix.getOne (m.users Ix.@= userId) of
              Just u -> u.name
              Nothing -> T.pack (show userId)
        openModal r.windowManager (studentEvaluatorModal r r.windowManager m.lesson userId userName mEvidence)

    -- Helper: find existing evidence for a student in this lesson
    findStudentEvidence :: LessonEvalModel -> UserId -> Maybe Evidence
    findStudentEvidence m userId =
      let evs = filter (\e -> e.userId == Just userId) m.lessonEvidences
       in case evs of
            (e : _) -> Just e
            [] -> Nothing

    -- ------------------------------------------------------------------
    -- VIEW
    -- ------------------------------------------------------------------

    view' = viewOverview

    viewOverview m =
      MH.div_
        [class_ "p-4 space-y-3 overflow-y-auto"]
        [ Typography.h3 (ms m.lesson.title)
        , case m.lesson.date of
            Just d -> MH.div_ [class_ "text-sm text-muted-foreground mb-2"] [M.text $ C.formatDay d]
            Nothing -> M.text ""
        , MH.div_
            [class_ "space-y-2"]
            (map (viewStudentCard m) sortedStudents)
        ]
      where
        sortedStudents = Ix.toAscList (Proxy @T.Text) m.users

    viewStudentCard m user =
      let prs = m.participationRecords Ix.@= user.id
          mEvidence = findStudentEvidence m user.id
       in MH.div_
            [class_ "border border-border rounded-lg p-3 bg-card"]
            [ -- Student name + edit button
              MH.div_
                [class_ "flex items-center justify-between mb-2"]
                [ MH.span_ [class_ "font-medium text-sm"] [M.text $ ms user.name]
                , Button.buttonGhost (C.translate' C.LblEdit)
                    & Button.withIcon IcnEdit
                    & Button.withSize Button.Small
                    & Button.withClick (OpenStudentDetail user.id)
                    & Button.renderButton
                ]
            , -- Participation toggles
              MH.div_
                [class_ "flex gap-1 mb-2"]
                (map (viewParticipationToggle user.id prs) [minBound .. maxBound])
            , -- Evidence summary badges
              case mEvidence of
                Nothing -> MH.div_ [class_ "text-xs text-muted-foreground"] [M.text $ C.translate' C.LblNoEvidence]
                Just ev -> viewEvidenceBadges m ev
            ]

    viewParticipationToggle userId prs pType =
      let isActive = not $ Ix.null (prs Ix.@= pType)
          btnClass =
            if isActive
              then "px-2 py-0.5 rounded text-xs cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90"
              else "px-2 py-0.5 rounded text-xs cursor-pointer bg-secondary text-secondary-foreground hover:bg-secondary/80"
       in MH.button_
            [class_ btnClass, MH.onClick (ToggleParticipation userId pType)]
            [M.text $ C.translate' (C.LblParticipationType pType)]

    viewEvidenceBadges m ev =
      let observations = Ix.toList ev.observations
       in if null observations
            then MH.div_ [class_ "text-xs text-muted-foreground"] [M.text $ C.translate' C.LblNoObservations]
            else
              MH.div_
                [class_ "flex flex-wrap gap-1"]
                (map (viewObservationBadge m) observations)

    viewObservationBadge m obs =
      let (competenceId, level) = obs.competenceLevelId
          competenceM = Ix.getOne (m.competences Ix.@= competenceId)
          label = case competenceM of
            Nothing -> "?"
            Just comp -> ms $ maybe "?" (.description) (comp.levels Map.!? level)
          colorClass = abilityColorClass obs.ability
       in MH.span_
            [class_ $ "px-1.5 py-0.5 rounded text-xs font-medium " <> colorClass]
            [M.text label]

    abilityColorClass :: Ability -> T.Text
    abilityColorClass SelfReliant = "bg-green-100 text-green-800"
    abilityColorClass SelfReliantWithSillyMistakes = "bg-lime-100 text-lime-800"
    abilityColorClass WithSupport = "bg-yellow-100 text-yellow-800"
    abilityColorClass NotYet = "bg-red-100 text-red-800"
