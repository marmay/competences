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
  , User (..)
  )
import Competences.Document.Competence (CompetenceIxs)
import Competences.Document.Evidence
  ( Evidence (..)
  , Observation (..)
  )
import Competences.Document.Lesson (Lesson (..))
import Competences.Document.ParticipationRecord
  ( ParticipationLevel (..)
  , ParticipationRecord (..)
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
import Competences.Frontend.Component.Selector.CompetenceLevelSelector
  ( ResultView (..)
  , formatCompetenceLevelBadge
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
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
import Competences.Frontend.View.Evaluation qualified as Eval
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.Lesson qualified as QLesson
import Data.Maybe (fromMaybe)
import Miso.CSS qualified as MC
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
  | ToggleParticipation !UserId !ParticipationType !ParticipationLevel
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

    -- Participation toggles: create, delete, or switch level
    update (ToggleParticipation userId pType pLevel) = do
      m <- M.get
      M.io_ $ do
        let existing = m.participationRecords Ix.@= userId Ix.@= pType
        case Ix.getOne existing of
          Just pr
            | pr.level == pLevel ->
                -- Same level clicked: toggle off
                modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Delete pr.id)
            | otherwise -> do
                -- Different level clicked: delete old, create new
                modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Delete pr.id)
                prId <- nextId r
                let newPr =
                      ParticipationRecord
                        { id = prId
                        , lessonId = m.lesson.id
                        , userId = userId
                        , participationType = pType
                        , level = pLevel
                        , remark = Nothing
                        }
                modifySyncDocument r (ParticipationRecords $ OnParticipationRecords $ Create newPr)
          Nothing -> do
            -- No record: create with selected level
            prId <- nextId r
            let pr =
                  ParticipationRecord
                    { id = prId
                    , lessonId = m.lesson.id
                    , userId = userId
                    , participationType = pType
                    , level = pLevel
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
        [class_ "space-y-3 overflow-y-auto"]
        [ MH.div_
            [ class_ "grid gap-3"
            , MC.style_ [("grid-template-columns", "repeat(auto-fill, minmax(300px, 1fr))")]
            ]
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
                , Button.ghostSm (Button.button Icon.IcnEdit (OpenStudentDetail user.id))
                ]
            , -- Participation toggles (two buttons per category)
              viewParticipationControls user.id prs
            , -- Evidence summary badges
              case mEvidence of
                Nothing -> MH.div_ [class_ "text-xs text-muted-foreground"] [M.text $ C.translate' C.LblNoEvidence]
                Just ev -> viewEvidenceBadges m ev
            ]

    viewParticipationControls userId prs =
      MH.div_
        [class_ "grid grid-cols-[auto_auto] gap-x-2 gap-y-1 items-center mb-2"]
        (concatMap (viewParticipationRow userId prs) [minBound .. maxBound])

    viewParticipationRow userId prs pType =
      [ MH.span_ [class_ "text-xs text-muted-foreground"] [M.text $ C.translate' (C.LblParticipationType pType)]
      , MH.div_
          [class_ "flex"]
          [ viewParticipationButton userId prs pType ParticipationLevel1
          , viewParticipationButton userId prs pType ParticipationLevel2
          ]
      ]

    viewParticipationButton userId prs pType pLevel =
      let mRecord = Ix.getOne (prs Ix.@= pType)
          isActive = case mRecord of
            Just pr -> pr.level == pLevel
            Nothing -> False
          icn = participationIcon pType pLevel
          tooltipText = C.translate' (C.LblParticipationLevel pType pLevel)
       in withTooltip (PlainTooltip tooltipText) $
            Button.toggleSm isActive
              (Button.button icn (ToggleParticipation userId pType pLevel))

    -- | Icon for participation button based on type and level
    participationIcon :: ParticipationType -> ParticipationLevel -> Icon.Icon
    participationIcon pType pLevel = case (pType, pLevel) of
      (PoorWorkEthic, ParticipationLevel1) -> Icon.IcnMinus
      (PoorWorkEthic, ParticipationLevel2) -> Icon.IcnMinusMinus
      (_, ParticipationLevel1) -> Icon.IcnPlus
      (_, ParticipationLevel2) -> Icon.IcnPlusPlus

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
          ResultView {badgeText, tooltipContent} =
            formatCompetenceLevelBadge m.competences m.competenceGrids
              (competenceId, level)
       in withTooltip (maybe NoTooltip PlainTooltip tooltipContent) $
            Badge.badge (Eval.abilityPalette obs.ability) badgeText
