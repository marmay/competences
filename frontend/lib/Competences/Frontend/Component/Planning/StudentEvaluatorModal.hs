-- |
-- Module      : Competences.Frontend.Component.Planning.StudentEvaluatorModal
-- Description : Modal dialog for evaluating a single student in a lesson
--
-- Opened from the LessonEvaluator overview. Provides task evaluation,
-- manual observations, aggregation, and evidence creation/update for
-- one student.
module Competences.Frontend.Component.Planning.StudentEvaluatorModal
  ( studentEvaluatorModal
  , openStudentEvaluator
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), ModifyCommand (..))
import Competences.Command.Evidences (EvidencePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , LessonId
  , User (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  , TaskEvaluations
  )
import Competences.Document.Lesson (Lesson (..))
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Deferred (Initializing, _Ready, deferredComponent)
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelSelectorComponent)
import Competences.Frontend.Component.Selector.MultiStageSelector (MultiStageSelectorStyle (..))
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Document.Id (idToText)
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), WindowMode, closeWindow, inlineComponent, openFramedModalWith)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Combobox
  ( ComboboxOption (..)
  , renderCombobox
  , singleSelectCombobox
  , withIsOpen
  , withOptions
  , withPlaceholder
  , withSearchQuery
  , withSelected
  )
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Evaluation qualified as Eval
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Lesson qualified as QLesson
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, fromMisoString)
import Optics.Core ((%))

-- | Open the student evaluator as a framed modal.
openStudentEvaluator :: SyncContext -> LessonId -> MisoString -> UserId -> IO ()
openStudentEvaluator r lessonId studentName userId =
  let cfg = ModalConfig (WindowChrome studentName Icon.IcnEvidence) (ModalId ("student-eval-" <> idToText lessonId <> "-" <> idToText userId)) ModalWide ModalFull Nothing
   in openFramedModalWith r.windowManager cfg (studentEvaluatorModal r lessonId userId)

-- ============================================================================
-- MODEL
-- ============================================================================

-- | Pre-computed data derived from the Document. Refreshed on every DocumentUpdated.
data ViewData = ViewData
  { userName :: !T.Text
  , lessonDate :: !Day
  , existingEvidence :: !(Maybe Evidence)
  , taskViewData :: !(Map.Map TaskId Eval.TaskViewData)
  , competenceLevelInfos :: !(Map.Map CompetenceLevelId Eval.CompetenceLevelInfo)
  , baseLessonTaskIds :: !(Set.Set TaskId)
  }
  deriving (Eq, Generic, Show)

data StudentEvalModel = StudentEvalModel
  { viewData :: !ViewData
  -- Student-specific editing state
  , taskObservations :: !(Map.Map (TaskId, CompetenceLevelId) Ability)
  , aggregatedResults :: !(Map.Map CompetenceLevelId Ability)
  , manualObservations :: !(Map.Map CompetenceLevelId Ability)
  , selectedSocialForm :: !SocialForm
  , includedTasks :: !(Set.Set TaskId)
  , expandedTaskContent :: !(Set.Set TaskId)
  , additionalTasks :: !(Set.Set TaskId)
  , aggregationStale :: !Bool
  -- Combobox state for adding tasks
  , taskSearchQuery :: !T.Text
  , taskComboboxOpen :: !Bool
  , selectedTaskToAdd :: !(Maybe TaskId)
  -- Manual observations collapse
  , manualObsExpanded :: !Bool
  -- Competence levels available for manual observation (managed by selector binding)
  , manualCompetenceLevels :: ![CompetenceLevelId]
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- ACTIONS
-- ============================================================================

data StudentEvalAction
  = DocumentUpdated !DocumentChange
  | CloseModal
  -- Task evaluation
  | SetTaskObservation !TaskId !CompetenceLevelId !Ability
  | ToggleTaskIncluded !TaskId
  | ToggleTaskContentExpanded !TaskId
  -- Add task (combobox)
  | TaskSearchChanged !T.Text
  | TaskComboboxToggled !TaskId
  | TaskComboboxOpenChanged !Bool
  | AddTask
  -- Manual observations
  | ToggleManualObsExpanded
  | AddManualObservation !CompetenceLevelId !Ability
  | RemoveManualObservation !CompetenceLevelId
  -- Aggregation + save
  | ComputeAggregation
  | SetAggregatedResult !CompetenceLevelId !Ability
  | SaveEvidence
  | DeleteEvidence
  deriving (Eq, Show)

-- ============================================================================
-- PRE-COMPUTATION
-- ============================================================================

computeViewData :: LessonId -> UserId -> Day -> T.Text -> Document -> ViewData
computeViewData lessonId userId fallbackDate fallbackName doc =
  let mLesson = QLesson.getLesson doc lessonId
      evs = QLesson.lessonEvidences doc lessonId
   in ViewData
        { userName = maybe fallbackName (.name) $ Ix.getOne (doc.users Ix.@= userId)
        , lessonDate = fromMaybe fallbackDate (mLesson >>= (.date))
        , existingEvidence = case filter (\e -> e.userId == Just userId) evs of
            (e : _) -> Just e
            [] -> Nothing
        , taskViewData = Eval.projectTasks doc.taskGroups doc.tasks
        , competenceLevelInfos = Eval.projectCompetenceLevels doc.competences doc.competenceGrids
        , baseLessonTaskIds = QLesson.lessonTaskIds doc lessonId
        }

-- ============================================================================
-- COMPONENT
-- ============================================================================

studentEvaluatorModal
  :: SyncContext
  -> LessonId
  -> UserId
  -> WindowMode
  -> M.Component p (Initializing StudentEvalModel) StudentEvalAction
studentEvaluatorModal r initialLessonId initialUserId wm =
  (deferredComponent
    (\case DocumentUpdated dc -> Just dc; _ -> Nothing)
    (initFromDocument initialLessonId initialUserId)
    update
    view
  ) { M.subs = [subscribeDocument r DocumentUpdated] }
  where
    initFromDocument :: LessonId -> UserId -> Document -> StudentEvalModel
    initFromDocument lessonId userId doc =
      let vd = computeViewData lessonId userId (read "2025-01-01") "" doc
          mLesson = QLesson.getLesson doc lessonId
          compLevels = maybe [] (.competenceLevels) mLesson
          (taskObs, aggResults, initIncluded) = case vd.existingEvidence of
            Just ev ->
              ( Map.fromList
                  [ ((tid, clid), ab)
                  | (tid, evals) <- Map.toList ev.tasks
                  , (clid, ab) <- Map.toList evals
                  ]
              , Map.fromList
                  [ (obs.competenceLevelId, obs.ability)
                  | obs <- Ix.toList ev.observations
                  ]
              , Map.keysSet ev.tasks
              )
            Nothing -> (Map.empty, Map.empty, Set.empty)
       in StudentEvalModel
            { viewData = vd
            , taskObservations = taskObs
            , aggregatedResults = aggResults
            , manualObservations = Map.empty
            , selectedSocialForm = Individual
            , includedTasks = initIncluded
            , expandedTaskContent = Set.empty
            , additionalTasks = Set.empty
            , aggregationStale = False
            , taskSearchQuery = ""
            , taskComboboxOpen = False
            , selectedTaskToAdd = Nothing
            , manualObsExpanded = False
            , manualCompetenceLevels = compLevels
            }

    -- ------------------------------------------------------------------
    -- UPDATE
    -- ------------------------------------------------------------------

    update (DocumentUpdated dc) = M.modify $ \m ->
      m { viewData = computeViewData initialLessonId initialUserId
            m.viewData.lessonDate m.viewData.userName dc.document }

    update CloseModal = M.io_ $ closeWindow wm

    -- Task observations
    update (SetTaskObservation taskId compId ability) = M.modify $ \m ->
      let current = Map.lookup (taskId, compId) m.taskObservations
          newObs =
            if current == Just ability
              then Map.delete (taskId, compId) m.taskObservations
              else Map.insert (taskId, compId) ability m.taskObservations
       in m{taskObservations = newObs, aggregationStale = not (Map.null m.aggregatedResults)}

    update (ToggleTaskIncluded taskId) = M.modify $ \m ->
      let newIncluded =
            if Set.member taskId m.includedTasks
              then Set.delete taskId m.includedTasks
              else Set.insert taskId m.includedTasks
       in m{includedTasks = newIncluded, aggregationStale = not (Map.null m.aggregatedResults)}

    update (ToggleTaskContentExpanded taskId) = M.modify $ \m ->
      let newExpanded =
            if Set.member taskId m.expandedTaskContent
              then Set.delete taskId m.expandedTaskContent
              else Set.insert taskId m.expandedTaskContent
       in m{expandedTaskContent = newExpanded}

    -- Add task (combobox)
    update (TaskSearchChanged q) = M.modify $ \m ->
      m{taskSearchQuery = q}

    update (TaskComboboxToggled tid) = M.modify $ \m ->
      m{selectedTaskToAdd = Just tid, taskComboboxOpen = False}

    update (TaskComboboxOpenChanged b) = M.modify $ \m ->
      m{taskComboboxOpen = b}

    update AddTask = M.modify $ \m ->
      case m.selectedTaskToAdd of
        Nothing -> m
        Just tid ->
          m
            { additionalTasks = Set.insert tid m.additionalTasks
            , includedTasks = Set.insert tid m.includedTasks
            , selectedTaskToAdd = Nothing
            , taskSearchQuery = ""
            , taskComboboxOpen = False
            }

    -- Manual observations
    update ToggleManualObsExpanded = M.modify $ \m ->
      m{manualObsExpanded = not m.manualObsExpanded}

    update (AddManualObservation compId ability) = M.modify $ \m ->
      let newManual = Map.insert compId ability m.manualObservations
       in m{manualObservations = newManual, aggregationStale = not (Map.null m.aggregatedResults), manualObsExpanded = True}

    update (RemoveManualObservation compId) = M.modify $ \m ->
      let newManual = Map.delete compId m.manualObservations
       in m{manualObservations = newManual, aggregationStale = not (Map.null m.aggregatedResults)}

    -- Aggregation
    update ComputeAggregation = M.modify $ \m ->
      let taskAgg =
            Map.foldrWithKey
              (\(_, compId) ability acc -> Map.insertWith max compId ability acc)
              Map.empty
              m.taskObservations
          -- Only include manual observations for levels still in the selector
          manualLevelsSet = Set.fromList m.manualCompetenceLevels
          activeManual = Map.filterWithKey (\k _ -> Set.member k manualLevelsSet) m.manualObservations
          merged = Map.unionWith max taskAgg activeManual
       in m{aggregatedResults = merged, aggregationStale = False}

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      let current = Map.lookup compId m.aggregatedResults
          newAgg =
            if current == Just ability
              then Map.delete compId m.aggregatedResults
              else Map.insert compId ability m.aggregatedResults
       in m{aggregatedResults = newAgg}

    -- Save evidence
    update SaveEvidence = do
      m <- M.get
      M.io_ $ do
        let sf = m.selectedSocialForm
            -- Build tasks map from included tasks
            allTaskIds = Set.toList m.includedTasks
            tasksMap :: Map.Map TaskId TaskEvaluations
            tasksMap =
              Map.fromList
                [ (tid, taskEvals)
                | tid <- allTaskIds
                , let taskEvals =
                        Map.fromList
                          [ (cid, ab)
                          | ((tid', cid), ab) <- Map.toList m.taskObservations
                          , tid' == tid
                          ]
                ]
        -- Build observations from aggregated results
        observations <- mapM (mkObservation sf) (Map.toList m.aggregatedResults)
        case m.viewData.existingEvidence of
          Just existingEv -> do
            -- Lock then modify existing evidence
            let lockCmd = Evidences (OnEvidences (Modify existingEv.id Lock))
                patch =
                  EvidencePatch
                    { userId = Nothing
                    , activityType = Just (existingEv.activityType, Conversation)
                    , date = Just (existingEv.date, m.viewData.lessonDate)
                    , tasks = Just (existingEv.tasks, tasksMap)
                    , oldTasks = Nothing
                    , observations = Just (existingEv.observations, Ix.fromList observations)
                    , taskRemarks = Nothing
                    , assignmentId = Nothing
                    , lessonId = Nothing
                    }
                releaseCmd = Evidences (OnEvidences (Modify existingEv.id (Release patch)))
            modifySyncDocument r lockCmd
            modifySyncDocument r releaseCmd
          Nothing -> do
            -- Create new evidence
            evidenceId <- nextId @IO @Evidence r
            let evidence =
                  Evidence
                    { id = evidenceId
                    , userId = Just initialUserId
                    , activityType = Conversation
                    , date = m.viewData.lessonDate
                    , tasks = tasksMap
                    , oldTasks = ""
                    , observations = Ix.fromList observations
                    , taskRemarks = Map.empty
                    , assignmentId = Nothing
                    , lessonId = Just initialLessonId
                    }
            modifySyncDocument r (Evidences $ OnEvidences $ Create evidence)
        closeWindow wm

    -- Delete existing evidence
    update DeleteEvidence = do
      m <- M.get
      M.io_ $ do
        case m.viewData.existingEvidence of
          Just existingEv ->
            modifySyncDocument r (Evidences $ OnEvidences $ Delete existingEv.id)
          Nothing -> pure ()
        closeWindow wm

    mkObservation :: SocialForm -> (CompetenceLevelId, Ability) -> IO Observation
    mkObservation sf (compId, ability) = do
      obsId <- nextId @IO @Observation r
      pure
        Observation
          { id = obsId
          , competenceLevelId = compId
          , socialForm = sf
          , ability = ability
          }

    -- ------------------------------------------------------------------
    -- VIEW
    -- ------------------------------------------------------------------

    view m =
      let allTaskIds = Set.union m.viewData.baseLessonTaskIds m.additionalTasks
          sortedTaskIds =
            map snd $
              Map.toAscList $
                Map.fromList
                  [ (tvd.identifier, tid)
                  | tid <- Set.toList allTaskIds
                  , Just tvd <- [Map.lookup tid m.viewData.taskViewData]
                  ]
          hasAggregatedResults = not $ Map.null m.aggregatedResults
          -- When evidence exists but all aggregated results deselected, show Delete
          canDelete = not hasAggregatedResults && not (isNothing m.viewData.existingEvidence)
          isDisabled = not hasAggregatedResults || m.aggregationStale
          actionLabel = C.translate' $ if isNothing m.viewData.existingEvidence then C.LblCreateEvidencesAction else C.LblSaveEvidences
       in Layout.vFlow Layout.hFull
            [ Layout.scrollContent $ Layout.padL $ Layout.vFlow Layout.gapM
                [ -- Task sections
                  if null sortedTaskIds
                    then Typography.muted (C.translate' C.LblLessonNoTasks)
                    else MH.div_ [class_ "space-y-4"] (map (viewTaskSection m) sortedTaskIds)
                , -- Add task
                  viewAddTaskSection m
                , -- Manual observations
                  viewManualObservationsSection m
                , -- Aggregation
                  viewAggregationSection m
                ]
            , Layout.actionFooter
                [ if canDelete
                    then
                      Button.deleteButton DeleteEvidence
                    else
                      Button.primary (Button.button actionLabel (not isDisabled, SaveEvidence))
                ]
            ]

    -- ==========================================================
    -- TASK SECTIONS
    -- ==========================================================

    viewTaskSection m taskId =
      let isExcluded = not $ Set.member taskId m.includedTasks
       in MH.div_
            [class_ "border-b pb-3"]
            [ Eval.viewTaskHeader m.viewData.taskViewData taskId isExcluded (ToggleTaskIncluded taskId) []
            , if isExcluded
                then M.text ""
                else
                  MH.div_
                    []
                    [ Eval.viewTaskContent r.formulaCache m.viewData.taskViewData m.expandedTaskContent taskId ToggleTaskContentExpanded
                    , Eval.viewTaskCompetences m.viewData.taskViewData m.viewData.competenceLevelInfos m.taskObservations taskId SetTaskObservation
                    ]
            ]

    -- ------------------------------------------------------------------
    -- ADD TASK SECTION
    -- ------------------------------------------------------------------

    viewAddTaskSection m =
      let allTaskIds = Set.union m.viewData.baseLessonTaskIds m.additionalTasks
          availableTasks =
            Map.toAscList $
              Map.fromList
                [ (tvd.identifier, tid)
                | (tid, tvd) <- Map.toList m.viewData.taskViewData
                , not $ Set.member tid allTaskIds
                ]
          filteredTasks =
            if T.null m.taskSearchQuery
              then availableTasks
              else
                filter
                  (\(ident, _) -> T.toCaseFold m.taskSearchQuery `T.isInfixOf` T.toCaseFold ident)
                  availableTasks
          comboboxOptions =
            map (\(ident, tid) -> ComboboxOption tid ident) filteredTasks
          combobox =
            singleSelectCombobox TaskSearchChanged TaskComboboxToggled TaskComboboxOpenChanged
              & withPlaceholder (fromMisoString $ C.translate' C.LblSelectTask)
              & withOptions comboboxOptions
              & withSelected (maybe Set.empty Set.singleton m.selectedTaskToAdd)
              & withSearchQuery m.taskSearchQuery
              & withIsOpen m.taskComboboxOpen
          canAdd = case m.selectedTaskToAdd of Just _ -> True; Nothing -> False
       in MH.div_
            [class_ "border-t pt-3"]
            [ Typography.h4 (C.translate' C.LblAddTask)
            , MH.div_ [class_ "mt-1"]
                [ Layout.hFlow
                    (Layout.gapS <> Layout.crossStart)
                    [ MH.div_ [class_ "flex-1"] [renderCombobox combobox]
                    , Button.primary (Button.button C.LblAdd (canAdd, AddTask))
                    ]
                ]
            ]

    -- ------------------------------------------------------------------
    -- MANUAL OBSERVATIONS SECTION
    -- ------------------------------------------------------------------

    viewManualObservationsSection m =
      Disclosure.disclosure ToggleManualObsExpanded $
        Disclosure.contents (Disclosure.titleText $ C.translate' C.LblManualObservations) m.manualObsExpanded (viewManualObservationsContent m) []

    viewManualObservationsContent m =
      MH.div_
        [class_ "space-y-3"]
        [ -- Competence level selector (3-stage: Grid → Competence → Level)
          inlineComponent
            "manual-comp-level-selector"
            ( competenceLevelSelectorComponent
                r
                (\_ -> m.manualCompetenceLevels)
                MultiStageSelectorEnabled
                0
                (selectorLens (_Ready % #manualCompetenceLevels))
            )
        , -- Ability rows for each selected competence level
          if null m.manualCompetenceLevels
            then MH.div_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translate' C.LblNoManualObservations]
            else
              MH.div_
                [class_ "space-y-1"]
                (map (viewManualObservationRow m) m.manualCompetenceLevels)
        ]

    viewManualObservationRow m compId =
      let mAbility = Map.lookup compId m.manualObservations
       in Eval.viewCompetenceRow m.viewData.competenceLevelInfos compId mAbility (AddManualObservation compId)

    -- ------------------------------------------------------------------
    -- AGGREGATION SECTION
    -- ------------------------------------------------------------------

    viewAggregationSection m =
      Eval.viewAggregationSection
        m.aggregationStale
        (not $ Map.null m.aggregatedResults)
        ComputeAggregation
        ( Eval.viewAggregatedResults m.viewData.competenceLevelInfos m.aggregatedResults
            (Eval.viewAggregatedCompetenceRow m.viewData.competenceLevelInfos SetAggregatedResult)
        )
