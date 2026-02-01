-- |
-- Module      : Competences.Frontend.Component.Planning.StudentEvaluatorModal
-- Description : Modal dialog for evaluating a single student in a lesson
--
-- Opened from the LessonEvaluator overview. Provides task evaluation,
-- manual observations, aggregation, and evidence creation/update for
-- one student.
module Competences.Frontend.Component.Planning.StudentEvaluatorModal
  ( studentEvaluatorModal
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), ModifyCommand (..))
import Competences.Command.Evidences (EvidencePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence
  , CompetenceGrid
  , CompetenceGridIxs
  , Document (..)
  , User (..)
  )
import Competences.Document.Competence (CompetenceIxs, CompetenceLevelId)
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  , TaskEvaluations
  , abilities
  )
import Competences.Document.Lesson (Lesson (..))
import Competences.Document.Task
  ( Task (..)
  , TaskGroup
  , TaskGroupIxs
  , TaskId
  , TaskIdentifier (..)
  , TaskIxs
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (WindowManagerRef, closeModal)
import Competences.Frontend.SyncContext.WindowManager qualified as WM
import Competences.Frontend.View.Button qualified as Button
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
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Lesson qualified as QLesson
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MH
import Miso.String (fromMisoString, ms)

-- ============================================================================
-- MODEL
-- ============================================================================

data StudentEvalModel = StudentEvalModel
  { lesson :: !Lesson
  , userId :: !UserId
  , userName :: !T.Text
  -- Document-derived data (refreshed on each DocumentUpdated)
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , lessonTaskIds :: !(Set.Set TaskId)
  , lessonEvidences :: ![Evidence]
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
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

studentEvaluatorModal
  :: SyncContext
  -> WindowManagerRef
  -> Lesson
  -> UserId
  -> T.Text
  -> Maybe Evidence
  -> M.Component WM.Model StudentEvalModel StudentEvalAction
studentEvaluatorModal r modalMgr initialLesson initialUserId initialUserName mEvidence =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel =
      let (taskObs, aggResults, initIncluded) = case mEvidence of
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
            { lesson = initialLesson
            , userId = initialUserId
            , userName = initialUserName
            , tasks = Ix.empty
            , taskGroups = Ix.empty
            , competences = Ix.empty
            , competenceGrids = Ix.empty
            , lessonTaskIds = Set.empty
            , lessonEvidences = []
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
            }

    -- ------------------------------------------------------------------
    -- UPDATE
    -- ------------------------------------------------------------------

    update (DocumentUpdated dc) = M.modify $ \m ->
      let doc = dc.document
          lesson' = fromMaybe m.lesson $ Ix.getOne (doc.lessons Ix.@= m.lesson.id)
          evs = QLesson.lessonEvidences doc lesson'.id
          tids = QLesson.lessonTaskIds doc lesson'.id
          allTids = Set.union tids m.additionalTasks
          userName' = case Ix.getOne (doc.users Ix.@= m.userId) of
            Just u -> u.name
            Nothing -> m.userName
       in m
            { lesson = lesson'
            , userName = userName'
            , lessonEvidences = evs
            , lessonTaskIds = allTids
            , tasks = doc.tasks
            , taskGroups = doc.taskGroups
            , competences = doc.competences
            , competenceGrids = doc.competenceGrids
            }

    update CloseModal = M.io_ $ closeModal modalMgr

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
            , lessonTaskIds = Set.insert tid m.lessonTaskIds
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
          merged = Map.unionWith max taskAgg m.manualObservations
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
            lessonDate = fromMaybe (read "2025-01-01") m.lesson.date
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
        case findStudentEvidence m of
          Just existingEv -> do
            -- Lock then modify existing evidence
            let lockCmd = Evidences (OnEvidences (Modify existingEv.id Lock))
                patch =
                  EvidencePatch
                    { userId = Nothing
                    , activityType = Just (existingEv.activityType, Conversation)
                    , date = Just (existingEv.date, lessonDate)
                    , tasks = Just (existingEv.tasks, tasksMap)
                    , oldTasks = Nothing
                    , observations = Just (existingEv.observations, Ix.fromList observations)
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
                    , userId = Just m.userId
                    , activityType = Conversation
                    , date = lessonDate
                    , tasks = tasksMap
                    , oldTasks = ""
                    , observations = Ix.fromList observations
                    , assignmentId = Nothing
                    , lessonId = Just m.lesson.id
                    }
            modifySyncDocument r (Evidences $ OnEvidences $ Create evidence)
        closeModal modalMgr

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

    findStudentEvidence :: StudentEvalModel -> Maybe Evidence
    findStudentEvidence m =
      case filter (\e -> e.userId == Just m.userId) m.lessonEvidences of
        (e : _) -> Just e
        [] -> Nothing

    -- ------------------------------------------------------------------
    -- VIEW
    -- ------------------------------------------------------------------

    view m =
      let sortedTaskIds =
            map (.id) $
              Ix.toAscList (Proxy @TaskIdentifier) $
                m.tasks Ix.@+ Set.toList m.lessonTaskIds
          hasAggregatedResults = not $ Map.null m.aggregatedResults
          isDisabled = not hasAggregatedResults || m.aggregationStale
          existingEvidence = findStudentEvidence m
          actionLabel = C.translate' $ if isNothing existingEvidence then C.LblCreateEvidencesAction else C.LblSaveEvidences
       in MH.div_
            [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg"
            , class_ "w-[900px] max-w-[95vw] max-h-[90vh] flex flex-col"
            ]
            [ Modal.modalHeader (ms m.userName) CloseModal
            , MH.div_
                [class_ "px-6 py-4 space-y-4 overflow-y-auto flex-1"]
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
            , Modal.modalFooter
                [ Button.buttonSecondary (C.translate' C.LblCancel)
                    & Button.withClick CloseModal
                    & Button.renderButton
                , MH.button_
                    ( [ MH.onClick SaveEvidence
                      , class_ $
                          if isDisabled
                            then "bg-muted text-muted-foreground px-4 py-2 rounded cursor-not-allowed"
                            else "bg-ability-success text-primary-foreground px-4 py-2 rounded hover:bg-ability-success/90"
                      ]
                        <> [MH.disabled_ | isDisabled]
                    )
                    [M.text actionLabel]
                ]
            ]

    -- ==========================================================
    -- TASK SECTIONS
    -- ==========================================================

    viewTaskSection m taskId =
      let isExcluded = not $ Set.member taskId m.includedTasks
       in MH.div_
            [class_ "border-b pb-3"]
            [ Eval.viewTaskHeader m.tasks taskId isExcluded (ToggleTaskIncluded taskId) []
            , if isExcluded
                then M.text ""
                else
                  MH.div_
                    []
                    [ Eval.viewTaskContent m.tasks m.taskGroups m.expandedTaskContent taskId ToggleTaskContentExpanded
                    , Eval.viewTaskCompetences m.tasks m.taskGroups m.competences m.taskObservations taskId SetTaskObservation
                    ]
            ]

    -- ------------------------------------------------------------------
    -- ADD TASK SECTION
    -- ------------------------------------------------------------------

    viewAddTaskSection m =
      let availableTasks =
            filter (\t -> not $ Set.member t.id m.lessonTaskIds) $
              Ix.toAscList (Proxy @TaskIdentifier) m.tasks
          filteredTasks =
            if T.null m.taskSearchQuery
              then availableTasks
              else
                filter
                  ( \t ->
                      let TaskIdentifier i = t.identifier
                       in T.toCaseFold m.taskSearchQuery `T.isInfixOf` T.toCaseFold i
                  )
                  availableTasks
          comboboxOptions =
            map
              (\t -> let TaskIdentifier i = t.identifier in ComboboxOption t.id i)
              filteredTasks
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
            , MH.div_
                [class_ "flex gap-2 mt-1 items-start"]
                [ MH.div_ [class_ "flex-1"] [renderCombobox combobox]
                , Button.buttonSecondary (C.translate' C.LblAdd)
                    & Button.withClick AddTask
                    & Button.withDisabled (not canAdd)
                    & Button.renderButton
                ]
            ]

    -- ------------------------------------------------------------------
    -- MANUAL OBSERVATIONS SECTION
    -- ------------------------------------------------------------------

    viewManualObservationsSection m =
      Disclosure.collapsible
        m.manualObsExpanded
        ToggleManualObsExpanded
        (MH.span_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblManualObservations])
        (viewManualObservationsContent m)

    viewManualObservationsContent m =
      let manualObs = Map.toList m.manualObservations
          lessonCompLevels = m.lesson.competenceLevels
          taskCompIds = Set.fromList [compId | ((_, compId), _) <- Map.toList m.taskObservations]
          manualCompIds = Set.fromList (Map.keys m.manualObservations)
          coveredCompIds = Set.union taskCompIds manualCompIds
          availableCompIds = filter (\c -> not $ Set.member c coveredCompIds) lessonCompLevels
       in MH.div_
            [class_ "space-y-2"]
            [ -- Existing manual observations
              if null manualObs
                then MH.div_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translate' C.LblNoManualObservations]
                else
                  MH.div_
                    [class_ "space-y-1"]
                    (map (viewManualObservationRow m) manualObs)
            , -- Add buttons for available competence levels
              if null availableCompIds
                then M.text ""
                else
                  MH.div_
                    []
                    [ MH.div_ [class_ "text-xs text-muted-foreground mb-1"] [M.text $ C.translate' C.LblAddObservation]
                    , MH.div_
                        [class_ "space-y-1"]
                        (map (viewManualCompetenceRow m) availableCompIds)
                    ]
            ]

    viewManualObservationRow m (compId, ability) =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ Eval.viewCompetenceName m.competences compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (Eval.viewAbilityBtn (Just ability) (AddManualObservation compId)) abilities)
        , MH.button_
            [ class_ "text-xs text-muted-foreground hover:text-destructive cursor-pointer"
            , MH.onClick (RemoveManualObservation compId)
            ]
            [M.text "x"]
        ]

    viewManualCompetenceRow m compId =
      Eval.viewCompetenceRow m.competences compId Nothing (AddManualObservation compId)

    -- ------------------------------------------------------------------
    -- AGGREGATION SECTION
    -- ------------------------------------------------------------------

    viewAggregationSection m =
      Eval.viewAggregationSection
        m.aggregationStale
        (not $ Map.null m.aggregatedResults)
        ComputeAggregation
        ( Eval.viewAggregatedResults m.competences m.competenceGrids m.aggregatedResults
            (Eval.viewAggregatedCompetenceRow m.competences SetAggregatedResult)
        )
