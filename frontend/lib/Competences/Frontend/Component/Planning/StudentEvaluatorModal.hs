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
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridIxs
  , Document (..)
  , LevelInfo (..)
  , Order
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
  , TaskAttributes (..)
  , TaskGroup
  , TaskGroupIxs
  , TaskId
  , TaskIdentifier (..)
  , TaskIxs
  , getTaskAttributes
  , getTaskContent
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText)
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
import Competences.Frontend.View.Disclosure qualified as Disclosure
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
import Miso.String (ms)

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
  , excludedTasks :: !(Set.Set TaskId)
  , expandedTaskContent :: !(Set.Set TaskId)
  , additionalTasks :: !(Set.Set TaskId)
  , aggregationStale :: !Bool
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
  -- Add task
  | AddTask !TaskId
  -- Manual observations
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
      let (taskObs, aggResults) = case mEvidence of
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
              )
            Nothing -> (Map.empty, Map.empty)
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
            , excludedTasks = Set.empty
            , expandedTaskContent = Set.empty
            , additionalTasks = Set.empty
            , aggregationStale = False
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
      let newExcluded =
            if Set.member taskId m.excludedTasks
              then Set.delete taskId m.excludedTasks
              else Set.insert taskId m.excludedTasks
       in m{excludedTasks = newExcluded, aggregationStale = not (Map.null m.aggregatedResults)}

    update (ToggleTaskContentExpanded taskId) = M.modify $ \m ->
      let newExpanded =
            if Set.member taskId m.expandedTaskContent
              then Set.delete taskId m.expandedTaskContent
              else Set.insert taskId m.expandedTaskContent
       in m{expandedTaskContent = newExpanded}

    -- Add task
    update (AddTask taskId) = M.modify $ \m ->
      m
        { additionalTasks = Set.insert taskId m.additionalTasks
        , lessonTaskIds = Set.insert taskId m.lessonTaskIds
        }

    -- Manual observations
    update (AddManualObservation compId ability) = M.modify $ \m ->
      let newManual = Map.insert compId ability m.manualObservations
       in m{manualObservations = newManual, aggregationStale = not (Map.null m.aggregatedResults)}

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
            -- Build tasks map from task observations
            allTaskIds = Set.toList $ Set.difference m.lessonTaskIds m.excludedTasks
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
      let isExcluded = Set.member taskId m.excludedTasks
       in MH.div_
            [class_ "border-b pb-3"]
            [ viewTaskHeader m taskId isExcluded
            , if isExcluded
                then M.text ""
                else
                  MH.div_
                    []
                    [ viewTaskContent m taskId
                    , viewTaskCompetenceEvaluations m taskId
                    ]
            ]

    viewTaskHeader m taskId isExcluded =
      case Ix.getOne (m.tasks Ix.@= taskId) of
        Nothing -> MH.div_ [] [M.text $ C.translate' C.LblTaskNotFound <> ": " <> ms (show taskId)]
        Just task ->
          let TaskIdentifier identifier = task.identifier
              toggleClass =
                if isExcluded
                  then "px-2 py-1 rounded text-sm cursor-pointer border border-muted-foreground text-muted-foreground hover:bg-muted/50"
                  else "px-2 py-1 rounded text-sm cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90"
           in MH.div_
                [class_ "mt-3 mb-1 flex items-center justify-between"]
                [ Typography.h4 $ C.translate' C.LblTaskPrefix <> ms identifier
                , MH.button_
                    [class_ toggleClass, MH.onClick (ToggleTaskIncluded taskId)]
                    [M.text $ C.translate' $ if isExcluded then C.LblIncludeTask else C.LblExcludeTask]
                ]

    viewTaskContent m taskId =
      case Ix.getOne (m.tasks Ix.@= taskId) of
        Nothing -> M.text ""
        Just task ->
          let content = getTaskContent m.taskGroups task
              isContentExpanded = Set.member taskId m.expandedTaskContent
           in case content of
                Nothing -> M.text ""
                Just c
                  | c == mempty -> M.text ""
                  | otherwise ->
                      MH.div_
                        [class_ "mb-2"]
                        [ MH.div_
                            [ class_ "flex items-center gap-2 cursor-pointer hover:bg-muted/50 px-2 py-1 rounded"
                            , MH.onClick (ToggleTaskContentExpanded taskId)
                            ]
                            [ Disclosure.disclosureChevron isContentExpanded
                            , MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translate' C.LblTaskStatement]
                            ]
                        , if isContentExpanded
                            then MH.div_ [class_ "ml-6 mb-2 prose prose-sm prose-stone max-w-none"] [renderRichText c]
                            else M.text ""
                        ]

    viewTaskCompetenceEvaluations m taskId =
      case Ix.getOne (m.tasks Ix.@= taskId) of
        Nothing -> M.text ""
        Just task ->
          let attrs = getTaskAttributes m.taskGroups task
              compIds = attrs.primary <> attrs.secondary
           in if null compIds
                then MH.div_ [class_ "mt-2"] [Typography.muted (C.translate' C.LblNoCompetences)]
                else MH.div_ [class_ "mt-2 space-y-1"] (map (viewCompetenceEvaluation m taskId) compIds)

    viewCompetenceEvaluation m taskId compId =
      let currentAbility = Map.lookup (taskId, compId) m.taskObservations
       in viewCompetenceRow m compId currentAbility (SetTaskObservation taskId)

    -- ------------------------------------------------------------------
    -- ADD TASK SECTION
    -- ------------------------------------------------------------------

    viewAddTaskSection m =
      let availableTasks =
            filter (\t -> not $ Set.member t.id m.lessonTaskIds) $
              Ix.toAscList (Proxy @TaskIdentifier) m.tasks
       in if null availableTasks
            then M.text ""
            else
              MH.div_
                [class_ "border-t pt-3"]
                [ Typography.h4 (C.translate' C.LblAddTask)
                , MH.div_
                    [class_ "flex flex-wrap gap-1 mt-1"]
                    (map viewAddTaskButton availableTasks)
                ]

    viewAddTaskButton task =
      let TaskIdentifier identifier = task.identifier
       in MH.button_
            [ class_ "px-2 py-1 rounded text-xs cursor-pointer bg-secondary text-secondary-foreground hover:bg-secondary/80"
            , MH.onClick (AddTask task.id)
            ]
            [M.text $ ms identifier]

    -- ------------------------------------------------------------------
    -- MANUAL OBSERVATIONS SECTION
    -- ------------------------------------------------------------------

    viewManualObservationsSection m =
      let manualObs = Map.toList m.manualObservations
          lessonCompLevels = m.lesson.competenceLevels
          taskCompIds = Set.fromList [compId | ((_, compId), _) <- Map.toList m.taskObservations]
          manualCompIds = Set.fromList (Map.keys m.manualObservations)
          coveredCompIds = Set.union taskCompIds manualCompIds
          availableCompIds = filter (\c -> not $ Set.member c coveredCompIds) lessonCompLevels
       in MH.div_
            [class_ "border-t pt-3"]
            [ Typography.h4 (C.translate' C.LblManualObservations)
            , -- Existing manual observations
              if null manualObs
                then MH.div_ [class_ "text-sm text-muted-foreground mt-1"] [M.text $ C.translate' C.LblNoManualObservations]
                else
                  MH.div_
                    [class_ "space-y-1 mt-1"]
                    (map (viewManualObservationRow m) manualObs)
            , -- Add buttons for available competence levels
              if null availableCompIds
                then M.text ""
                else
                  MH.div_
                    [class_ "mt-2"]
                    [ MH.div_ [class_ "text-xs text-muted-foreground mb-1"] [M.text $ C.translate' C.LblAddObservation]
                    , MH.div_
                        [class_ "space-y-1"]
                        (map (viewManualCompetenceRow m) availableCompIds)
                    ]
            ]

    viewManualObservationRow m (compId, ability) =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName m compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn compId (Just ability) AddManualObservation) abilities)
        , MH.button_
            [ class_ "text-xs text-muted-foreground hover:text-destructive cursor-pointer"
            , MH.onClick (RemoveManualObservation compId)
            ]
            [M.text "x"]
        ]

    viewManualCompetenceRow m compId =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName m compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn compId Nothing AddManualObservation) abilities)
        ]

    -- ------------------------------------------------------------------
    -- AGGREGATION SECTION
    -- ------------------------------------------------------------------

    viewAggregationSection m =
      MH.div_
        [class_ "border-t pt-3"]
        [ MH.div_
            [class_ "flex items-center justify-between mb-2"]
            [ Typography.h4 (C.translate' C.LblAggregatedResults)
            , MH.div_
                [class_ "flex items-center gap-2"]
                [ if m.aggregationStale
                    then MH.span_ [class_ "text-xs text-yellow-700"] [M.text $ C.translate' C.LblAggregationStale]
                    else M.text ""
                , MH.button_
                    [ MH.onClick ComputeAggregation
                    , class_ "bg-primary text-primary-foreground px-3 py-1 text-sm rounded hover:bg-primary/90"
                    ]
                    [M.text $ C.translate' C.LblComputeAggregation]
                ]
            ]
        , if Map.null m.aggregatedResults
            then Typography.muted (C.translate' C.LblComputeAggregationHint)
            else viewAggregatedResults m
        ]

    viewAggregatedResults m =
      let compIds = Set.fromList [compId | (compId, _) <- Map.keys m.aggregatedResults]
          competencesWithResults = Ix.toAscList (Proxy @Order) $ m.competences Ix.@+ Set.toList compIds
          gridIds = Set.fromList $ map (.competenceGridId) competencesWithResults
          sortedGrids = Ix.toAscList (Proxy @Order) $ m.competenceGrids Ix.@+ Set.toList gridIds
       in MH.div_ [class_ "space-y-3"] (map (viewGridAggregation m) sortedGrids)

    viewGridAggregation m grid =
      let gridCompetences = Ix.toAscList (Proxy @Order) $ m.competences Ix.@= grid.id
          resultsForGrid =
            [ (compLevelId, ability)
            | comp <- gridCompetences
            , (compLevelId@(compId, _), ability) <- Map.toList m.aggregatedResults
            , compId == comp.id
            ]
       in if null resultsForGrid
            then M.text ""
            else
              MH.div_
                [class_ "border border-border rounded bg-muted/50"]
                [ MH.div_ [class_ "px-3 py-1 border-b bg-muted font-medium text-sm"] [M.text $ ms grid.title]
                , MH.div_ [class_ "p-2 space-y-1"] (map (viewAggregatedCompetence m) resultsForGrid)
                ]

    viewAggregatedCompetence m (compId, ability) =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName m compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAggAbilityBtn compId ability) abilities)
        ]

    viewAggAbilityBtn compId currentAbility ability =
      let isSelected = currentAbility == ability
          buttonClass =
            if isSelected
              then "bg-primary text-primary-foreground px-2 py-0.5 text-xs rounded"
              else "bg-secondary text-secondary-foreground px-2 py-0.5 text-xs rounded hover:bg-secondary/80"
       in MH.button_
            [class_ buttonClass, MH.onClick (SetAggregatedResult compId ability)]
            [M.text $ C.translate' $ C.LblAbility ability]

    -- ------------------------------------------------------------------
    -- SHARED VIEW HELPERS
    -- ------------------------------------------------------------------

    viewCompetenceName m compId =
      let (competenceId, level) = compId
          competenceM = Ix.getOne (m.competences Ix.@= competenceId)
          name = case competenceM of
            Nothing -> C.translate' C.LblCompetence <> " " <> ms (T.pack (show compId))
            Just comp -> ms $ maybe (comp.description <> " - " <> T.pack (show level)) (.description) (comp.levels Map.!? level)
       in MH.span_ [class_ "flex-1 text-sm"] [M.text name]

    viewCompetenceRow m compId currentAbility mkAction =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName m compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn compId currentAbility mkAction) abilities)
        ]

    viewAbilityBtn compId currentAbility mkAction ability =
      let isSelected = currentAbility == Just ability
          buttonClass =
            if isSelected
              then "bg-primary text-primary-foreground px-2 py-0.5 text-xs rounded"
              else "bg-secondary text-secondary-foreground px-2 py-0.5 text-xs rounded hover:bg-secondary/80"
       in MH.button_
            [class_ buttonClass, MH.onClick (mkAction compId ability)]
            [M.text $ C.translate' $ C.LblAbility ability]
