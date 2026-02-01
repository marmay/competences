module Competences.Frontend.Component.Planning.LessonEvaluator
  ( lessonEvaluatorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), ModifyCommand (..), ParticipationRecordsCommand (..))
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
import Competences.Document.ParticipationRecord
  ( ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  )
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
import Competences.Document.User (UserId, UserIxs)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
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
import Miso.Html.Property qualified as MH
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
  , mode :: !EvalMode
  }
  deriving (Eq, Generic, Show)

data EvalMode
  = Overview
  | StudentDetail !StudentDetailModel
  deriving (Eq, Generic, Show)

data StudentDetailModel = StudentDetailModel
  { userId :: !UserId
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

data LessonEvalAction
  = DocumentUpdated !DocumentChange
  -- Participation (immediate commands)
  | ToggleParticipation !UserId !ParticipationType
  -- Navigation
  | OpenStudentDetail !UserId
  | BackToOverview
  -- Student detail: task evaluation
  | SetTaskObservation !TaskId !CompetenceLevelId !Ability
  | ToggleTaskIncluded !TaskId
  | ToggleTaskContentExpanded !TaskId
  -- Student detail: add task
  | AddTask !TaskId
  -- Student detail: manual observations
  | AddManualObservation !CompetenceLevelId !Ability
  | RemoveManualObservation !CompetenceLevelId
  -- Student detail: aggregation + save
  | ComputeAggregation
  | SetAggregatedResult !CompetenceLevelId !Ability
  | SaveEvidence
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
        , mode = Overview
        }

    -- ------------------------------------------------------------------
    -- UPDATE
    -- ------------------------------------------------------------------

    update (DocumentUpdated dc) = M.modify $ \m ->
      let doc = dc.document
          lesson' = fromMaybe m.lesson $ Ix.getOne (doc.lessons Ix.@= m.lesson.id)
          evs = QLesson.lessonEvidences doc lesson'.id
          tids = QLesson.lessonTaskIds doc lesson'.id
          -- Also include additional tasks from current editing session
          allTids = case m.mode of
            StudentDetail sd -> Set.union tids sd.additionalTasks
            _ -> tids
       in m
            { lesson = lesson'
            , users = doc.users
            , participationRecords = doc.participationRecords Ix.@= lesson'.id
            , lessonEvidences = evs
            , lessonTaskIds = allTids
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

    -- Navigation
    update (OpenStudentDetail userId) = M.modify $ \m ->
      let -- Pre-load from existing evidence if available
          mEvidence = findStudentEvidence m userId
          sd = case mEvidence of
            Just ev ->
              let loadedObs =
                    Map.fromList
                      [ ((tid, clid), ab)
                      | (tid, evals) <- Map.toList ev.tasks
                      , (clid, ab) <- Map.toList evals
                      ]
                  loadedAgg =
                    Map.fromList
                      [ (obs.competenceLevelId, obs.ability)
                      | obs <- Ix.toList ev.observations
                      ]
               in StudentDetailModel
                    { userId = userId
                    , taskObservations = loadedObs
                    , aggregatedResults = loadedAgg
                    , manualObservations = Map.empty
                    , selectedSocialForm = Individual
                    , excludedTasks = Set.empty
                    , expandedTaskContent = Set.empty
                    , additionalTasks = Set.empty
                    , aggregationStale = False
                    }
            Nothing ->
              StudentDetailModel
                { userId = userId
                , taskObservations = Map.empty
                , aggregatedResults = Map.empty
                , manualObservations = Map.empty
                , selectedSocialForm = Individual
                , excludedTasks = Set.empty
                , expandedTaskContent = Set.empty
                , additionalTasks = Set.empty
                , aggregationStale = False
                }
       in m{mode = StudentDetail sd}

    update BackToOverview = M.modify $ \m -> m{mode = Overview}

    -- Task observations
    update (SetTaskObservation taskId compId ability) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let current = Map.lookup (taskId, compId) sd.taskObservations
              newObs =
                if current == Just ability
                  then Map.delete (taskId, compId) sd.taskObservations
                  else Map.insert (taskId, compId) ability sd.taskObservations
           in m{mode = StudentDetail sd{taskObservations = newObs, aggregationStale = not (Map.null sd.aggregatedResults)}}
        _ -> m

    update (ToggleTaskIncluded taskId) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let newExcluded =
                if Set.member taskId sd.excludedTasks
                  then Set.delete taskId sd.excludedTasks
                  else Set.insert taskId sd.excludedTasks
           in m{mode = StudentDetail sd{excludedTasks = newExcluded, aggregationStale = not (Map.null sd.aggregatedResults)}}
        _ -> m

    update (ToggleTaskContentExpanded taskId) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let newExpanded =
                if Set.member taskId sd.expandedTaskContent
                  then Set.delete taskId sd.expandedTaskContent
                  else Set.insert taskId sd.expandedTaskContent
           in m{mode = StudentDetail sd{expandedTaskContent = newExpanded}}
        _ -> m

    -- Add task
    update (AddTask taskId) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          m
            { mode = StudentDetail sd{additionalTasks = Set.insert taskId sd.additionalTasks}
            , lessonTaskIds = Set.insert taskId m.lessonTaskIds
            }
        _ -> m

    -- Manual observations
    update (AddManualObservation compId ability) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let newManual = Map.insert compId ability sd.manualObservations
           in m{mode = StudentDetail sd{manualObservations = newManual, aggregationStale = not (Map.null sd.aggregatedResults)}}
        _ -> m

    update (RemoveManualObservation compId) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let newManual = Map.delete compId sd.manualObservations
           in m{mode = StudentDetail sd{manualObservations = newManual, aggregationStale = not (Map.null sd.aggregatedResults)}}
        _ -> m

    -- Aggregation
    update ComputeAggregation = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let -- Worst ability per competence from task observations
              taskAgg =
                Map.foldrWithKey
                  (\(_, compId) ability acc -> Map.insertWith max compId ability acc)
                  Map.empty
                  sd.taskObservations
              -- Merge manual observations (override task aggregation)
              merged = Map.unionWith max taskAgg sd.manualObservations
           in m{mode = StudentDetail sd{aggregatedResults = merged, aggregationStale = False}}
        _ -> m

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      case m.mode of
        StudentDetail sd ->
          let current = Map.lookup compId sd.aggregatedResults
              newAgg =
                if current == Just ability
                  then Map.delete compId sd.aggregatedResults
                  else Map.insert compId ability sd.aggregatedResults
           in m{mode = StudentDetail sd{aggregatedResults = newAgg}}
        _ -> m

    -- Save evidence
    update SaveEvidence = do
      m <- M.get
      case m.mode of
        StudentDetail sd -> M.io_ $ do
          let sf = sd.selectedSocialForm
              lessonDate = fromMaybe (read "2025-01-01") m.lesson.date
              -- Build tasks map from task observations
              allTaskIds = Set.toList $ Set.difference m.lessonTaskIds sd.excludedTasks
              tasksMap :: Map.Map TaskId TaskEvaluations
              tasksMap =
                Map.fromList
                  [ (tid, taskEvals)
                  | tid <- allTaskIds
                  , let taskEvals =
                          Map.fromList
                            [ (cid, ab)
                            | ((tid', cid), ab) <- Map.toList sd.taskObservations
                            , tid' == tid
                            ]
                  ]
          -- Build observations from aggregated results
          observations <- mapM (mkObservation sf) (Map.toList sd.aggregatedResults)
          case findStudentEvidence m sd.userId of
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
                      , userId = Just sd.userId
                      , activityType = Conversation
                      , date = lessonDate
                      , tasks = tasksMap
                      , oldTasks = ""
                      , observations = Ix.fromList observations
                      , assignmentId = Nothing
                      , lessonId = Just m.lesson.id
                      }
              modifySyncDocument r (Evidences $ OnEvidences $ Create evidence)
          -- Return to overview
        _ -> pure ()
      M.modify $ \m' -> m'{mode = Overview}

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

    view' m = case m.mode of
      Overview -> viewOverview m
      StudentDetail sd -> viewStudentDetail m sd

    -- ==========================================================
    -- OVERVIEW MODE
    -- ==========================================================

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

    -- ==========================================================
    -- STUDENT DETAIL MODE
    -- ==========================================================

    viewStudentDetail m sd =
      let userName = case Ix.getOne (m.users Ix.@= sd.userId) of
            Just u -> u.name
            Nothing -> T.pack (show sd.userId)
          -- Task IDs for this lesson, sorted by identifier
          sortedTaskIds =
            map (.id) $
              Ix.toAscList (Proxy @TaskIdentifier) $
                m.tasks Ix.@+ Set.toList m.lessonTaskIds
          hasAggregatedResults = not $ Map.null sd.aggregatedResults
          isDisabled = not hasAggregatedResults || sd.aggregationStale
          existingEvidence = findStudentEvidence m sd.userId
          actionLabel = C.translate' $ if existingEvidence == Nothing then C.LblCreateEvidencesAction else C.LblSaveEvidences
       in MH.div_
            [class_ "p-4 space-y-4 overflow-y-auto"]
            [ -- Back button + student name
              MH.div_
                [class_ "flex items-center gap-2 mb-2"]
                [ Button.buttonGhost (C.translate' C.LblBack)
                    & Button.withSize Button.Small
                    & Button.withClick BackToOverview
                    & Button.renderButton
                , Typography.h3 (ms userName)
                ]
            , -- Task sections
              if null sortedTaskIds
                then Typography.muted (C.translate' C.LblLessonNoTasks)
                else MH.div_ [class_ "space-y-4"] (map (viewTaskSection m sd) sortedTaskIds)
            , -- Add task
              viewAddTaskSection m sd
            , -- Manual observations
              viewManualObservationsSection m sd
            , -- Aggregation
              viewAggregationSection m sd
            , -- Save button
              MH.div_
                [class_ "flex justify-end"]
                [ MH.button_
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

    -- ------------------------------------------------------------------
    -- TASK SECTIONS (in student detail)
    -- ------------------------------------------------------------------

    viewTaskSection m sd taskId =
      let isExcluded = Set.member taskId sd.excludedTasks
       in MH.div_
            [class_ "border-b pb-3"]
            [ viewTaskHeader m sd taskId isExcluded
            , if isExcluded
                then M.text ""
                else
                  MH.div_
                    []
                    [ viewTaskContent m sd taskId
                    , viewTaskCompetenceEvaluations m sd taskId
                    ]
            ]

    viewTaskHeader m _sd taskId isExcluded =
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

    viewTaskContent m sd taskId =
      case Ix.getOne (m.tasks Ix.@= taskId) of
        Nothing -> M.text ""
        Just task ->
          let content = getTaskContent m.taskGroups task
              isContentExpanded = Set.member taskId sd.expandedTaskContent
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

    viewTaskCompetenceEvaluations m sd taskId =
      case Ix.getOne (m.tasks Ix.@= taskId) of
        Nothing -> M.text ""
        Just task ->
          let attrs = getTaskAttributes m.taskGroups task
              compIds = attrs.primary <> attrs.secondary
           in if null compIds
                then MH.div_ [class_ "mt-2"] [Typography.muted (C.translate' C.LblNoCompetences)]
                else MH.div_ [class_ "mt-2 space-y-1"] (map (viewCompetenceEvaluation sd taskId) compIds)

    viewCompetenceEvaluation sd taskId compId =
      let currentAbility = Map.lookup (taskId, compId) sd.taskObservations
       in viewCompetenceRow compId currentAbility (SetTaskObservation taskId)

    -- ------------------------------------------------------------------
    -- ADD TASK SECTION
    -- ------------------------------------------------------------------

    viewAddTaskSection m _sd =
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

    viewManualObservationsSection m sd =
      let manualObs = Map.toList sd.manualObservations
          -- Competences from lesson (via lesson.competenceLevels)
          lessonCompLevels = m.lesson.competenceLevels
          -- Competences already covered by task evaluations or manual obs
          taskCompIds = Set.fromList [compId | ((_, compId), _) <- Map.toList sd.taskObservations]
          manualCompIds = Set.fromList (Map.keys sd.manualObservations)
          coveredCompIds = Set.union taskCompIds manualCompIds
          -- Available for manual: lesson competence levels not yet covered
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
                        (map viewManualCompetenceRow availableCompIds)
                    ]
            ]

    viewManualObservationRow m (compId, ability) =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName m compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn compId (Just ability) (\cid ab -> AddManualObservation cid ab)) abilities)
        , MH.button_
            [ class_ "text-xs text-muted-foreground hover:text-destructive cursor-pointer"
            , MH.onClick (RemoveManualObservation compId)
            ]
            [M.text "x"]
        ]

    viewManualCompetenceRow compId =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName' compId
        , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn compId Nothing (\cid ab -> AddManualObservation cid ab)) abilities)
        ]

    -- ------------------------------------------------------------------
    -- AGGREGATION SECTION
    -- ------------------------------------------------------------------

    viewAggregationSection m sd =
      MH.div_
        [class_ "border-t pt-3"]
        [ MH.div_
            [class_ "flex items-center justify-between mb-2"]
            [ Typography.h4 (C.translate' C.LblAggregatedResults)
            , MH.div_
                [class_ "flex items-center gap-2"]
                [ if sd.aggregationStale
                    then MH.span_ [class_ "text-xs text-yellow-700"] [M.text $ C.translate' C.LblAggregationStale]
                    else M.text ""
                , MH.button_
                    [ MH.onClick ComputeAggregation
                    , class_ "bg-primary text-primary-foreground px-3 py-1 text-sm rounded hover:bg-primary/90"
                    ]
                    [M.text $ C.translate' C.LblComputeAggregation]
                ]
            ]
        , if Map.null sd.aggregatedResults
            then Typography.muted (C.translate' C.LblComputeAggregationHint)
            else viewAggregatedResults m sd
        ]

    viewAggregatedResults m sd =
      let compIds = Set.fromList [compId | (compId, _) <- Map.keys sd.aggregatedResults]
          competencesWithResults = Ix.toAscList (Proxy @Order) $ m.competences Ix.@+ Set.toList compIds
          gridIds = Set.fromList $ map (.competenceGridId) competencesWithResults
          sortedGrids = Ix.toAscList (Proxy @Order) $ m.competenceGrids Ix.@+ Set.toList gridIds
       in MH.div_ [class_ "space-y-3"] (map (viewGridAggregation m sd) sortedGrids)

    viewGridAggregation m sd grid =
      let gridCompetences = Ix.toAscList (Proxy @Order) $ m.competences Ix.@= grid.id
          resultsForGrid =
            [ (compLevelId, ability)
            | comp <- gridCompetences
            , (compLevelId@(compId, _), ability) <- Map.toList sd.aggregatedResults
            , compId == comp.id
            ]
       in if null resultsForGrid
            then M.text ""
            else
              MH.div_
                [class_ "border border-border rounded bg-muted/50"]
                [ MH.div_ [class_ "px-3 py-1 border-b bg-muted font-medium text-sm"] [M.text $ ms grid.title]
                , MH.div_ [class_ "p-2 space-y-1"] (map (viewAggregatedCompetence m sd) resultsForGrid)
                ]

    viewAggregatedCompetence m _sd (compId, ability) =
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

    viewCompetenceName' compId =
      -- Simplified version without model access (for manual section where model isn't in scope)
      MH.span_ [class_ "flex-1 text-sm"] [M.text $ ms $ T.pack (show compId)]

    viewCompetenceRow compId currentAbility mkAction =
      MH.div_
        [class_ "flex items-center gap-2"]
        [ viewCompetenceName' compId
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
