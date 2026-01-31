module Competences.Frontend.Component.Assignment.EvaluatorDetail
  ( evaluatorDetailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Command.Evidences (EvidencePatch (..))
import Competences.Document (Assignment (..), Competence (..), CompetenceGrid (..), CompetenceGridIxs, Document (..), LevelInfo (..), Order, Solution (..), SolutionId, SolutionIxs, SolutionType (..), User (..))
import Competences.Document.Competence (CompetenceIxs, CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..), TaskEvaluations, abilities, socialForms)
import Competences.Document.Task (Task (..), TaskAttributes (..), TaskGroup, TaskGroupIxs, TaskId, TaskIdentifier (..), TaskIxs, getTaskAttributes, getTaskContent)
import Competences.Document.User (UserId, UserIxs)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.TaskStatus (viewCompactTaskStatus)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus (..), taskCompletionStatuses)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)

-- | Find evidences for a specific date, keyed by student.
-- Used to filter assignmentEvidences by the current evaluationDate at each usage site.
evidencesForDate :: Day -> [Evidence] -> Map.Map UserId Evidence
evidencesForDate day evs = Map.fromList
  [ (uid, ev)
  | ev <- evs
  , ev.date == day
  , Just uid <- [ev.userId]
  ]

-- | Detail view for evaluating an assignment
-- The mode type parameter allows this to work with any mode type
evaluatorDetailView
  :: SyncContext
  -> Assignment
  -> M.View (SD.Model Assignment mode) (SD.Action mode)
evaluatorDetailView r assignment =
  V.component
    ("assignment-evaluator-" <> M.ms (show assignment.id))
    (evaluatorComponent r assignment)

-- | Internal model for the evaluator component
-- Tracks per-task observations, aggregated results, and selected students
-- Only stores the document subsets actually needed (not the full Document)
data EvaluatorModel = EvaluatorModel
  { assignment :: !Assignment
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , users :: !(Ix.IxSet UserIxs User)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , solutions :: !(Ix.IxSet SolutionIxs Solution)
  -- Map from (TaskId, CompetenceLevelId) to Ability - applies to all selected students
  , taskObservations :: !(Map.Map (TaskId, CompetenceLevelId) Ability)
  -- Aggregated results (worst ability per competence) - editable before Evidence creation
  , aggregatedResults :: !(Map.Map CompetenceLevelId Ability)
  -- Students selected for Evidence creation
  , selectedStudents :: !(Set.Set UserId)
  -- Social form for the evaluation (Individual or Group)
  , selectedSocialForm :: !SocialForm
  -- Tasks excluded from evaluation (toggled off by teacher)
  , excludedTasks :: !(Set.Set TaskId)
  -- Date for the evidence (defaults to assignment date, can be overridden)
  , evaluationDate :: !Day
  -- Which task contents are expanded (collapsed by default)
  , expandedTaskContent :: !(Set.Set TaskId)
  -- Which Results solutions are collapsed (expanded by default for Results type)
  , collapsedResults :: !(Set.Set SolutionId)
  -- All evidences for this assignment (any date); filter by evaluationDate at usage sites
  , assignmentEvidences :: ![Evidence]
  -- Which student's evidence is currently being edited (loaded via banner)
  , editingEvidence :: !(Maybe UserId)
  -- True when task observations changed after last aggregation computation
  , aggregationStale :: !Bool
  -- Per-student task completion statuses (pre-computed from document)
  , taskStatuses :: !(Map.Map UserId (Map.Map TaskId TaskCompletionStatus))
  }
  deriving (Eq, Generic, Show)

data EvaluatorAction
  = UpdateDocument !DocumentChange
  | SetTaskObservationForAll !TaskId !CompetenceLevelId !Ability
  | ToggleStudentSelection !UserId
  | SetSocialForm !SocialForm
  | ComputeAggregation -- Compute aggregated results from task observations
  | SetAggregatedResult !CompetenceLevelId !Ability -- Edit aggregated result
  | CreateEvidences
  | ToggleTaskIncluded !TaskId -- Toggle whether a task is included in evaluation
  | SetEvaluationDate !MisoString -- Set the date for evidence creation (YYYY-MM-DD format)
  | ToggleTaskContentExpanded !TaskId -- Toggle expand/collapse for task content
  | ToggleSolutionExpanded !SolutionId -- Toggle expand/collapse for a solution
  | LoadStudentEvidence !UserId -- Load existing evidence data into evaluator
  | ResetLoadedEvidence -- Clear loaded evidence, reset to fresh evaluation
  deriving (Eq, Show)

-- | The evaluator component with its own state management
evaluatorComponent :: SyncContext -> Assignment -> M.Component p EvaluatorModel EvaluatorAction
evaluatorComponent r assignment =
  (M.component model update view')
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      EvaluatorModel
        { assignment = assignment
        , tasks = Ix.empty
        , taskGroups = Ix.empty
        , users = Ix.empty
        , competences = Ix.empty
        , competenceGrids = Ix.empty
        , solutions = Ix.empty
        , taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , selectedStudents = Set.empty
        , selectedSocialForm = Individual
        , excludedTasks = Set.empty
        , evaluationDate = assignment.assignmentDate
        , expandedTaskContent = Set.empty
        , collapsedResults = Set.empty
        , assignmentEvidences = []
        , editingEvidence = Nothing
        , aggregationStale = False
        , taskStatuses = Map.empty
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = maybe m.assignment id $ Ix.getOne (doc.assignments Ix.@= m.assignment.id)
          -- All evidences for this assignment (any date)
          asmtEvidences = Ix.toList $ doc.evidences Ix.@= m.assignment.id :: [Evidence]
          -- Pre-compute per-student task statuses for all assignment students
          relevantTasks = Ix.toList $ doc.tasks Ix.@+ updatedAssignment.tasks
          allStatuses = Map.fromList
            [ (sid, taskCompletionStatuses doc sid relevantTasks)
            | sid <- Set.toList updatedAssignment.studentIds
            ]
       in EvaluatorModel
            { assignment = updatedAssignment
            , tasks = doc.tasks
            , taskGroups = doc.taskGroups
            , users = doc.users
            , competences = doc.competences
            , competenceGrids = doc.competenceGrids
            , solutions = doc.solutions
            , taskObservations = m.taskObservations
            , aggregatedResults = m.aggregatedResults
            , selectedStudents = m.selectedStudents
            , selectedSocialForm = m.selectedSocialForm
            , excludedTasks = m.excludedTasks
            , evaluationDate = m.evaluationDate
            , expandedTaskContent = m.expandedTaskContent
            , collapsedResults = m.collapsedResults
            , assignmentEvidences = asmtEvidences
            , editingEvidence = m.editingEvidence
            , aggregationStale = m.aggregationStale
            , taskStatuses = allStatuses
            }

    update (SetTaskObservationForAll taskId compId ability) = M.modify $ \m ->
      let current = Map.lookup (taskId, compId) m.taskObservations
       in m{ taskObservations =
               if current == Just ability
                 then Map.delete (taskId, compId) m.taskObservations  -- Toggle off
                 else Map.insert (taskId, compId) ability m.taskObservations
           , aggregationStale = not (Map.null m.aggregatedResults)
           }

    update (ToggleStudentSelection userId) = M.modify $ \m ->
      let newSelected =
            if Set.member userId m.selectedStudents
              then Set.delete userId m.selectedStudents
              else Set.insert userId m.selectedStudents
          newSocialForm = if Set.size newSelected == 1 then Individual else Group
       in m{ selectedStudents = newSelected
           , selectedSocialForm = newSocialForm
           , editingEvidence = Nothing
           , taskObservations = Map.empty
           , aggregatedResults = Map.empty
           , aggregationStale = False
           }

    update (SetSocialForm sf) = M.modify $ \m ->
      m{selectedSocialForm = sf}

    update ComputeAggregation = M.modify $ \m ->
      let aggregated = computeAggregation m
       in m{aggregatedResults = aggregated, aggregationStale = False}

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      let current = Map.lookup compId m.aggregatedResults
       in m{aggregatedResults =
              if current == Just ability
                then Map.delete compId m.aggregatedResults  -- Toggle off
                else Map.insert compId ability m.aggregatedResults}

    update CreateEvidences = do
      m <- M.get
      M.io_ $ do
        -- Create one Evidence per selected student (may produce multiple commands each)
        evidenceCommands <- mapM (createEvidenceForStudent m) (Set.toList m.selectedStudents)
        -- Send all commands in order (Lock must precede Release)
        mapM_ (modifySyncDocument r) (concat evidenceCommands)
      -- Reset all evaluation state after creating evidences
      M.modify $ \m' -> m'
        { taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , selectedStudents = Set.empty
        , editingEvidence = Nothing
        , aggregationStale = False
        }

    update (ToggleTaskIncluded taskId) = M.modify $ \m ->
      m{ excludedTasks =
           if Set.member taskId m.excludedTasks
             then Set.delete taskId m.excludedTasks  -- Re-include
             else Set.insert taskId m.excludedTasks  -- Exclude
       , aggregationStale = not (Map.null m.aggregatedResults)
       }

    update (SetEvaluationDate dateStr) = M.modify $ \m ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr) of
        Just day -> m{evaluationDate = day}
        Nothing -> m  -- Keep old date if parsing fails

    update (ToggleTaskContentExpanded taskId) = M.modify $ \m ->
      m{expandedTaskContent =
          if Set.member taskId m.expandedTaskContent
            then Set.delete taskId m.expandedTaskContent
            else Set.insert taskId m.expandedTaskContent}

    update (ToggleSolutionExpanded solId) = M.modify $ \m ->
      -- Results type is expanded by default, so we track which are collapsed
      -- For other types, they are collapsed by default
      -- We toggle the collapsedResults set: if in set, remove (expand); if not, add (collapse)
      m{collapsedResults =
          if Set.member solId m.collapsedResults
            then Set.delete solId m.collapsedResults
            else Set.insert solId m.collapsedResults}

    update (LoadStudentEvidence userId) = M.modify $ \m ->
      case Map.lookup userId (evidencesForDate m.evaluationDate m.assignmentEvidences) of
        Nothing -> m
        Just ev ->
          let -- Convert stored per-task evaluations to evaluator's flat map format
              -- ev.tasks :: Map TaskId (Map CompetenceLevelId Ability)
              loadedObs = Map.fromList
                [ ((tid, clid), ab)
                | (tid, evals) <- Map.toList ev.tasks
                , (clid, ab) <- Map.toList evals
                ]
              -- Convert observations to aggregated results
              loadedAgg = Map.fromList
                [ (obs.competenceLevelId, obs.ability)
                | obs <- Ix.toList ev.observations
                ]
           in m{ taskObservations = loadedObs
               , aggregatedResults = loadedAgg
               , editingEvidence = Just userId
               , selectedSocialForm = case Ix.toList ev.observations of
                   (obs : _) -> obs.socialForm
                   [] -> m.selectedSocialForm
               , evaluationDate = ev.date
               , aggregationStale = False
               }

    update ResetLoadedEvidence = M.modify $ \m ->
      m{ editingEvidence = Nothing
       , taskObservations = Map.empty
       , aggregatedResults = Map.empty
       , aggregationStale = False
       }

    -- Compute aggregated results from task observations (pure function)
    -- Takes the worst (maximum) ability per competence across all tasks
    computeAggregation m =
      Map.foldrWithKey groupByCompetence Map.empty m.taskObservations
      where
        groupByCompetence (_, compId) ability acc =
          Map.insertWith max compId ability acc

    -- Create or modify evidence for a single student from aggregated results.
    -- If the student already has an evidence for this assignment, use Lock+Modify;
    -- otherwise create a new one.
    createEvidenceForStudent :: EvaluatorModel -> UserId -> IO [Command]
    createEvidenceForStudent m userId = do
      let sf = m.selectedSocialForm
          asmt = m.assignment
          -- Build tasks map: for each included task, collect its per-competence evaluations
          tasksMap :: Map.Map TaskId TaskEvaluations
          tasksMap = Map.fromList
            [ (tid, taskEvals)
            | tid <- asmt.tasks
            , not (Set.member tid m.excludedTasks)
            , let taskEvals = Map.fromList
                    [ (cid, ab)
                    | ((tid', cid), ab) <- Map.toList m.taskObservations
                    , tid' == tid
                    ]
            ]
      observations <- mapM (mkObservation sf) (Map.toList m.aggregatedResults)
      case Map.lookup userId (evidencesForDate m.evaluationDate m.assignmentEvidences) of
        Just existingEv -> do
          -- Lock then modify existing evidence
          let lockCmd = Evidences (OnEvidences (Modify existingEv.id Lock))
              patch = EvidencePatch
                { userId = Nothing
                , activityType = Just (existingEv.activityType, asmt.activityType)
                , date = Just (existingEv.date, m.evaluationDate)
                , tasks = Just (existingEv.tasks, tasksMap)
                , oldTasks = Nothing
                , observations = Just (existingEv.observations, Ix.fromList observations)
                , assignmentId = Nothing
                , lessonId = Nothing
                }
              releaseCmd = Evidences (OnEvidences (Modify existingEv.id (Release patch)))
          pure [lockCmd, releaseCmd]
        Nothing -> do
          -- Create new evidence
          evidenceId <- nextId @IO @Evidence r
          let evidence = Evidence
                { id = evidenceId
                , userId = Just userId
                , activityType = asmt.activityType
                , date = m.evaluationDate
                , tasks = tasksMap
                , oldTasks = ""
                , observations = Ix.fromList observations
                , assignmentId = Just asmt.id
                , lessonId = Nothing
                }
          pure [Evidences (OnEvidences (Create evidence))]
      where
        mkObservation sf (compId, ability) = do
          obsId <- nextId @IO @Observation r
          pure
            Observation
              { id = obsId
              , competenceLevelId = compId
              , socialForm = sf
              , ability = ability
              }

    view' m =
      if null m.assignment.tasks
        then Typography.paragraph (C.translate' C.LblAssignmentNoTasks)
        else
          let -- Sort tasks by identifier for consistent display order
              sortedTaskIds = map (.id) $
                Ix.toAscList (Proxy @TaskIdentifier) $ m.tasks Ix.@+ m.assignment.tasks
           in M.div_
                []
                [ Typography.h2 (C.translate' C.LblEvaluateAssignment)
                , viewStudentSelection m
                , viewOverwriteBanner m
                , M.div_ [class_ "space-y-6"] (map (viewTaskSection m) sortedTaskIds)
                , viewAggregationSection m
                , viewCreateEvidencesButton m
                ]

    viewStudentSelection m =
      let students = Ix.toAscList (Proxy @T.Text) $ m.users Ix.@+ Set.toList m.assignment.studentIds
          selectedCount = Set.size m.selectedStudents
          dateValue = ms $ formatTime defaultTimeLocale "%Y-%m-%d" m.evaluationDate
       in M.div_
            [class_ "mb-6 p-4 bg-muted/50 rounded border border-border"]
            [ M.div_ [class_ "mb-3"] [Typography.h3 $ C.translate' C.LblStudents <> " (" <> C.translate' (C.LblNSelected selectedCount) <> ")"]
            , M.div_ [class_ "flex flex-wrap gap-2 mb-4"] (map (viewStudentButton m) students)
            , M.div_ [class_ "flex items-center gap-4 mt-3 pt-3 border-t"]
                [ M.div_ [class_ "flex items-center gap-2"]
                    [ M.span_ [class_ "font-semibold text-sm"] [M.text $ C.translate' C.LblPhaseSocialForm <> ":"]
                    , M.div_ [class_ "flex gap-2"] (map (viewSocialFormButton m) socialForms)
                    ]
                , M.div_ [class_ "flex items-center gap-2"]
                    [ M.span_ [class_ "font-semibold text-sm"] [M.text $ C.translate' C.LblEvidenceDate <> ":"]
                    , Input.dateInput dateValue SetEvaluationDate
                    ]
                ]
            ]

    viewSocialFormButton m sf =
      let isSelected = m.selectedSocialForm == sf
          buttonClass = if isSelected
                          then "px-3 py-1 rounded bg-primary text-primary-foreground text-sm cursor-pointer hover:bg-primary/90"
                          else "px-3 py-1 rounded bg-secondary text-secondary-foreground text-sm cursor-pointer hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (SetSocialForm sf)]
            [M.text $ C.translate' $ C.LblSocialForm sf]

    viewStudentButton m student =
      let isSelected = Set.member student.id m.selectedStudents
          hasEvidence = Map.member student.id (evidencesForDate m.evaluationDate m.assignmentEvidences)
          buttonClass = if isSelected
                          then "px-3 py-1 rounded bg-primary text-primary-foreground text-sm cursor-pointer hover:bg-primary/90"
                          else "px-3 py-1 rounded bg-secondary text-secondary-foreground text-sm cursor-pointer hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (ToggleStudentSelection student.id)]
            [ M.text $ ms student.name
            , if hasEvidence
                then M.span_ [class_ "ml-1 text-xs opacity-75"] [M.text "\x2713"]
                else M.text ""
            ]

    viewOverwriteBanner m =
      let dateEvMap = evidencesForDate m.evaluationDate m.assignmentEvidences
          studentsWithEvidence =
            [ uid
            | uid <- Set.toList m.selectedStudents
            , Map.member uid dateEvMap
            ]
          lookupName uid = case Ix.getOne (m.users Ix.@= uid) of
            Just u -> u.name
            Nothing -> T.pack (show uid)
          studentNames = T.intercalate ", " (map lookupName studentsWithEvidence)
       in if null studentsWithEvidence
            then M.text ""
            else case m.editingEvidence of
              -- State B: evidence loaded — show single text with basis info + reset button
              Just loadedUid ->
                let loadedName = lookupName loadedUid
                 in M.div_ [class_ "my-4 p-4 bg-yellow-50 border border-yellow-200 rounded-lg"]
                      [ M.div_ [class_ "flex items-center justify-between"]
                          [ M.p_ [class_ "text-sm text-yellow-800 font-medium"]
                              [ M.text $
                                  C.translate' C.LblEvidencesBasedOn
                                  <> ms loadedName
                                  <> C.translate' C.LblWillBeEdited
                                  <> ms studentNames
                              ]
                          , M.button_
                              [ class_ "ml-3 text-sm px-3 py-1 bg-secondary text-secondary-foreground rounded hover:bg-secondary/80 shrink-0"
                              , M.onClick ResetLoadedEvidence
                              ]
                              [M.text $ C.translate' C.LblReset]
                          ]
                      ]
              -- State A: no evidence loaded — show per-student items with load buttons
              Nothing ->
                M.div_ [class_ "my-4 p-4 bg-yellow-50 border border-yellow-200 rounded-lg"]
                  [ M.p_ [class_ "text-sm text-yellow-800 font-medium mb-2"]
                      [M.text $ C.translate' C.LblEvidencesWillBeEdited]
                  , M.div_ [class_ "space-y-2"]
                      (map (viewOverwriteItem m) studentsWithEvidence)
                  ]

    viewOverwriteItem m userId =
      let userName = case Ix.getOne (m.users Ix.@= userId) of
            Just u -> u.name
            Nothing -> T.pack (show userId)
       in M.div_ [class_ "flex items-center justify-between"]
            [ M.span_ [class_ "text-sm text-yellow-800"] [M.text $ ms userName]
            , M.button_
                [ class_ "text-sm px-3 py-1 bg-primary text-primary-foreground rounded hover:bg-primary/90"
                , M.onClick (LoadStudentEvidence userId)
                ]
                [M.text $ C.translate' C.LblLoadEvidence]
            ]

    viewCompactStudentStatus m taskId userId =
      let status = fromMaybe TaskNotEvaluated $ do
            userStatuses <- Map.lookup userId m.taskStatuses
            Map.lookup taskId userStatuses
          studentName = case Ix.getOne (m.users Ix.@= userId) of
            Just u -> u.name
            Nothing -> T.pack (show userId)
       in M.div_ [class_ "group relative", MP.title_ (ms studentName)]
            [viewCompactTaskStatus status]

    viewTaskSection m taskId =
      let isExcluded = Set.member taskId m.excludedTasks
       in M.div_
            [class_ "border-b pb-4"]
            [ viewTaskHeader m taskId isExcluded
            , if isExcluded
                then M.text ""  -- Collapsed when excluded
                else M.div_ []
                       [ viewTaskContent m taskId
                       , viewTaskSolutions m taskId
                       , viewStudentEvaluations m taskId
                       ]
            ]

    viewTaskHeader m taskId isExcluded =
      let taskM = Ix.getOne (m.tasks Ix.@= taskId)
       in case taskM of
            Nothing -> M.div_ [] [M.text $ C.translate' C.LblTaskNotFound <> ": " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
                  toggleClass = if isExcluded
                    then "px-2 py-1 rounded text-sm cursor-pointer border border-muted-foreground text-muted-foreground hover:bg-muted/50"
                    else "px-2 py-1 rounded text-sm cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90"
                  selectedList = Set.toList m.selectedStudents
                  statusDots =
                    if null selectedList
                      then M.text ""
                      else M.div_ [class_ "flex gap-0.5 items-center"]
                             (map (viewCompactStudentStatus m taskId) selectedList)
               in M.div_ [class_ "mt-4 mb-1 flex items-center justify-between"]
                    [ M.div_ [class_ "flex items-center gap-3"]
                        [ Typography.h3 $ C.translate' C.LblTaskPrefix <> ms identifier
                        , statusDots
                        ]
                    , M.button_
                        [class_ toggleClass, M.onClick (ToggleTaskIncluded taskId)]
                        [M.text $ C.translate' $ if isExcluded then C.LblIncludeTask else C.LblExcludeTask]
                    ]

    viewTaskContent m taskId =
      let taskM = Ix.getOne (m.tasks Ix.@= taskId)
          isContentExpanded = Set.member taskId m.expandedTaskContent
       in case taskM of
            Nothing -> M.text ""
            Just task ->
              let content = getTaskContent m.taskGroups task
               in case content of
                    Nothing -> M.text ""
                    Just c ->
                      if c == mempty
                        then M.text ""
                        else M.div_ [class_ "mb-2"]
                               [ -- Collapsible header
                                 M.div_
                                   [ class_ "flex items-center gap-2 cursor-pointer hover:bg-muted/50 px-2 py-1 rounded"
                                   , M.onClick (ToggleTaskContentExpanded taskId)
                                   ]
                                   [ Disclosure.disclosureChevron isContentExpanded
                                   , M.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translate' C.LblTaskStatement]
                                   ]
                               , -- Content (only when expanded)
                                 if isContentExpanded
                                   then M.div_ [class_ "ml-6 mb-2 prose prose-sm prose-stone max-w-none"]
                                          [renderRichText c]
                                   else M.text ""
                               ]

    viewTaskSolutions m taskId =
      let taskSolutions = Ix.toList $ m.solutions Ix.@= taskId
       in if null taskSolutions
            then M.text ""
            else M.div_ [class_ "space-y-2 mb-3"]
                   (map (viewSolutionItem m) taskSolutions)

    viewSolutionItem m solution =
      let -- Results type is expanded by default
          -- We track which Results are collapsed in collapsedResults
          -- For non-Results types, they are collapsed unless explicitly expanded (but we use same logic)
          isExpanded = case solution.solutionType of
            Results -> not $ Set.member solution.id m.collapsedResults
            _ -> Set.member solution.id m.collapsedResults  -- Non-results: collapsed by default, tracked as "expanded" in set
          solutionTypeLabel = C.translate' (C.LblSolutionType solution.solutionType)
       in Disclosure.collapsible isExpanded (ToggleSolutionExpanded solution.id)
            (M.span_ [class_ "text-sm font-medium"] [M.text solutionTypeLabel])
            (M.div_ [class_ "prose prose-sm prose-stone max-w-none"] [renderRichText solution.content])

    viewStudentEvaluations m taskId =
      let taskM = Ix.getOne (m.tasks Ix.@= taskId)
       in case taskM of
            Nothing -> M.div_ [] [M.text $ C.translate' C.LblTaskNotFound]
            Just task ->
              let competences = getTaskCompetences m task
               in if null m.selectedStudents
                    then M.div_ [class_ "mt-4"] [Typography.muted $ C.translate' C.LblPleaseSelectStudents]
                    else M.div_ [class_ "mt-4 space-y-2"] (map (viewCompetenceEvaluation m taskId) competences)

    getTaskCompetences m task =
      let attrs = getTaskAttributes m.taskGroups task
       in attrs.primary <> attrs.secondary

    viewCompetenceEvaluation m taskId compId =
      let currentAbility = Map.lookup (taskId, compId) m.taskObservations
          (competenceId, level) = compId
          competenceM = Ix.getOne (m.competences Ix.@= competenceId)
          compLevelName = case competenceM of
            Nothing -> C.translate' C.LblCompetence <> " " <> ms (T.pack (show compId))
            Just comp -> ms $ maybe (comp.description <> " - " <> T.pack (show level)) (.description) (comp.levels Map.!? level)
       in M.div_
            [M.class_ "flex items-center gap-2"]
            [ M.span_ [M.class_ "flex-1"] [M.text compLevelName]  -- Takes remaining space
            , M.div_ [M.class_ "flex gap-1 shrink-0"] (map (viewAbilityButton taskId compId currentAbility) abilities)  -- Right-aligned
            ]

    viewAbilityButton taskId compId currentAbility ability =
      let isSelected = currentAbility == Just ability
          buttonClass = if isSelected then "bg-primary text-primary-foreground px-2 py-1 text-sm rounded" else "bg-secondary text-secondary-foreground px-2 py-1 text-sm rounded hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (SetTaskObservationForAll taskId compId ability)]
            [M.text $ C.translate' $ C.LblAbility ability]

    viewAggregationSection m =
      M.div_
        [class_ "mt-6 border-t pt-6"]
        [ M.div_ [class_ "flex items-center justify-between mb-4"]
            [ Typography.h3 (C.translate' C.LblAggregatedResults)
            , M.div_ [class_ "flex items-center gap-3"]
                [ if m.aggregationStale
                    then M.span_ [class_ "text-sm text-yellow-700"]
                           [M.text $ C.translate' C.LblAggregationStale]
                    else M.text ""
                , M.button_
                    [ M.onClick ComputeAggregation
                    , class_ "bg-primary text-primary-foreground px-4 py-2 rounded hover:bg-primary/90"
                    ]
                    [M.text $ C.translate' C.LblComputeAggregation]
                ]
            ]
        , if Map.null m.aggregatedResults
            then Typography.muted (C.translate' C.LblComputeAggregationHint)
            else viewAggregatedResults m
        ]

    viewAggregatedResults m =
      let -- Get competence IDs from aggregated results
          compIds = Set.fromList [compId | (compId, _) <- Map.keys m.aggregatedResults]
          -- Get competences that have results, sorted by order
          competencesWithResults = Ix.toAscList (Proxy @Order) $ m.competences Ix.@+ Set.toList compIds
          -- Get unique grid IDs from these competences
          gridIds = Set.fromList $ map (.competenceGridId) competencesWithResults
          -- Sort grids by their order
          sortedGrids = Ix.toAscList (Proxy @Order) $ m.competenceGrids Ix.@+ Set.toList gridIds
       in M.div_
            [class_ "space-y-4"]
            (map (viewGridAggregation m) sortedGrids)

    viewGridAggregation m grid =
      let -- Get competences for this grid, sorted by order
          gridCompetences = Ix.toAscList (Proxy @Order) $ m.competences Ix.@= grid.id
          -- Build results in competence order - for each competence, include all its levels that have results
          resultsForGrid =
            [ (compLevelId, ability)
            | comp <- gridCompetences
            , (compLevelId@(compId, _), ability) <- Map.toList m.aggregatedResults
            , compId == comp.id
            ]
       in if null resultsForGrid
            then M.text ""
            else M.div_ [class_ "border border-border rounded bg-muted/50"]
                   [ -- Grid title header
                     M.div_ [class_ "px-3 py-2 border-b bg-muted font-medium"]
                       [M.text $ ms grid.title]
                   , -- Competence results
                     M.div_ [class_ "p-3 space-y-2"]
                       (map (viewAggregatedCompetence m) resultsForGrid)
                   ]

    viewAggregatedCompetence m (compId, ability) =
      let (competenceId, level) = compId
          competenceM = Ix.getOne (m.competences Ix.@= competenceId)
          compLevelName = case competenceM of
            Nothing -> C.translate' C.LblCompetence <> " " <> ms (T.pack (show compId))
            Just comp -> ms $ maybe (comp.description <> " - " <> T.pack (show level)) (.description) (comp.levels Map.!? level)
          contributingTasks = getContributingTasks m compId
       in M.div_
            [class_ "mb-3"]
            [ M.div_ [class_ "flex items-center gap-2"]
                [ M.span_ [class_ "flex-1"] [M.text compLevelName]  -- Takes remaining space
                , M.div_ [class_ "flex gap-1 shrink-0"] (map (viewAggregatedAbilityButton compId ability) abilities)  -- Right-aligned
                ]
            , if null contributingTasks
                then M.text ""
                else M.div_ [class_ "text-xs text-muted-foreground mt-1 ml-1"]
                       [M.text $ C.translate' C.LblContributingTasks <> ms (T.intercalate ", " contributingTasks)]
            ]

    getContributingTasks m compId =
      let taskIds = Map.keys $ Map.filterWithKey (\(_, cid) _ -> cid == compId) m.taskObservations
          taskIdentifiers = map (\tid -> case Ix.getOne (m.tasks Ix.@= tid) of
                                   Nothing -> T.pack (show tid)
                                   Just task -> let TaskIdentifier ident = task.identifier in ident
                                ) [tid | (tid, _) <- taskIds]
       in taskIdentifiers

    viewAggregatedAbilityButton compId currentAbility ability =
      let isSelected = currentAbility == ability
          buttonClass = if isSelected then "bg-primary text-primary-foreground px-2 py-1 text-sm rounded" else "bg-secondary text-secondary-foreground px-2 py-1 text-sm rounded hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (SetAggregatedResult compId ability)]
            [M.text $ C.translate' $ C.LblAbility ability]

    viewCreateEvidencesButton m =
      let selectedCount = Set.size m.selectedStudents
          hasAggregatedResults = not $ Map.null m.aggregatedResults
          isDisabled = selectedCount == 0 || not hasAggregatedResults || m.aggregationStale
          dateEvMap = evidencesForDate m.evaluationDate m.assignmentEvidences
          hasExisting = any (`Map.member` dateEvMap) (Set.toList m.selectedStudents)
          actionLabel = C.translate' $ if hasExisting then C.LblSaveEvidences else C.LblCreateEvidencesAction
          buttonText = actionLabel <> " (" <> C.translate' (C.LblStudentsSelected selectedCount) <> ")"
          attrs =
            [ M.onClick CreateEvidences
            , class_ $
                if isDisabled
                  then "bg-muted text-muted-foreground px-4 py-2 rounded cursor-not-allowed"
                  else "bg-ability-success text-primary-foreground px-4 py-2 rounded hover:bg-ability-success/90"
            ]
              <> [M.disabled_ | isDisabled]
       in M.div_
            [class_ "mt-6 flex justify-end"]
            [M.button_ attrs [M.text buttonText]]
