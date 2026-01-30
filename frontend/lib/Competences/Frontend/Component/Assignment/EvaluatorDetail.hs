module Competences.Frontend.Component.Assignment.EvaluatorDetail
  ( evaluatorDetailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Competence (..), CompetenceGrid (..), CompetenceGridIxs, Document (..), LevelInfo (..), Order, Solution (..), SolutionId, SolutionIxs, SolutionType (..), User (..))
import Competences.Document.Competence (CompetenceIxs, CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..), abilities, mkEvidence, socialForms)
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
import Competences.Frontend.View.TaskContent (renderRichText)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Miso.String (MisoString, ms)

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
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = maybe m.assignment id $ Ix.getOne (doc.assignments Ix.@= m.assignment.id)
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
            }

    update (SetTaskObservationForAll taskId compId ability) = M.modify $ \m ->
      let current = Map.lookup (taskId, compId) m.taskObservations
       in m{taskObservations =
              if current == Just ability
                then Map.delete (taskId, compId) m.taskObservations  -- Toggle off
                else Map.insert (taskId, compId) ability m.taskObservations}

    update (ToggleStudentSelection userId) = M.modify $ \m ->
      let newSelected =
            if Set.member userId m.selectedStudents
              then Set.delete userId m.selectedStudents
              else Set.insert userId m.selectedStudents
          newSocialForm = if Set.size newSelected == 1 then Individual else Group
       in m{selectedStudents = newSelected, selectedSocialForm = newSocialForm}

    update (SetSocialForm sf) = M.modify $ \m ->
      m{selectedSocialForm = sf}

    update ComputeAggregation = M.modify $ \m ->
      let aggregated = computeAggregation m
       in m{aggregatedResults = aggregated}

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      let current = Map.lookup compId m.aggregatedResults
       in m{aggregatedResults =
              if current == Just ability
                then Map.delete compId m.aggregatedResults  -- Toggle off
                else Map.insert compId ability m.aggregatedResults}

    update CreateEvidences = do
      m <- M.get
      M.io_ $ do
        -- Create one Evidence per selected student
        evidenceCommands <- mapM (createEvidenceForStudent m) (Set.toList m.selectedStudents)
        -- Send all commands
        mapM_ (modifySyncDocument r) evidenceCommands
      -- Reset all evaluation state after creating evidences
      M.modify $ \m' -> m'
        { taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , selectedStudents = Set.empty
        }

    update (ToggleTaskIncluded taskId) = M.modify $ \m ->
      m{excludedTasks =
          if Set.member taskId m.excludedTasks
            then Set.delete taskId m.excludedTasks  -- Re-include
            else Set.insert taskId m.excludedTasks}  -- Exclude

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

    -- Compute aggregated results from task observations (pure function)
    -- Takes the worst (maximum) ability per competence across all tasks
    computeAggregation m =
      Map.foldrWithKey groupByCompetence Map.empty m.taskObservations
      where
        groupByCompetence (_, compId) ability acc =
          Map.insertWith max compId ability acc

    -- Create Evidence for a single student from aggregated results
    createEvidenceForStudent :: EvaluatorModel -> UserId -> IO Command
    createEvidenceForStudent m userId = do
      evidenceId <- nextId @IO @Evidence r
      -- Use the aggregated results (same for all students)
      -- Generate observation IDs and create Observation records
      let sf = m.selectedSocialForm
          asmt = m.assignment
          -- Filter out excluded tasks from the evidence
          includedTasks = filter (`Set.notMember` m.excludedTasks) asmt.tasks
      observations <- mapM (mkObservation sf) (Map.toList m.aggregatedResults)
      let evidence =
            (mkEvidence evidenceId m.evaluationDate)  -- Use evaluationDate instead of assignmentDate
              { userId = Just userId
              , activityType = asmt.activityType
              , tasks = includedTasks
              , observations = Ix.fromList observations
              , assignmentId = Just asmt.id
              , oldTasks = ""
              }
      pure $ Evidences (OnEvidences (Create evidence))
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
        then Typography.paragraph "Dieser Auftrag hat keine Aufgaben"
        else
          let -- Sort tasks by identifier for consistent display order
              sortedTaskIds = map (.id) $
                Ix.toAscList (Proxy @TaskIdentifier) $ m.tasks Ix.@+ m.assignment.tasks
           in M.div_
                []
                [ Typography.h2 "Auftrag auswerten"
                , viewStudentSelection m
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
            [ M.div_ [class_ "mb-3"] [Typography.h3 $ C.translate' C.LblStudents <> " (" <> ms (show selectedCount) <> " ausgewählt)"]
            , M.div_ [class_ "flex flex-wrap gap-2 mb-4"] (map (viewStudentButton m) students)
            , M.div_ [class_ "flex items-center gap-4 mt-3 pt-3 border-t"]
                [ M.div_ [class_ "flex items-center gap-2"]
                    [ M.span_ [class_ "font-semibold text-sm"] [M.text "Sozialform:"]
                    , M.div_ [class_ "flex gap-2"] (map (viewSocialFormButton m) socialForms)
                    ]
                , M.div_ [class_ "flex items-center gap-2"]
                    [ M.span_ [class_ "font-semibold text-sm"] [M.text "Datum:"]
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
          buttonClass = if isSelected
                          then "px-3 py-1 rounded bg-primary text-primary-foreground text-sm cursor-pointer hover:bg-primary/90"
                          else "px-3 py-1 rounded bg-secondary text-secondary-foreground text-sm cursor-pointer hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (ToggleStudentSelection student.id)]
            [M.text $ ms student.name]

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
            Nothing -> M.div_ [] [M.text $ "Aufgabe nicht gefunden: " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
                  toggleClass = if isExcluded
                    then "px-2 py-1 rounded text-sm cursor-pointer border border-muted-foreground text-muted-foreground hover:bg-muted/50"
                    else "px-2 py-1 rounded text-sm cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90"
               in M.div_ [class_ "mt-4 mb-1 flex items-center justify-between"]
                    [ Typography.h3 $ "Aufgabe: " <> ms identifier
                    , M.button_
                        [class_ toggleClass, M.onClick (ToggleTaskIncluded taskId)]
                        [M.text $ if isExcluded then "Einbeziehen" else "Ausschließen"]
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
                      if T.null c
                        then M.text ""
                        else M.div_ [class_ "mb-2"]
                               [ -- Collapsible header
                                 M.div_
                                   [ class_ "flex items-center gap-2 cursor-pointer hover:bg-muted/50 px-2 py-1 rounded"
                                   , M.onClick (ToggleTaskContentExpanded taskId)
                                   ]
                                   [ Disclosure.disclosureChevron isContentExpanded
                                   , M.span_ [class_ "text-sm text-muted-foreground"] [M.text "Aufgabenstellung"]
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
            Nothing -> M.div_ [] [M.text "Aufgabe nicht gefunden"]
            Just task ->
              let competences = getTaskCompetences m task
               in if null m.selectedStudents
                    then M.div_ [class_ "mt-4"] [Typography.muted "Bitte wählen Sie Schüler zur Auswertung aus"]
                    else M.div_ [class_ "mt-4 space-y-2"] (map (viewCompetenceEvaluation m taskId) competences)

    getTaskCompetences m task =
      let attrs = getTaskAttributes m.taskGroups task
       in attrs.primary <> attrs.secondary

    viewCompetenceEvaluation m taskId compId =
      let currentAbility = Map.lookup (taskId, compId) m.taskObservations
          (competenceId, level) = compId
          competenceM = Ix.getOne (m.competences Ix.@= competenceId)
          compLevelName = case competenceM of
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
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
            [ Typography.h3 "Aggregierte Ergebnisse"
            , M.button_
                [ M.onClick ComputeAggregation
                , class_ "bg-primary text-primary-foreground px-4 py-2 rounded hover:bg-primary/90"
                ]
                [M.text "Aggregation berechnen"]
            ]
        , if Map.null m.aggregatedResults
            then Typography.muted "Klicken Sie auf 'Aggregation berechnen', um die Ergebnisse zu aggregieren."
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
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
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
                       [M.text $ "Aufgaben: " <> ms (T.intercalate ", " contributingTasks)]
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
          buttonText = "Nachweise erstellen (" <> ms (show selectedCount) <> " Schüler ausgewählt)"
          attrs =
            [ M.onClick CreateEvidences
            , class_ $
                if selectedCount == 0 || not hasAggregatedResults
                  then "bg-muted text-muted-foreground px-4 py-2 rounded cursor-not-allowed"
                  else "bg-ability-success text-primary-foreground px-4 py-2 rounded hover:bg-ability-success/90"
            ]
              <> [M.disabled_ | selectedCount == 0 || not hasAggregatedResults]
       in M.div_
            [class_ "mt-6 flex justify-end"]
            [M.button_ attrs [M.text buttonText]]
