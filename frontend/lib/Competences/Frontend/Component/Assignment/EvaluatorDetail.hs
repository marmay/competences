module Competences.Frontend.Component.Assignment.EvaluatorDetail
  ( evaluatorDetailView
  , evaluatorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Command.Evidences (EvidencePatch (..))
import Competences.Document (Assignment (..), Document (..), Solution (..), SolutionId, SolutionIxs, SolutionType (..), User (..))
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionIxs, SubmissionKind (..), ownerIds)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..), TaskEvaluations, TaskRemark (..), taskRemarks, socialForms)
import Competences.Document.Task (Task (..), TaskId, TaskIdentifier (..), taskDisplayName)
import Competences.Document.User (UserId, UserIxs)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Evaluation qualified as Eval
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Color.Completion (CompletionStatus (..))
import Competences.Frontend.View.StatusIcon (completionIcon)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (isDigitalSubmission, isVoidSubmission, isNonDigitalSubmission)
import Competences.Query.TaskStatus (TaskCompletionStatus (..), taskCompletionStatuses)
import Competences.Frontend.Component.SubmissionPreview qualified as SubPreview
import Competences.Frontend.View.SubmissionViewer qualified as SubViewer
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Optics.Core qualified as O
import Optics.Core ((&), (.~))
import Competences.Frontend.View.Badge qualified as Badge
import qualified Competences.Frontend.View.Button as Button
import Competences.Frontend.Component.Selector.Common (selectorTransformedLens)
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..), searchSelectComponent)

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
  inlineComponent
    ("assignment-evaluator-" <> M.ms (show assignment.id))
    (evaluatorComponent r assignment)

-- | Internal model for the evaluator component
-- Tracks per-task observations, aggregated results, and selected students
-- Only stores the document subsets actually needed (not the full Document)
data EvaluatorModel = EvaluatorModel
  { assignment :: !Assignment
  , users :: !(Ix.IxSet UserIxs User)
  , solutions :: !(Ix.IxSet SolutionIxs Solution)
  -- Pre-computed view data (refreshed on each UpdateDocument)
  , taskViewData :: !(Map.Map TaskId Eval.TaskViewData)
  , competenceLevelInfos :: !(Map.Map CompetenceLevelId Eval.CompetenceLevelInfo)
  -- Map from (TaskId, CompetenceLevelId) to Ability - applies to all selected students
  , taskObservations :: !(Map.Map (TaskId, CompetenceLevelId) Ability)
  -- Aggregated results (worst ability per competence) - editable before Evidence creation
  , aggregatedResults :: !(Map.Map CompetenceLevelId Ability)
  -- The student whose submissions panel is shown
  , clickedStudent :: !(Maybe UserId)
  -- Student selection state (additive free-select or subtractive from submission)
  , selection :: !SelectionState
  -- Whether to show submission panel (auto/on/off)
  , submissionMode :: !SubmissionMode
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
  -- Per-task qualitative remarks (e.g. sloppy, exceptional)
  , taskRemarks :: !(Map.Map TaskId (Set.Set TaskRemark))
  -- Extra tasks added by teacher (not in assignment.tasks)
  , additionalTasks :: !(Set.Set TaskId)
  -- Counter to re-key the inline extra-task selector (incremented on reset)
  , selectorGeneration :: !Int
  -- Session preference: start with all tasks excluded when selecting students
  , startFromEmpty :: !Bool
  -- All submissions for this assignment (any student), indexed by user
  , submissions :: !(Ix.IxSet SubmissionIxs Submission)
  }
  deriving (Eq, Generic, Show)

-- | Whether to show the submission panel for students
data SubmissionMode = AutoSubmissions | SubmissionsOn | SubmissionsOff
  deriving (Eq, Show)

-- | Student selection state
data SelectionState
  = -- | Free multi-select: toggle students in/out
    AdditiveSelection !(Set.Set UserId)
  | -- | Constrained by submission: can only deselect from base set
    SubtractiveSelection
      { baseStudents :: !(Set.Set UserId) -- owners from the submission
      , deselected :: !(Set.Set UserId) -- manually removed from base
      , submissionId :: !SubmissionId -- the active submission
      }
  deriving (Eq, Show)

data EvaluatorAction
  = UpdateDocument !DocumentChange
  | SetTaskObservationForAll !TaskId !CompetenceLevelId !Ability
  | ToggleStudentSelection !UserId
  | SetSocialForm !SocialForm
  | SetSubmissionMode !SubmissionMode
  | ComputeAggregation -- Compute aggregated results from task observations
  | SetAggregatedResult !CompetenceLevelId !Ability -- Edit aggregated result
  | CreateEvidences
  | ToggleTaskIncluded !TaskId -- Toggle whether a task is included in evaluation
  | SetEvaluationDate !MisoString -- Set the date for evidence creation (YYYY-MM-DD format)
  | ToggleTaskContentExpanded !TaskId -- Toggle expand/collapse for task content
  | ToggleSolutionExpanded !SolutionId -- Toggle expand/collapse for a solution
  | LoadStudentEvidence !UserId -- Load existing evidence data into evaluator
  | ResetLoadedEvidence -- Clear loaded evidence, reset to fresh evaluation
  | ToggleTaskRemark !TaskId !TaskRemark -- Toggle a per-task remark
  | ToggleStartFromEmpty -- Toggle "start from empty" session preference
  | DismissSubmissions -- Close the submission panel
  deriving (Eq, Show)

-- | Derive the effective set of selected students.
activeStudents :: EvaluatorModel -> Set.Set UserId
activeStudents m = case m.selection of
  AdditiveSelection students -> students
  SubtractiveSelection base desel _ -> base `Set.difference` desel

-- | Whether the submission panel should be shown for the clicked student.
submissionsActive :: EvaluatorModel -> Bool
submissionsActive m = case m.submissionMode of
  AutoSubmissions -> case m.clickedStudent of
    Nothing -> False
    Just uid -> not (Ix.null (m.submissions Ix.@= uid))
  SubmissionsOn -> True
  SubmissionsOff -> False

-- | Lens for the submission selector binding.
-- Getter: extracts SubmissionId from SubtractiveSelection.
-- Setter: transitions between Additive/Subtractive based on selection.
activeSubmissionLens :: O.Lens' EvaluatorModel (Maybe SubmissionId)
activeSubmissionLens = O.lens getter setter
  where
    getter m = case m.selection of
      SubtractiveSelection _ _ sid -> Just sid
      _ -> Nothing
    setter m Nothing =
      m {selection = AdditiveSelection (activeStudents m)}
    setter m (Just sid) =
      let base = case Ix.getOne (m.submissions Ix.@= sid) of
            Just sub -> Set.fromList (ownerIds sub.ownership)
            Nothing -> Set.empty
       in m {selection = SubtractiveSelection base Set.empty sid}

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
        , users = Ix.empty
        , solutions = Ix.empty
        , taskViewData = Map.empty
        , competenceLevelInfos = Map.empty
        , taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , clickedStudent = Nothing
        , selection = AdditiveSelection Set.empty
        , submissionMode = AutoSubmissions
        , selectedSocialForm = Individual
        , excludedTasks = Set.empty
        , evaluationDate = assignment.assignmentDate
        , expandedTaskContent = Set.empty
        , collapsedResults = Set.empty
        , assignmentEvidences = []
        , editingEvidence = Nothing
        , aggregationStale = False
        , taskStatuses = Map.empty
        , taskRemarks = Map.empty
        , additionalTasks = Set.empty
        , selectorGeneration = 0
        , startFromEmpty = False
        , submissions = Ix.empty
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = fromMaybe m.assignment $ Ix.getOne (doc.assignments Ix.@= m.assignment.id)
          -- All evidences for this assignment (any date)
          asmtEvidences = Ix.toList $ doc.evidences Ix.@= m.assignment.id :: [Evidence]
          -- Pre-compute per-student task statuses for all assignment students
          relevantTasks = Ix.toList $ doc.tasks Ix.@+ updatedAssignment.tasks
          allStatuses = Map.fromList
            [ (sid, taskCompletionStatuses doc sid relevantTasks)
            | sid <- Set.toList updatedAssignment.studentIds
            ]
       in m
            { assignment = updatedAssignment
            , users = doc.users
            , solutions = doc.solutions
            , taskViewData = Eval.projectTasks doc.tasks
            , competenceLevelInfos = Eval.projectCompetenceLevels doc.competences doc.competenceGrids
            , assignmentEvidences = asmtEvidences
            , taskStatuses = allStatuses
            , submissions = doc.submissions Ix.@= m.assignment.id
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
      case m.selection of
        SubtractiveSelection base desel sid ->
          -- Subtractive: can only toggle deselection of base members
          if Set.member userId base
            then
              let newDesel =
                    if Set.member userId desel
                      then Set.delete userId desel
                      else Set.insert userId desel
                  newActive = base `Set.difference` newDesel
               in m
                    { selection = SubtractiveSelection base newDesel sid
                    , selectedSocialForm = if Set.size newActive == 1 then Individual else Group
                    }
            else m
        AdditiveSelection students ->
          if submissionsActive m
            then
              -- Submission mode: switch to this student, show their submissions
              m
                { clickedStudent = Just userId
                , selection = AdditiveSelection (Set.singleton userId)
                , editingEvidence = Nothing
                , taskObservations = Map.empty
                , aggregatedResults = Map.empty
                , aggregationStale = False
                , taskRemarks = Map.empty
                , additionalTasks = Set.empty
                , selectorGeneration = m.selectorGeneration + 1
                , selectedSocialForm = Individual
                , excludedTasks =
                    if Set.null students && m.startFromEmpty
                      then Set.fromList m.assignment.tasks
                      else m.excludedTasks
                }
            else
              -- Free mode: toggle student in/out
              let isSelected = Set.member userId students
                  wasEmpty = Set.null students
                  newStudents =
                    if isSelected
                      then Set.delete userId students
                      else Set.insert userId students
                  newClicked =
                    if isSelected && m.clickedStudent == Just userId
                      then Nothing
                      else Just userId
               in m
                    { clickedStudent = newClicked
                    , selection = AdditiveSelection newStudents
                    , editingEvidence = if isSelected then m.editingEvidence else Nothing
                    , taskObservations = if wasEmpty then Map.empty else m.taskObservations
                    , aggregatedResults = if wasEmpty then Map.empty else m.aggregatedResults
                    , aggregationStale = if wasEmpty then False else m.aggregationStale
                    , taskRemarks = if wasEmpty then Map.empty else m.taskRemarks
                    , additionalTasks = if wasEmpty then Set.empty else m.additionalTasks
                    , selectorGeneration = if wasEmpty then m.selectorGeneration + 1 else m.selectorGeneration
                    , selectedSocialForm = if Set.size newStudents == 1 then Individual else Group
                    , excludedTasks =
                        if wasEmpty && m.startFromEmpty
                          then Set.fromList m.assignment.tasks
                          else m.excludedTasks
                    }

    update (SetSocialForm sf) = M.modify $ \m ->
      m{selectedSocialForm = sf}

    update (SetSubmissionMode mode) = M.modify $ \m ->
      m { submissionMode = mode
        , selection = AdditiveSelection (activeStudents m)
        }

    update ComputeAggregation = M.modify $ \m ->
      let activeTaskIds = Set.fromList m.assignment.tasks <> m.additionalTasks
          activeObs = Map.filterWithKey
            (\(tid, _) _ -> Set.member tid activeTaskIds && not (Set.member tid m.excludedTasks))
            m.taskObservations
          aggregated = Eval.computeAggregation activeObs
       in m{aggregatedResults = aggregated, aggregationStale = False}

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      let current = Map.lookup compId m.aggregatedResults
       in m{aggregatedResults =
              if current == Just ability
                then Map.delete compId m.aggregatedResults  -- Toggle off
                else Map.insert compId ability m.aggregatedResults}

    update CreateEvidences = do
      m <- M.get
      let students = activeStudents m
      M.io_ $ do
        -- Create one Evidence per active student (may produce multiple commands each)
        evidenceCommands <- mapM (createEvidenceForStudent m) (Set.toList students)
        -- Send all commands in order (Lock must precede Release)
        mapM_ (modifySyncDocument r) (concat evidenceCommands)
      -- Reset all evaluation state after creating evidences
      M.modify $ \m' -> m'
        { taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , clickedStudent = Nothing
        , selection = AdditiveSelection Set.empty
        , editingEvidence = Nothing
        , aggregationStale = False
        , taskRemarks = Map.empty
        , additionalTasks = Set.empty
        , selectorGeneration = m'.selectorGeneration + 1
        }

    update (ToggleTaskIncluded taskId) = M.modify $ \m ->
      if Set.member taskId m.additionalTasks
        then -- Extra task: remove entirely instead of just excluding
          m{ additionalTasks = Set.delete taskId m.additionalTasks
           , taskObservations = Map.filterWithKey (\(t, _) _ -> t /= taskId) m.taskObservations
           , aggregationStale = not (Map.null m.aggregatedResults)
           , selectorGeneration = m.selectorGeneration + 1
           }
        else -- Assignment task: toggle exclusion as before
          m{ excludedTasks =
               if Set.member taskId m.excludedTasks
                 then Set.delete taskId m.excludedTasks
                 else Set.insert taskId m.excludedTasks
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
              -- Compute extra tasks (in evidence but not in assignment)
              assignmentTaskSet = Set.fromList m.assignment.tasks
              evidenceTaskSet = Map.keysSet ev.tasks
              loadedExtras = evidenceTaskSet `Set.difference` assignmentTaskSet
           in m{ taskObservations = loadedObs
               , aggregatedResults = loadedAgg
               , editingEvidence = Just userId
               , selectedSocialForm = case Ix.toList ev.observations of
                   (obs : _) -> obs.socialForm
                   [] -> m.selectedSocialForm
               , evaluationDate = ev.date
               , aggregationStale = False
               , taskRemarks = ev.taskRemarks
               , additionalTasks = loadedExtras
               , selectorGeneration = m.selectorGeneration + 1
               }

    update ResetLoadedEvidence = M.modify $ \m ->
      m{ editingEvidence = Nothing
       , taskObservations = Map.empty
       , aggregatedResults = Map.empty
       , aggregationStale = False
       , taskRemarks = Map.empty
       , additionalTasks = Set.empty
       , selectorGeneration = m.selectorGeneration + 1
       }

    update (ToggleTaskRemark taskId remark) = M.modify $ \m ->
      let current = Map.findWithDefault Set.empty taskId m.taskRemarks
          updated = if Set.member remark current
                      then Set.delete remark current
                      else Set.insert remark current
          newRemarks = if Set.null updated
                         then Map.delete taskId m.taskRemarks
                         else Map.insert taskId updated m.taskRemarks
       in m & #taskRemarks .~ newRemarks

    update ToggleStartFromEmpty = M.modify $ \m ->
      m{startFromEmpty = not m.startFromEmpty}

    update DismissSubmissions = M.modify $ \m ->
      m{selection = AdditiveSelection (activeStudents m)}

    -- Create or modify evidence for a single student from aggregated results.
    -- If the student already has an evidence for this assignment, use Lock+Modify;
    -- otherwise create a new one.
    createEvidenceForStudent :: EvaluatorModel -> UserId -> IO [Command]
    createEvidenceForStudent m userId = do
      let sf = m.selectedSocialForm
          asmt = m.assignment
          -- Build tasks map: for each included task, collect its per-competence evaluations
          allTaskIds = Set.toList (Set.fromList asmt.tasks <> m.additionalTasks)
          tasksMap :: Map.Map TaskId TaskEvaluations
          tasksMap = Map.fromList
            [ (tid, taskEvals)
            | tid <- allTaskIds
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
                , taskRemarks = Just (existingEv.taskRemarks, m.taskRemarks)
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
                , taskRemarks = m.taskRemarks
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
      let allTaskIds = m.assignment.tasks
            <> [tid | tid <- Set.toList m.additionalTasks, tid `notElem` m.assignment.tasks]
          sortedTaskIds = filter (\tid -> Map.member tid m.taskViewData) allTaskIds
          -- Left panel: task evaluation content
          taskContent =
            Layout.vFlow
              Layout.gapL
              [ Layout.vFlow Layout.gapL (map (viewTaskSection m) sortedTaskIds)
              , viewExtraTaskSelector m
              , viewAggregationSection m
              , viewCreateEvidencesButton m
              ]
          -- Compact banner for non-digital submissions (empty list otherwise)
          compactBanner = case m.clickedStudent of
            Just uid | submissionsActive m ->
              let studentSubs = Ix.toList (m.submissions Ix.@= uid)
               in if any isDigitalSubmission studentSubs
                    then []
                    else [viewCompactSubmissionBanner studentSubs]
            _ -> []
          leftContent =
            Layout.vFlow
              mempty
              ( [ viewStudentSelection m
                , viewOverwriteBanner m
                ]
                ++ compactBanner
                ++ [taskContent]
              )
       in if null sortedTaskIds && Set.null m.additionalTasks
            then Typography.paragraph (C.translate' C.LblAssignmentNoTasks)
            else case m.clickedStudent of
              Just uid | submissionsActive m ->
                let studentSubs = Ix.toList (m.submissions Ix.@= uid)
                 in if any isDigitalSubmission studentSubs
                      then
                        -- Full split view for digital submissions
                        let key = "sub-sel-" <> ms (show uid)
                            binding = selectorTransformedLens id id activeSubmissionLens
                         in Layout.hFlow
                              (Layout.gapM <> Layout.hFull)
                              [ Layout.scrollContent $ Layout.addClass "w-1/2" leftContent
                              , Layout.addClass "w-1/2 flex-1 min-h-0 flex flex-col" $
                                  MH.div_ [class_ "h-full flex flex-col"]
                                    [ MH.div_ [class_ "flex justify-end"]
                                        [Button.ghostSm (Button.button Icon.IcnCancel DismissSubmissions)]
                                    , inlineComponent key
                                        (SubPreview.submissionSelectorComponent r m.assignment.id uid binding)
                                    ]
                              ]
                      else leftContent
              _ -> leftContent

    viewStudentSelection m =
      let students = Ix.toAscList (Proxy @T.Text) $ m.users Ix.@+ Set.toList m.assignment.studentIds
          active = activeStudents m
          selectedCount = Set.size active
          dateValue = ms $ formatTime defaultTimeLocale "%Y-%m-%d" m.evaluationDate
       in M.div_
            [class_ "mb-6 p-4 bg-muted/50 rounded border border-border"]
            [ M.div_ [class_ "mb-3"] [Typography.h3 $ C.translate' C.LblStudents <> " (" <> C.translate' (C.LblNSelected selectedCount) <> ")"]
            , MH.div_ [class_ "mb-4"]
                [ Layout.hFlow (Layout.gapS <> Layout.flexWrap)
                    (map (viewStudentButton m) students)
                ]
            , MH.div_ [class_ "mt-3 pt-3 border-t"]
                [ Layout.hFlow (Layout.gapM <> Layout.hFull <> Layout.crossCenter)
                    [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                        [ M.span_ [class_ "font-semibold text-sm"] [M.text $ C.translate' C.LblPhaseSocialForm <> ":"]
                        , Layout.hFlow Layout.gapS (map (viewSocialFormButton m) socialForms)
                        ]
                    , Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                        [ M.span_ [class_ "font-semibold text-sm"] [M.text $ C.translate' C.LblEvidenceDate <> ":"]
                        , Input.dateInput dateValue SetEvaluationDate
                        ]
                    , M.label_ [class_ "flex items-center gap-2 text-sm font-medium select-none cursor-pointer"]
                        [ M.input_ [MP.type_ "checkbox", MP.checked_ m.startFromEmpty, M.onClick ToggleStartFromEmpty]
                        , M.text (C.translate' C.LblOnlySelectedTasks)
                        ]
                    ]
                ]
            ]

    viewSocialFormButton m sf =
      Button.toggle (m.selectedSocialForm == sf) (Button.button (C.LblSocialForm sf) (SetSocialForm sf))

    viewStudentButton m student =
      let active = activeStudents m
          isActive = Set.member student.id active
          isClicked = m.clickedStudent == Just student.id
          -- In subtractive mode: disable students not in the submission's base set
          isDisabled = case m.selection of
            AdditiveSelection _ -> False
            SubtractiveSelection base _ _ ->
              not (Set.member student.id base) && not isClicked
          hasEvidenceOnDate = Map.member student.id (evidencesForDate m.evaluationDate m.assignmentEvidences)
          hasAnyEvidence = any (\ev -> ev.userId == Just student.id) m.assignmentEvidences
          studentSubs = Ix.toList (m.submissions Ix.@= student.id)
          hasOpenDigital = any (SubViewer.isSubmissionOpen student.id m.assignmentEvidences) studentSubs
          hasVoid = any isVoidSubmission studentSubs
          hasNonDigital = any isNonDigitalSubmission studentSubs
          contents
            | hasOpenDigital = Button.toButtonContents (Icon.IcnImport, ms student.name)
            | hasEvidenceOnDate = Button.toButtonContents (Icon.IcnApply, ms student.name)
            | hasVoid = Button.toButtonContents (Icon.IcnCancel, ms student.name)
            | hasNonDigital = Button.toButtonContents (Icon.IcnLessonNotes, ms student.name)
            | hasAnyEvidence = Button.toButtonContents (Icon.IcnEvidence, ms student.name)
            | otherwise = Button.toButtonContents (ms student.name)
      in Button.toggleSm isActive $ Button.button contents (not isDisabled, ToggleStudentSelection student.id)

    viewCompactSubmissionBanner :: [Submission] -> M.View EvaluatorModel EvaluatorAction
    viewCompactSubmissionBanner subs =
      M.div_ [class_ "my-4 space-y-2"] (map submissionBannerItem subs)
      where
        bannerClasses = "flex items-center gap-3 p-3 bg-stone-50 border border-stone-200 rounded-lg"

        submissionBannerItem sub = case sub.kind of
          VoidSubmission reason ->
            M.div_ [class_ bannerClasses]
              [ Icon.icon [] Icon.IcnCancel
              , Badge.destructive (Badge.badgeLabel C.LblNichtGemacht)
              , M.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translateVoidReason reason]
              , Layout.flowSpring
              , Button.ghostSm (Button.button Icon.IcnCancel DismissSubmissions)
              ]
          NonDigitalSubmission mLocation ->
            M.div_ [class_ bannerClasses]
              [ Icon.icon [] Icon.IcnLessonNotes
              , Badge.outline (Badge.badgeLabel C.LblGemacht)
              , case mLocation of
                  Just loc -> M.span_ [class_ "text-sm text-muted-foreground"] [M.text $ ms loc]
                  Nothing -> M.text ""
              , Layout.flowSpring
              , Button.ghostSm (Button.button Icon.IcnCancel DismissSubmissions)
              ]
          DigitalSubmission _ -> M.text ""  -- Shouldn't happen in this branch

    viewOverwriteBanner m =
      let dateEvMap = evidencesForDate m.evaluationDate m.assignmentEvidences
          active = activeStudents m
          studentsWithEvidence =
            [ uid
            | uid <- Set.toList active
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
                      [ Layout.hFlow (Layout.hFull <> Layout.crossCenter)
                          [ M.p_ [class_ "text-sm text-yellow-800 font-medium"]
                              [ M.text $
                                  C.translate' C.LblEvidencesBasedOn
                                  <> ms loadedName
                                  <> C.translate' C.LblWillBeEdited
                                  <> ms studentNames
                              ]
                          , Layout.flowSpring
                          , Button.secondarySm (Button.button C.LblReset ResetLoadedEvidence)
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
       in Layout.hFlow (Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "text-sm text-yellow-800"] [M.text $ ms userName]
            , Layout.flowSpring
            , Button.secondarySm (Button.button C.LblLoadEvidence (LoadStudentEvidence userId))
            ]

    viewCompactStudentStatus m taskId userId =
      let status = fromMaybe TaskNotEvaluated $ do
            userStatuses <- Map.lookup userId m.taskStatuses
            Map.lookup taskId userStatuses
          completionStatus = case status of
            TaskNotEvaluated -> Open
            TaskDone _ -> Done
            TaskNotDone _ -> InProgress
          studentName = case Ix.getOne (m.users Ix.@= userId) of
            Just u -> u.name
            Nothing -> T.pack (show userId)
       in M.div_ [class_ "group relative", MP.title_ (ms studentName)]
            [completionIcon completionStatus]

    viewTaskSection m taskId =
      let isExcluded = Set.member taskId m.excludedTasks
          isExtra = Set.member taskId m.additionalTasks
          selectedList = Set.toList (activeStudents m)
          extraBadge =
            if isExtra
              then [Badge.outline (Badge.badgeLabel C.LblExtraTask)]
              else []
          statusDots =
            if null selectedList
              then []
              else [Layout.hFlow (Layout.hFull <> Layout.crossCenter <> Layout.gapMicro) (map (viewCompactStudentStatus m taskId) selectedList)]
       in M.div_
            [class_ "border-b pb-4"]
            [ Eval.viewTaskHeader m.taskViewData taskId isExcluded (ToggleTaskIncluded taskId) (extraBadge <> statusDots)
            , if isExcluded
                then M.text ""
                else M.div_ []
                       [ Eval.viewTaskContent r.formulaCache m.taskViewData m.expandedTaskContent taskId ToggleTaskContentExpanded
                       , viewTaskSolutions m taskId
                       , viewTaskRemarkButtons m taskId
                       , viewStudentEvaluations m taskId
                       ]
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
          titleView = Disclosure.titleIconText Icon.IcnSolution (C.translate' (C.LblSolutionType solution.solutionType))
          bodyView = M.div_ [class_ "prose prose-sm prose-stone max-w-none"] [renderRichText r.formulaCache solution.content]
       in Disclosure.innerDisclosure (ToggleSolutionExpanded solution.id) $
            Disclosure.contents titleView isExpanded bodyView []

    viewTaskRemarkButtons m taskId =
      if Set.null (activeStudents m)
        then M.text ""
        else
          let currentRemarks = Map.findWithDefault Set.empty taskId m.taskRemarks
           in M.div_ [class_ "mt-2 mb-2"]
                [ Layout.hFlow (Layout.gapS <> Layout.crossCenter)
                    ( M.span_ [class_ "text-xs text-muted-foreground font-medium"]
                        [M.text $ C.translate' C.LblTaskRemarks <> ":"]
                    : map (viewRemarkButton currentRemarks taskId) taskRemarks
                    )
                ]

    viewRemarkButton currentRemarks taskId remark =
      Button.toggleSm (Set.member remark currentRemarks)
        (Button.button (C.LblTaskRemark remark) (ToggleTaskRemark taskId remark))

    viewStudentEvaluations m taskId =
      if Set.null (activeStudents m)
        then M.div_ [class_ "mt-4"] [Typography.muted $ C.translate' C.LblPleaseSelectStudents]
        else Eval.viewTaskCompetences m.taskViewData m.competenceLevelInfos m.taskObservations taskId SetTaskObservationForAll

    viewExtraTaskSelector m =
      let assignmentTaskSet = Set.fromList m.assignment.tasks
          extraTaskSearchConfig =
            SearchSelectConfig
              { projectItems = \doc ->
                  filter (\t -> not (Set.member t.id assignmentTaskSet))
                    (Ix.toAscList (Proxy @TaskIdentifier) doc.tasks)
              , itemId = (.id)
              , itemLabel = taskDisplayName
              , metaFilters = []
              , viewTag = \t -> (Icon.IcnTask, ms $ taskDisplayName t)
              , placeholder = M.fromMisoString $ C.translate' C.LblSelectTasks
              , selectionOrder = AutoOrder id
              , tagLayout = TagsInline
              , onCreate = Nothing
              }
          key = "extra-task-selector-" <> ms (show m.selectorGeneration)
       in M.div_ [class_ "border-t pt-3"]
            [ Typography.h4 (C.translate' C.LblAddTask)
            , inlineComponent key
                (searchSelectComponent r key extraTaskSearchConfig
                   (Set.toList m.additionalTasks)
                   (selectorTransformedLens (.id) Set.fromList #additionalTasks))
            ]

    viewAggregationSection m =
      M.div_
        [class_ "mt-6"]
        [ Eval.viewAggregationSection
            m.aggregationStale
            (not $ Map.null m.aggregatedResults)
            ComputeAggregation
            ( Eval.viewAggregatedResults m.competenceLevelInfos m.aggregatedResults
                (viewAggregatedCompetenceWithTasks m)
            )
        ]

    viewAggregatedCompetenceWithTasks m (compId, ability) =
      let contributingTasks = getContributingTasks m compId
       in M.div_
            []
            [ Eval.viewAggregatedCompetenceRow m.competenceLevelInfos SetAggregatedResult (compId, ability)
            , if null contributingTasks
                then M.text ""
                else M.div_ [class_ "text-xs text-muted-foreground mt-1 ml-1"]
                       [M.text $ C.translate' C.LblContributingTasks <> ms (T.intercalate ", " contributingTasks)]
            ]

    getContributingTasks m compId =
      let taskIds = Map.keys $ Map.filterWithKey (\(_, cid) _ -> cid == compId) m.taskObservations
       in [ maybe (T.pack (show tid)) (.identifier) (Map.lookup tid m.taskViewData)
          | (tid, _) <- taskIds
          ]

    viewCreateEvidencesButton m =
      let active = activeStudents m
          selectedCount = Set.size active
          isDisabled = selectedCount == 0 || m.aggregationStale
          dateEvMap = evidencesForDate m.evaluationDate m.assignmentEvidences
          hasExisting = any (`Map.member` dateEvMap) (Set.toList active)
          actionLabel = C.translate' $ if hasExisting then C.LblSaveEvidences else C.LblCreateEvidencesAction
          buttonText = actionLabel <> " (" <> C.translate' (C.LblStudentsSelected selectedCount) <> ")"
       in MH.div_ [class_ "mt-6"]
            [ Layout.hFlow (Layout.wFull <> Layout.mainEnd)
                [Button.primary $ Button.button buttonText (not isDisabled, CreateEvidences)]
            ]
