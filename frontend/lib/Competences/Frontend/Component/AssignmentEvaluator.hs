module Competences.Frontend.Component.AssignmentEvaluator
  ( assignmentEvaluatorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), EvidencesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Competence (..)
  , Document (..)
  , User (..)
  , emptyDocument
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..), abilities, mkEvidence)
import Competences.Document.Task (Task (..), TaskAttributes (..), TaskId, TaskIdentifier (..), TaskType (..))
import Competences.Document.User (UserId)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Miso.String (ms)

-- | Model for assignment evaluation
-- Tracks per-task observations, aggregated results, and selected students
data Model = Model
  { assignment :: !(Maybe Assignment)
  , currentDocument :: !Document
  -- Map from (TaskId, UserId, CompetenceLevelId) to Ability
  , taskObservations :: !(Map.Map (TaskId, UserId, CompetenceLevelId) Ability)
  -- Aggregated results (worst ability per competence) - editable before Evidence creation
  , aggregatedResults :: !(Map.Map (UserId, CompetenceLevelId) Ability)
  -- Students selected for Evidence creation
  , selectedStudents :: !(Set.Set UserId)
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | SetTaskObservation !TaskId !UserId !CompetenceLevelId !Ability
  | ToggleStudentSelection !UserId
  | ComputeAggregation -- Compute aggregated results from task observations
  | SetAggregatedResult !UserId !CompetenceLevelId !Ability -- Edit aggregated result
  | CreateEvidences
  deriving (Eq, Show)

assignmentEvaluatorComponent :: SyncDocumentRef -> M.Component p Model Action
assignmentEvaluatorComponent r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      Model
        { assignment = Nothing
        , currentDocument = emptyDocument
        , taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , selectedStudents = Set.empty
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let newDoc = dc.document
       in m{currentDocument = newDoc}

    update (SetTaskObservation taskId userId compId ability) = M.modify $ \m ->
      m{taskObservations = Map.insert (taskId, userId, compId) ability m.taskObservations}

    update (ToggleStudentSelection userId) = M.modify $ \m ->
      let newSelected =
            if Set.member userId m.selectedStudents
              then Set.delete userId m.selectedStudents
              else Set.insert userId m.selectedStudents
       in m{selectedStudents = newSelected}

    update ComputeAggregation = M.modify $ \m ->
      case m.assignment of
        Nothing -> m
        Just assignment ->
          let aggregated = computeAggregation m assignment
           in m{aggregatedResults = aggregated}

    update (SetAggregatedResult userId compId ability) = M.modify $ \m ->
      m{aggregatedResults = Map.insert (userId, compId) ability m.aggregatedResults}

    update CreateEvidences = do
      m <- M.get
      case m.assignment of
        Nothing -> pure ()
        Just assignment -> do
          M.io_ $ do
            -- Create one Evidence per selected student
            evidenceCommands <- mapM (createEvidenceForStudent m assignment) (Set.toList m.selectedStudents)
            -- Send all commands
            mapM_ (modifySyncDocument r) evidenceCommands
          -- Clear selections after creating evidences
          M.modify $ \m' -> m'{selectedStudents = Set.empty}

    -- Compute aggregated results from task observations (pure function)
    computeAggregation m assignment =
      let allStudents = Set.toList assignment.studentIds
          studentResults = concatMap (computeStudentAggregation m assignment) allStudents
       in Map.fromList studentResults

    computeStudentAggregation m assignment userId =
      let -- Get all observations for this student across all tasks in the assignment
          studentObs = Map.filterWithKey (\(taskId, uid, _) _ -> uid == userId && taskId `elem` assignment.tasks) m.taskObservations
          -- Group by CompetenceLevelId and take worst (maximum) ability
          grouped = Map.foldrWithKey groupByCompetence Map.empty studentObs
       in map (\(compId, ability) -> ((userId, compId), ability)) (Map.toList grouped)
      where
        groupByCompetence (_, _, compId) ability acc =
          Map.insertWith max compId ability acc

    -- Create Evidence for a single student from aggregated results
    createEvidenceForStudent m assignment userId = do
      evidenceId <- nextId @M.JSM @Evidence r
      -- Get aggregated observations for this student
      let studentAggregated = Map.filterWithKey (\(uid, _) _ -> uid == userId) m.aggregatedResults
      -- Generate observation IDs and create Observation records
      observations <- mapM mkObservation (Map.toList studentAggregated)
      let evidence =
            (mkEvidence evidenceId assignment.assignmentDate)
              { userIds = Set.singleton userId
              , activityType = assignment.activityType
              , tasks = assignment.tasks
              , observations = Ix.fromList observations
              , assignmentId = Just assignment.id
              , oldTasks = ""
              }
      pure $ Evidences (OnEvidences (Create evidence))
      where
        mkObservation ((_, compId), ability) = do
          obsId <- nextId @M.JSM @Observation r
          pure
            Observation
              { id = obsId
              , competenceLevelId = compId
              , socialForm = Individual -- Default to Individual for assignment evaluations
              , ability = ability
              }

    view m =
      V.sideMenu
        (V.componentA "evaluator-assignment-selection" [] (assignmentSelectorComponent r #assignment))
        (viewEvaluator m)

    viewEvaluator m =
      case m.assignment of
        Nothing -> M.div_ [] [M.text "Bitte wählen Sie einen Auftrag zur Auswertung"]
        Just a ->
          if null a.tasks
            then M.div_ [] [M.text "Dieser Auftrag hat keine Aufgaben"]
            else
              M.div_
                []
                [ M.h2_ [] [M.text "Auftrag auswerten"]
                , M.div_ [M.class_ "space-y-6"] (map (viewTaskSection m a) a.tasks)
                , viewAggregationSection m a
                , viewCreateEvidencesButton m
                ]

    viewTaskSection m a taskId =
      M.div_
        [M.class_ "border-b pb-4"]
        [ viewTaskInfo m.currentDocument taskId
        , viewStudentEvaluations m a taskId
        ]

    viewTaskInfo doc taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId doc.tasks)
       in case taskM of
            Nothing -> M.div_ [] [M.text $ "Aufgabe nicht gefunden: " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
               in M.h3_ [M.class_ "font-bold text-lg mt-4 mb-2"] [M.text $ "Aufgabe: " <> ms identifier]

    viewStudentEvaluations m a taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId m.currentDocument.tasks)
          students = map (\userId -> Ix.getOne (Ix.getEQ userId m.currentDocument.users)) (Set.toList a.studentIds)
       in case taskM of
            Nothing -> M.div_ [] [M.text "Aufgabe nicht gefunden"]
            Just task ->
              let competences = getTaskCompetences task
               in M.div_
                    [M.class_ "mt-4"]
                    [ M.h3_ [M.class_ "font-bold mb-2"] [M.text "Schüler auswerten"]
                    , M.div_ [M.class_ "space-y-4"] (map (\studentM -> viewStudentEvaluation m taskId competences studentM) students)
                    ]

    getTaskCompetences task =
      case task.taskType of
        SelfContained attrs -> attrs.primary <> attrs.secondary
        SubTask _ _ -> []

    viewStudentEvaluation _m _taskId _competences Nothing =
      M.div_ [M.class_ "border p-3 rounded"] [M.text "Schüler nicht gefunden"]
    viewStudentEvaluation m taskId competences (Just student) =
      let isSelected = Set.member student.id m.selectedStudents
       in M.div_
            [M.class_ "border p-3 rounded"]
            [ M.div_
                [M.class_ "flex items-center gap-2 mb-2"]
                [ M.input_
                    [ M.type_ "checkbox"
                    , M.checked_ isSelected
                    , M.onClick (ToggleStudentSelection student.id)
                    , M.class_ "w-4 h-4"
                    ]
                , M.h4_ [M.class_ "font-semibold"] [M.text $ ms student.name]
                ]
            , M.div_ [M.class_ "space-y-2"] (map (viewCompetenceEvaluation m taskId student.id) competences)
            ]

    viewCompetenceEvaluation m taskId userId compId =
      let currentAbility = Map.lookup (taskId, userId, compId) m.taskObservations
          (competenceId, level) = compId
          competenceM = Ix.getOne (Ix.getEQ competenceId m.currentDocument.competences)
          compLevelName = case competenceM of
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
            Just comp -> ms $ fromMaybe (comp.description <> " - " <> T.pack (show level)) (comp.levelDescriptions Map.!? level)
       in M.div_
            [M.class_ "flex items-center gap-2"]
            [ M.span_ [M.class_ "min-w-[200px]"] [M.text compLevelName]
            , M.div_ [M.class_ "flex gap-1"] (map (viewAbilityButton taskId userId compId currentAbility) abilities)
            ]

    viewAbilityButton taskId userId compId currentAbility ability =
      let isSelected = currentAbility == Just ability
          buttonClass = if isSelected then "bg-blue-500 text-white px-2 py-1 text-sm rounded" else "bg-gray-200 px-2 py-1 text-sm rounded hover:bg-gray-300"
       in M.button_
            [M.class_ buttonClass, M.onClick (SetTaskObservation taskId userId compId ability)]
            [M.text $ ms $ show ability]

    viewAggregationSection m a =
      M.div_
        [M.class_ "mt-6 border-t pt-6"]
        [ M.div_ [M.class_ "flex items-center justify-between mb-4"]
            [ M.h3_ [M.class_ "font-bold text-lg"] [M.text "Aggregierte Ergebnisse"]
            , M.button_
                [ M.onClick ComputeAggregation
                , M.class_ "bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
                ]
                [M.text "Aggregation berechnen"]
            ]
        , if Map.null m.aggregatedResults
            then M.p_ [M.class_ "text-sm text-gray-500"] [M.text "Klicken Sie auf 'Aggregation berechnen', um die Ergebnisse zu aggregieren."]
            else viewAggregatedResults m a
        ]

    viewAggregatedResults m a =
      let students = map (\userId -> Ix.getOne (Ix.getEQ userId m.currentDocument.users)) (Set.toList a.studentIds)
       in M.div_ [M.class_ "space-y-4"] (map (viewStudentAggregatedResults m) students)

    viewStudentAggregatedResults _ Nothing =
      M.div_ [M.class_ "border p-3 rounded bg-gray-50"] [M.text "Schüler nicht gefunden"]
    viewStudentAggregatedResults m (Just student) =
      let studentResults = Map.filterWithKey (\(uid, _) _ -> uid == student.id) m.aggregatedResults
       in if Map.null studentResults
            then M.div_ [] []
            else
              M.div_
                [M.class_ "border p-3 rounded bg-gray-50"]
                [ M.h4_ [M.class_ "font-semibold mb-2"] [M.text $ ms student.name]
                , M.div_ [M.class_ "space-y-2"] (map (viewAggregatedCompetence m student.id) (Map.toList studentResults))
                ]

    viewAggregatedCompetence m userId ((_, compId), ability) =
      let (competenceId, level) = compId
          competenceM = Ix.getOne (Ix.getEQ competenceId m.currentDocument.competences)
          compLevelName = case competenceM of
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
            Just comp -> ms $ fromMaybe (comp.description <> " - " <> T.pack (show level)) (comp.levelDescriptions Map.!? level)
       in M.div_
            [M.class_ "flex items-center gap-2"]
            [ M.span_ [M.class_ "min-w-[200px]"] [M.text compLevelName]
            , M.div_ [M.class_ "flex gap-1"] (map (viewAggregatedAbilityButton userId compId ability) abilities)
            ]

    viewAggregatedAbilityButton userId compId currentAbility ability =
      let isSelected = currentAbility == ability
          buttonClass = if isSelected then "bg-blue-500 text-white px-2 py-1 text-sm rounded" else "bg-gray-200 px-2 py-1 text-sm rounded hover:bg-gray-300"
       in M.button_
            [M.class_ buttonClass, M.onClick (SetAggregatedResult userId compId ability)]
            [M.text $ ms $ show ability]

    viewCreateEvidencesButton m =
      let selectedCount = Set.size m.selectedStudents
          hasAggregatedResults = not $ Map.null m.aggregatedResults
          buttonText = "Nachweise erstellen (" <> ms (show selectedCount) <> " Schüler ausgewählt)"
          attrs =
            [ M.onClick CreateEvidences
            , M.class_ $
                if selectedCount == 0 || not hasAggregatedResults
                  then "bg-gray-400 text-white px-4 py-2 rounded cursor-not-allowed"
                  else "bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600"
            ]
              <> [M.disabled_ | selectedCount == 0 || not hasAggregatedResults]
       in M.div_
            [M.class_ "mt-6 flex justify-end"]
            [M.button_ attrs [M.text buttonText]]
