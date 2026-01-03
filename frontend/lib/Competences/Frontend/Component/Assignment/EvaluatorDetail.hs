module Competences.Frontend.Component.Assignment.EvaluatorDetail
  ( evaluatorDetailView
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
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..), abilities, mkEvidence, socialForms)
import Competences.Document.Task (Task (..), TaskAttributes (..), TaskId, TaskIdentifier (..), TaskType (..))
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Miso.String (ms)
import Optics.Core ((&), (.~), (^.))

-- | Detail view for evaluating an assignment
-- The mode type parameter allows this to work with any mode type
evaluatorDetailView
  :: SyncDocumentRef
  -> Assignment
  -> M.View (SD.Model Assignment mode) (SD.Action mode)
evaluatorDetailView r assignment =
  V.component
    ("assignment-evaluator-" <> M.ms (show assignment.id))
    (evaluatorComponent r assignment)

-- | Internal model for the evaluator component
-- Tracks per-task observations, aggregated results, and selected students
data EvaluatorModel = EvaluatorModel
  { currentDocument :: !Document
  -- Map from (TaskId, CompetenceLevelId) to Ability - applies to all selected students
  , taskObservations :: !(Map.Map (TaskId, CompetenceLevelId) Ability)
  -- Aggregated results (worst ability per competence) - editable before Evidence creation
  , aggregatedResults :: !(Map.Map CompetenceLevelId Ability)
  -- Students selected for Evidence creation
  , selectedStudents :: !(Set.Set UserId)
  -- Social form for the evaluation (Individual or Group)
  , socialForm :: !SocialForm
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
  deriving (Eq, Show)

-- | The evaluator component with its own state management
evaluatorComponent :: SyncDocumentRef -> Assignment -> M.Component p EvaluatorModel EvaluatorAction
evaluatorComponent r assignment =
  (M.component model update view')
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      EvaluatorModel
        { currentDocument = emptyDocument
        , taskObservations = Map.empty
        , aggregatedResults = Map.empty
        , selectedStudents = Set.empty
        , socialForm = Individual
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let newDoc = dc.document
       in m{currentDocument = newDoc}

    update (SetTaskObservationForAll taskId compId ability) = M.modify $ \m ->
      m{taskObservations = Map.insert (taskId, compId) ability m.taskObservations}

    update (ToggleStudentSelection userId) = M.modify $ \m ->
      let newSelected =
            if Set.member userId (m ^. #selectedStudents)
              then Set.delete userId (m ^. #selectedStudents)
              else Set.insert userId (m ^. #selectedStudents)
          newSocialForm = if Set.size newSelected == 1 then Individual else Group
       in m & #selectedStudents .~ newSelected & #socialForm .~ newSocialForm

    update (SetSocialForm sf) = M.modify $ \m ->
      m & #socialForm .~ sf

    update ComputeAggregation = M.modify $ \m ->
      let aggregated = computeAggregation m
       in m{aggregatedResults = aggregated}

    update (SetAggregatedResult compId ability) = M.modify $ \m ->
      m{aggregatedResults = Map.insert compId ability m.aggregatedResults}

    update CreateEvidences = do
      m <- M.get
      M.io_ $ do
        -- Create one Evidence per selected student
        evidenceCommands <- mapM (createEvidenceForStudent m) (Set.toList m.selectedStudents)
        -- Send all commands
        mapM_ (modifySyncDocument r) evidenceCommands
      -- Clear selections after creating evidences
      M.modify $ \m' -> m'{selectedStudents = Set.empty}

    -- Compute aggregated results from task observations (pure function)
    -- Takes the worst (maximum) ability per competence across all tasks
    computeAggregation m =
      Map.foldrWithKey groupByCompetence Map.empty m.taskObservations
      where
        groupByCompetence (_, compId) ability acc =
          Map.insertWith max compId ability acc

    -- Create Evidence for a single student from aggregated results
    createEvidenceForStudent m userId = do
      evidenceId <- nextId @IO @Evidence r
      -- Use the aggregated results (same for all students)
      -- Generate observation IDs and create Observation records
      observations <- mapM mkObservation (Map.toList (m ^. #aggregatedResults))
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
        mkObservation (compId, ability) = do
          obsId <- nextId @IO @Observation r
          pure
            Observation
              { id = obsId
              , competenceLevelId = compId
              , socialForm = m ^. #socialForm
              , ability = ability
              }

    view' m =
      if null assignment.tasks
        then Typography.paragraph "Dieser Auftrag hat keine Aufgaben"
        else
          M.div_
            []
            [ Typography.h2 "Auftrag auswerten"
            , viewStudentSelection m
            , M.div_ [class_ "space-y-6"] (map (viewTaskSection m) assignment.tasks)
            , viewAggregationSection m
            , viewCreateEvidencesButton m
            ]

    viewStudentSelection m =
      let students = map (\userId -> Ix.getOne (Ix.getEQ userId m.currentDocument.users)) (Set.toList assignment.studentIds)
          selectedCount = Set.size m.selectedStudents
       in M.div_
            [class_ "mb-6 p-4 bg-muted/50 rounded border border-border"]
            [ M.div_ [class_ "mb-3"] [Typography.h3 $ C.translate' C.LblStudents <> " (" <> ms (show selectedCount) <> " ausgewählt)"]
            , M.div_ [class_ "flex flex-wrap gap-2 mb-4"] (map (viewStudentSelectionButton m) students)
            , M.div_ [class_ "flex items-center gap-2 mt-3 pt-3 border-t"]
                [ M.span_ [class_ "font-semibold text-sm"] [M.text "Sozialform:"]
                , M.div_ [class_ "flex gap-2"] (map (viewSocialFormButton m) socialForms)
                ]
            ]

    viewSocialFormButton m sf =
      let isSelected = m.socialForm == sf
          buttonClass = if isSelected
                          then "px-3 py-1 rounded bg-primary text-primary-foreground text-sm cursor-pointer hover:bg-primary/90"
                          else "px-3 py-1 rounded bg-secondary text-secondary-foreground text-sm cursor-pointer hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (SetSocialForm sf)]
            [M.text $ C.translate' $ C.LblSocialForm sf]

    viewStudentSelectionButton _ Nothing =
      M.div_ [class_ "px-3 py-1 rounded bg-muted text-muted-foreground text-sm"] [M.text "Schüler nicht gefunden"]
    viewStudentSelectionButton m (Just student) =
      let isSelected = Set.member student.id m.selectedStudents
          buttonClass = if isSelected
                          then "px-3 py-1 rounded bg-primary text-primary-foreground text-sm cursor-pointer hover:bg-primary/90"
                          else "px-3 py-1 rounded bg-secondary text-secondary-foreground text-sm cursor-pointer hover:bg-secondary/80"
       in M.button_
            [class_ buttonClass, M.onClick (ToggleStudentSelection student.id)]
            [M.text $ ms student.name]

    viewTaskSection m taskId =
      M.div_
        [class_ "border-b pb-4"]
        [ viewTaskInfo m.currentDocument taskId
        , viewStudentEvaluations m taskId
        ]

    viewTaskInfo doc taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId doc.tasks)
       in case taskM of
            Nothing -> M.div_ [] [M.text $ "Aufgabe nicht gefunden: " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
               in M.div_ [class_ "mt-4 mb-3"]
                    [ M.div_ [class_ "mb-1"] [Typography.h3 $ "Aufgabe: " <> ms identifier]
                    , case task.content of
                        Nothing -> M.text ""
                        Just content -> M.div_ [class_ "mb-2"] [Typography.small $ ms content]
                    ]

    viewStudentEvaluations m taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId m.currentDocument.tasks)
       in case taskM of
            Nothing -> M.div_ [] [M.text "Aufgabe nicht gefunden"]
            Just task ->
              let competences = getTaskCompetences task
               in if null m.selectedStudents
                    then M.div_ [class_ "mt-4"] [Typography.muted "Bitte wählen Sie Schüler zur Auswertung aus"]
                    else M.div_ [class_ "mt-4 space-y-2"] (map (viewCompetenceEvaluation m taskId) competences)

    getTaskCompetences task =
      case task.taskType of
        SelfContained attrs -> attrs.primary <> attrs.secondary
        SubTask _ _ -> []

    viewCompetenceEvaluation m taskId compId =
      let currentAbility = Map.lookup (taskId, compId) m.taskObservations
          (competenceId, level) = compId
          competenceM = Ix.getOne (Ix.getEQ competenceId m.currentDocument.competences)
          compLevelName = case competenceM of
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
            Just comp -> ms $ fromMaybe (comp.description <> " - " <> T.pack (show level)) (comp.levelDescriptions Map.!? level)
       in M.div_
            [M.class_ "flex items-center gap-2"]
            [ M.span_ [M.class_ "min-w-[200px]"] [M.text compLevelName]
            , M.div_ [M.class_ "flex gap-1"] (map (viewAbilityButton taskId compId currentAbility) abilities)
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
      M.div_
        [class_ "border border-border p-3 rounded bg-muted/50"]
        [ M.div_ [class_ "space-y-2"] (map (viewAggregatedCompetence m) (Map.toList m.aggregatedResults))
        ]

    viewAggregatedCompetence m (compId, ability) =
      let (competenceId, level) = compId
          competenceM = Ix.getOne (Ix.getEQ competenceId m.currentDocument.competences)
          compLevelName = case competenceM of
            Nothing -> ms $ "Kompetenz " <> T.pack (show compId)
            Just comp -> ms $ fromMaybe (comp.description <> " - " <> T.pack (show level)) (comp.levelDescriptions Map.!? level)
          contributingTasks = getContributingTasks m compId
       in M.div_
            [class_ "mb-3"]
            [ M.div_ [class_ "flex items-center gap-2"]
                [ M.span_ [class_ "min-w-[200px]"] [M.text compLevelName]
                , M.div_ [class_ "flex gap-1"] (map (viewAggregatedAbilityButton compId ability) abilities)
                ]
            , if null contributingTasks
                then M.text ""
                else M.div_ [class_ "text-xs text-muted-foreground mt-1 ml-1"]
                       [M.text $ "Aufgaben: " <> ms (T.intercalate ", " contributingTasks)]
            ]

    getContributingTasks m compId =
      let taskIds = Map.keys $ Map.filterWithKey (\(_, cid) _ -> cid == compId) m.taskObservations
          taskIdentifiers = map (\tid -> case Ix.getOne (Ix.getEQ tid m.currentDocument.tasks) of
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
