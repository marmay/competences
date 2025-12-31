module Competences.Frontend.Component.AssignmentEvaluator
  ( assignmentEvaluatorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Competence (..)
  , Document (..)
  , User (..)
  , emptyDocument
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), abilities)
import Competences.Document.Task (Task (..), TaskAttributes (..), TaskId, TaskIdentifier (..), TaskType (..))
import Competences.Document.User (UserId)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
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
-- Tracks per-task observations and aggregated results
data Model = Model
  { assignment :: !(Maybe Assignment)
  , currentDocument :: !Document
  -- Map from (TaskId, UserId, CompetenceLevelId) to Ability
  , taskObservations :: !(Map.Map (TaskId, UserId, CompetenceLevelId) Ability)
  -- Current task being evaluated (index into assignment.tasks)
  , currentTaskIndex :: !Int
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | SetTaskObservation !TaskId !UserId !CompetenceLevelId !Ability
  | NextTask
  | PreviousTask
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
        , currentTaskIndex = 0
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let newDoc = dc.document
       in m{currentDocument = newDoc}

    update (SetTaskObservation taskId userId compId ability) = M.modify $ \m ->
      m{taskObservations = Map.insert (taskId, userId, compId) ability m.taskObservations}

    update NextTask = M.modify $ \m ->
      case m.assignment of
        Nothing -> m
        Just a -> m{currentTaskIndex = min (length a.tasks - 1) (m.currentTaskIndex + 1)}

    update PreviousTask = M.modify $ \m ->
      m{currentTaskIndex = max 0 (m.currentTaskIndex - 1)}

    update CreateEvidences = pure ()  -- TODO: Implement evidence creation

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
            else viewTaskEvaluation m a

    viewTaskEvaluation m a =
      let currentTask = if m.currentTaskIndex < length a.tasks then Just (a.tasks !! m.currentTaskIndex) else Nothing
       in case currentTask of
            Nothing -> M.div_ [] [M.text "Keine Aufgabe ausgewählt"]
            Just taskId ->
              M.div_
                []
                [ M.h2_ [] [M.text "Auftrag auswerten"]
                , M.p_ [] [M.text $ "Aufgabe " <> ms (show (m.currentTaskIndex + 1)) <> " von " <> ms (show (length a.tasks))]
                , viewTaskInfo m.currentDocument taskId
                , viewStudentEvaluations m a taskId
                , viewNavigation m a
                ]

    viewTaskInfo doc taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId doc.tasks)
       in case taskM of
            Nothing -> M.div_ [] [M.text $ "Aufgabe nicht gefunden: " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
               in M.div_
                    [M.class_ "border p-2 mb-4"]
                    [M.text $ "Aufgabe: " <> ms identifier]

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
      M.div_
        [M.class_ "border p-3 rounded"]
        [ M.h4_ [M.class_ "font-semibold mb-2"] [M.text $ ms student.name]
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

    viewNavigation _m _a =
      M.div_
        [M.class_ "flex gap-2 mt-4"]
        [ M.button_
            [M.onClick PreviousTask]
            [M.text "← Vorherige Aufgabe"]
        , M.button_
            [M.onClick NextTask]
            [M.text "Nächste Aufgabe →"]
        , M.button_
            [M.onClick CreateEvidences, M.class_ "ml-auto"]
            [M.text "Nachweise erstellen"]
        ]
