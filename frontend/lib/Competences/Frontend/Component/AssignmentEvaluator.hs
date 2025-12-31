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
-- Tracks per-task observations and selected students for Evidence creation
data Model = Model
  { assignment :: !(Maybe Assignment)
  , currentDocument :: !Document
  -- Map from (TaskId, UserId, CompetenceLevelId) to Ability
  , taskObservations :: !(Map.Map (TaskId, UserId, CompetenceLevelId) Ability)
  -- Students selected for Evidence creation
  , selectedStudents :: !(Set.Set UserId)
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | SetTaskObservation !TaskId !UserId !CompetenceLevelId !Ability
  | ToggleStudentSelection !UserId
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
            else
              M.div_
                []
                [ M.h2_ [] [M.text "Auftrag auswerten"]
                , M.div_ [M.class_ "space-y-6"] (map (viewTaskSection m a) a.tasks)
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

    viewCreateEvidencesButton m =
      let selectedCount = Set.size m.selectedStudents
          buttonText = "Nachweise erstellen (" <> ms (show selectedCount) <> " Schüler ausgewählt)"
          attrs =
            [ M.onClick CreateEvidences
            , M.class_ $
                if selectedCount == 0
                  then "bg-gray-400 text-white px-4 py-2 rounded cursor-not-allowed"
                  else "bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600"
            ]
              <> [M.disabled_ | selectedCount == 0]
       in M.div_
            [M.class_ "mt-6 flex justify-end"]
            [M.button_ attrs [M.text buttonText]]
