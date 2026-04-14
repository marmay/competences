-- | Shared task list rendering for parent components.
--
-- Provides 'taskListView' and 'renderSolutionList' — polymorphic
-- rendering functions that parent components (assignment viewer,
-- resource modal, etc.) use to render task lists with disclosures,
-- solutions, status tinting, and teacher actions.
module Competences.Frontend.Component.Task.ListView
  ( taskListView
  , renderSolutionList
  )
where

import Competences.Document (Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.Component.Task.EditButton (solutionEditButton)
import Competences.Frontend.Component.TaskResource (TaskWithSolutions (..))
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.SyncDocument (SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Task qualified as V
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus)
import Data.Set (Set)
import Data.Set qualified as Set
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- | Render a list of tasks with disclosures, solutions, and status tinting.
taskListView
  :: SyncContext
  -> V.TaskViewState
  -> (TaskId -> Maybe TaskCompletionStatus)
  -> (TaskWithSolutions -> [M.View m a])
  -> (TaskId -> [M.View m a])
  -> (V.TaskViewAction -> a)
  -> [TaskWithSolutions]
  -> M.View m a
taskListView _ _ _ _ _ _ [] =
  Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
taskListView r state statusLookup mkAnnotations mkExtraBody liftAction tasks =
  Layout.vFlow Layout.gapM (map renderOne tasks)
  where
    isTeacher = (syncDocumentEnv r).connectedUser.role == Teacher

    renderOne tws =
      let tid = tws.task.id
          name = ms (taskDisplayName tws.task)
          expanded = Set.member tid state.expandedTasks
          contentPresent = case tws.taskContent of
            Nothing -> False
            Just c -> c /= mempty
          solsPresent = not (null tws.solutions)
          extra = mkExtraBody tid

          parts = concat
            [ [ V.taskContentView (renderRichTextWithFiles r.formulaCache r tws.task.attachments rc)
              | contentPresent
              , Just rc <- [tws.taskContent]
              ]
            , [ renderSolutions' tid tws.solutions | solsPresent ]
            , extra
            ]

          mBody = if null parts then Nothing else Just (MH.div_ [class_ "space-y-3"] parts)
       in V.taskItemView (statusLookup tid) (liftAction (V.ToggleTask tid)) name (mkAnnotations tws) expanded mBody

    renderSolutions' tid =
      renderSolutionList r state.expandedSolutions isTeacher
        (liftAction . V.ToggleSolution)
        (liftAction . V.DeleteSolution)
        (liftAction (V.AddSolution tid))

-- | Render a list of solutions with collapsible disclosures and optional teacher actions.
renderSolutionList
  :: SyncContext
  -> Set SolutionId
  -> Bool
  -> (SolutionId -> a)
  -> (SolutionId -> a)
  -> a
  -> [Solution]
  -> M.View m a
renderSolutionList r expandedSet isTeacher mkToggle mkDelete addAction sols =
  MH.div_ [class_ "space-y-1"]
    ( map (renderOneSol r expandedSet isTeacher mkToggle mkDelete) sols
        <> [addSolButton | isTeacher]
    )
  where
    addSolButton =
      MH.div_ [class_ "flex justify-end"]
        [Button.ghostSm (Button.ButtonConfig (Button.IconText Icon.IcnAdd (C.translate' C.LblAddSolution)) (Just addAction))]

renderOneSol
  :: SyncContext -> Set SolutionId -> Bool
  -> (SolutionId -> a) -> (SolutionId -> a)
  -> Solution -> M.View m a
renderOneSol r expandedSet isTeacher mkToggle mkDelete sol =
  let isExpanded = Set.member sol.id expandedSet
      rendered =
        if sol.content == mempty
          then Typography.muted (C.translate' C.LblNoContent)
          else V.taskContentView (renderRichText r.formulaCache sol.content)
      actions
        | isTeacher =
            [ Disclosure.viewAction (solutionEditButton r sol)
            , Disclosure.destructiveAction Icon.IcnDelete (mkDelete sol.id)
            ]
        | otherwise = []
   in V.solutionView (V.solutionTypeLabel sol.solutionType) isExpanded rendered actions (mkToggle sol.id)
