-- | Embedding helpers for the detailed task view.
--
-- Depends only on the pure view layer, so it is safe to import from any
-- entity's component module without creating cycles.
module Competences.Frontend.Component.Task.Detailed.Embed
  ( updateTaskDetailed
  , taskListView
  , renderSolutionList
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Document (Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Solution (mkSolution)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.Component.Task.EditButton (solutionEditButton)
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.SyncContext (SyncContext (..), modifySyncDocument, nextId)
import Competences.Frontend.SyncContext.SyncDocument (SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.Fragment.Task.Detailed qualified as V
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus)
import Data.Set qualified as Set
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core (Lens', (%), (%~))

-- | Embeddable update: pass a lens at the parent's 'TaskDetailedState'
-- and a lifter wrapping 'TaskDetailedAction' back into the parent action.
updateTaskDetailed
  :: Lens' model V.TaskDetailedState
  -> SyncContext
  -> (V.TaskDetailedAction -> action)
  -> V.TaskDetailedAction
  -> M.Effect parent model action
updateTaskDetailed stateLens r lift = go
  where
    go (V.AddSolution tid) = M.io_ $ do
      solId <- nextId r
      let uid = (syncDocumentEnv r).connectedUser.id
      modifySyncDocument r $ Solutions (OnSolutions (CreateAndLock (mkSolution solId tid uid)))
    go (V.HoldDeleteSolution ha) =
      HoldButton.handleHoldAction'
        (stateLens % #holdDeleteSolution)
        (\sid -> modifySyncDocument r $ Solutions (OnSolutions (Delete sid)))
        (lift . V.HoldDeleteSolution)
        ha
    go action = M.modify (stateLens %~ V.updateTaskDetailedPure action)

-- | Render a list of tasks with disclosures, solutions, and status tinting.
taskListView
  :: SyncContext
  -> V.TaskDetailedState
  -> (TaskId -> Maybe TaskCompletionStatus)
  -> (TaskWithSolutions -> [M.View m a])
  -> (TaskId -> [M.View m a])
  -> (V.TaskDetailedAction -> a)
  -> [TaskWithSolutions]
  -> M.View m a
taskListView _ _ _ _ _ _ [] =
  Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
taskListView r state statusLookup mkAnnotations mkExtraBody liftAction tasks =
  Layout.vFlow Layout.gapM (map renderOne tasks)
  where
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
            , [renderSolutionList r state liftAction tid tws.solutions | solsPresent]
            , extra
            ]

          mBody = if null parts then Nothing else Just (MH.div_ [class_ "space-y-3"] parts)
       in V.taskItemView (statusLookup tid) (liftAction (V.ToggleTask tid)) name (mkAnnotations tws) expanded mBody

-- | Render a list of solutions with collapsible disclosures and (for teachers) edit/delete/add actions.
renderSolutionList
  :: SyncContext
  -> V.TaskDetailedState
  -> (V.TaskDetailedAction -> a)
  -> TaskId
  -> [Solution]
  -> M.View m a
renderSolutionList r state liftAction tid sols =
  MH.div_ [class_ "space-y-1"]
    ( map (renderOneSol r state liftAction isTeacher) sols
        <> [addSolButton | isTeacher]
    )
  where
    isTeacher = (syncDocumentEnv r).connectedUser.role == Teacher
    addSolButton =
      MH.div_ [class_ "flex justify-end"]
        [Button.ghostSm (Button.ButtonConfig (Button.IconText Icon.IcnAdd (C.translate' C.LblAddSolution)) (Just (liftAction (V.AddSolution tid))))]

renderOneSol
  :: SyncContext
  -> V.TaskDetailedState
  -> (V.TaskDetailedAction -> a)
  -> Bool
  -> Solution
  -> M.View m a
renderOneSol r state liftAction isTeacher sol =
  let isExpanded = Set.member sol.id state.expandedSolutions
      rendered
        | sol.content == mempty = Typography.muted (C.translate' C.LblNoContent)
        | otherwise = V.taskContentView (renderRichText r.formulaCache sol.content)
      actions
        | isTeacher =
            [ Disclosure.viewAction (solutionEditButton r sol)
            , Disclosure.holdDestructiveAction (liftAction . V.HoldDeleteSolution) state.holdDeleteSolution sol.id
            ]
        | otherwise = []
   in V.solutionView (V.solutionTypeLabel sol.solutionType) isExpanded rendered actions (liftAction (V.ToggleSolution sol.id))
