-- | Task detail view for the task editor.
--
-- Renders the assignment refs banner above a standard task component.
-- The task itself is rendered identically to how it appears in assignments.
module Competences.Frontend.Component.TaskEditor.TaskDetailView
  ( taskDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Task (..), User)
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Task qualified as TaskComp
import Competences.Frontend.SyncContext (ProjectedChange (..), SyncContext (..), subscribeWithProjection)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- | Lightweight projection: assignment names referencing a given task
data TaskAssignmentRefs = TaskAssignmentRefs
  { assignmentNames :: ![AssignmentName]
  }
  deriving (Eq, Generic, Show)

-- | Projection function: filter doc.assignments for those containing this taskId
taskAssignmentRefsProjection :: TaskId -> Document -> Maybe User -> TaskAssignmentRefs
taskAssignmentRefsProjection taskId doc _mUser =
  let names =
        [ a.name
        | a <- Ix.toList doc.assignments
        , taskId `elem` a.tasks
        ]
   in TaskAssignmentRefs names

-- | Component: subscribes to projection, renders banner showing which assignments reference a task
assignmentRefsBanner :: SyncContext -> TaskId -> M.Component p TaskAssignmentRefs (ProjectedChange TaskAssignmentRefs)
assignmentRefsBanner r taskId =
  (M.component (TaskAssignmentRefs []) update' view')
    { M.subs = [subscribeWithProjection r (taskAssignmentRefsProjection taskId) id]
    }
  where
    update' change = M.modify $ \_ -> change.projection
    view' m
      | null m.assignmentNames = M.text ""
      | [AssignmentName single] <- m.assignmentNames =
          banner [M.text (C.translate' C.LblUsedInAssignment <> " " <> ms single)]
      | otherwise =
          banner
            [ M.text (C.translate' C.LblUsedInAssignments)
            , MH.ul_
                [class_ "list-disc list-inside mt-1"]
                [MH.li_ [] [M.text (ms n)] | AssignmentName n <- m.assignmentNames]
            ]
    banner content =
      MH.div_
        [class_ "rounded-lg border border-sky-200 bg-sky-50 p-3 text-sm text-sky-800"]
        content

-- | Task detail view: assignment refs banner + standard task component.
taskDetailView
  :: SyncContext
  -> EntityOrigin
  -> Task
  -> M.View p a
taskDetailView r origin task =
  MH.div_
    [class_ "space-y-4"]
    [ inlineComponent
        ("task-assignment-refs-" <> ms (show task.id))
        (assignmentRefsBanner r task.id)
    , inlineComponent
        ("task-detail-" <> ms (show task.id))
        (TaskComp.taskComponent r (TaskComp.TaskConfig task.id origin TaskComp.TaskInDetail))
    ]
