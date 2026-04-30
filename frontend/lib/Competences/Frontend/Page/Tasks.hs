module Competences.Frontend.Page.Tasks
  ( tasksPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Task (..), User)
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.WithOrigin (WithOrigin (..))
import Competences.Frontend.Component.Draft (EntityOrigin)
import Competences.Frontend.Component.Task.Selector (taskSelectorComponent)
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.SyncContext (ProjectedChange (..), SyncContext (..), subscribeWithProjection)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- | Model for the unified task editor
data Model = Model
  { selected :: !(Maybe (WithOrigin Task))
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Action for the unified task editor
data Action
  = ToggleSidebar
  deriving (Eq, Show)

-- | Unified task editor component.
--
-- The page stays mounted across URL pushes (the App's mount key is
-- constructor-only). Selection state lives in 'm.selected', written
-- by the task selector via its parent-binding. The selector also
-- owns URL synchronisation: it parses incoming URIs, dispatches its
-- own selection on back/forward, and pushes the URL on user click.
tasksPage :: SyncContext -> Maybe TaskId -> M.Component p Model Action
tasksPage r mTaskId =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        (inlineComponentAttrs "task-selector" [class_ "h-full"] $ taskSelectorComponent r mTaskId #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just w) =
      taskDetailView r w.origin w.value

-- ---------------------------------------------------------------------------
-- Task detail view: assignment-refs banner + standard task component
-- ---------------------------------------------------------------------------

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
        (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig task.id origin adminSettings))
    ]
  where
    adminSettings = TaskComp.defaultTaskDetailedSettings {TaskComp.enableGoTo = False, TaskComp.enableDelete = True}
