-- | Cross-entity assembly: provides renderers that combine multiple entity
-- components. Breaks circular dependencies by being the single module that
-- imports all entity Component modules.
module Competences.Frontend.Component.Entity.Assembly
  ( renderResolvedItem
  )
where

import Competences.Document (Resource (..), Task (..))
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Resource.Detailed qualified as ResComp
import Competences.Frontend.Component.ResourceLookup (ResolvedItem (..))
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Miso qualified as M
import Miso.String (ms)

-- | Render a resolved item (task or resource) as an inline component.
renderResolvedItem :: SyncContext -> ResolvedItem -> M.View m a
renderResolvedItem r (ResolvedResource res) =
  inlineComponent
    ("ln-resource-" <> ms (show res.id))
    (ResComp.resourceDetailedComponent r (ResComp.ResourceDetailedConfig res.id ResComp.defaultResourceDetailedSettings))
renderResolvedItem r (ResolvedTask tws) =
  inlineComponent
    ("ln-task-" <> ms (show tws.task.id))
    (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig tws.task.id Published TaskComp.defaultTaskDetailedSettings))
