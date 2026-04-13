-- | Read-only task detail view with LockButton for pin-based editing.
module Competences.Frontend.Component.TaskEditor.TaskDetailView
  ( taskDetailView
  )
where

import Competences.Command (Command (..), ModifyCommand (..), TasksCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), User)
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.TaskEditor.TaskSolutionsList (taskSolutionsListComponent)
import Competences.Frontend.SyncContext (ProjectedChange (..), SyncContext (..), subscribeWithProjection)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
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

-- | Read-only detail view for a task with LockButton to open pin editor
taskDetailView
  :: SyncContext
  -> EntityOrigin
  -> Task
  -> M.View p a
taskDetailView r origin task =
  let wrap = wrapForOrigin origin
   in MH.div_
    [class_ "space-y-6"]
    [ inlineComponent
        ("task-assignment-refs-" <> ms (show task.id))
        (assignmentRefsBanner r task.id)
    , -- Header with title and LockButton
      MH.div_
        [class_ "flex items-center justify-between"]
        [ MH.div_
            []
            [ Typography.h3 (ms (taskDisplayName task))
            ]
        , inlineComponent
            ("task-lock-btn-" <> ms (show task.id))
            (lockButtonComponent r
              (LockButtonConfig (TaskLock task.id) (wrap (Tasks (OnTasks (Modify task.id Lock)))) Button.IconTextS))
        ]
    , -- Task fields (read-only)
      MH.div_
        [class_ "space-y-4"]
        [ field (C.translate' C.LblTaskPurposeLabel) $
            M.text (C.translate' (C.LblTaskPurpose task.purpose))
        , field (C.translate' C.LblTaskPrimaryCompetences) $
            competenceList task.primary
        , field (C.translate' C.LblTaskSecondaryCompetences) $
            competenceList task.secondary
        , case task.content of
            Nothing -> M.text ""
            Just content ->
              field (C.translate' C.LblTaskContent) $
                renderRichText r.formulaCache content
        ]
    , -- Solutions list
      inlineComponent
        ("task-solutions-" <> ms (show task.id))
        (taskSolutionsListComponent r task.id)
    ]
  where
    field label content =
      MH.div_
        [class_ "space-y-1"]
        [ MH.div_ [class_ "text-sm font-medium text-stone-500"] [M.text label]
        , MH.div_ [] [content]
        ]

    competenceList :: [CompetenceLevelId] -> M.View p a
    competenceList [] = Typography.placeholder (C.translate' C.LblNoCompetences)
    competenceList comps =
      MH.div_
        [class_ "text-sm text-stone-600"]
        [M.text (ms (show (length comps)) <> " " <> C.translate' C.LblCompetence)]
