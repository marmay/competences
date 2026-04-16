-- | Full Miso component wrapping the detailed task Fragment.
module Competences.Frontend.Component.Task.Detailed
  ( TaskDetailedConfig (..)
  , TaskDetailedSettings (..)
  , defaultTaskDetailedSettings
  , taskDetailedComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Component.Task.Detailed.Embed (renderSolutionList, updateTaskDetailed)
import Competences.Frontend.Fragment.Task.Badge (assessmentStar, purposeBadge)
import Competences.Frontend.Fragment.Task.Detailed qualified as V
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , isTeacher
  , modifySyncDocument
  , subscribeWithProjection
  )
import Competences.Frontend.View.EntityMenu (EntityMenuConfig (..), entityMenu)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((.~))

data TaskDetailedConfig = TaskDetailedConfig
  { taskId :: !TaskId
  , origin :: !EntityOrigin
  , settings :: !TaskDetailedSettings
  }

data TaskDetailedSettings = TaskDetailedSettings
  { collapsible :: !Bool
  , showSolutions :: !Bool
  , showAnnotations :: !Bool
  , startExpanded :: !Bool
  , enableGoTo :: !Bool
  , enableDelete :: !Bool
  }
  deriving (Eq, Show)

defaultTaskDetailedSettings :: TaskDetailedSettings
defaultTaskDetailedSettings = TaskDetailedSettings
  { collapsible = False
  , showSolutions = True
  , showAnnotations = True
  , startExpanded = True
  , enableGoTo = True
  , enableDelete = False
  }

data TaskProjection = TaskProjection
  { task :: !(Maybe Task)
  , solutions :: ![Solution]
  , hasFocusedStudent :: !Bool
  }
  deriving (Eq, Generic, Show)

data Model = Model
  { projection :: !TaskProjection
  , viewState :: !V.TaskDetailedState
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange TaskProjection)
  | ViewAction !V.TaskDetailedAction
  deriving (Eq, Show)

taskDetailedComponent :: SyncContext -> TaskDetailedConfig -> M.Component p Model Action
taskDetailedComponent r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (taskProjection cfg) ProjectionChanged]
    }
  where
    model = Model
      { projection = TaskProjection
          { task = Nothing
          , solutions = []
          , hasFocusedStudent = False
          }
      , viewState = V.initialTaskDetailedState [cfg.taskId | cfg.settings.startExpanded]
      }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction (V.MenuEdit tid)) =
      M.io_ $ modifySyncDocument r $ wrapForOrigin cfg.origin (Tasks (OnTasks (Modify tid Lock)))
    update' (ViewAction (V.MenuDelete tid)) =
      M.io_ $ modifySyncDocument r $ wrapForOrigin cfg.origin (Tasks (OnTasks (Delete tid)))
    update' (ViewAction a) = updateTaskDetailed #viewState r ViewAction a

    view' m = case m.projection.task of
      Nothing -> Layout.empty
      Just task -> viewTask r cfg m task

taskProjection :: TaskDetailedConfig -> Document -> Maybe User -> TaskProjection
taskProjection cfg doc mUser =
  TaskProjection
    { task = case cfg.origin of
        Published -> Ix.getOne (doc.tasks Ix.@= cfg.taskId)
        Draft -> Ix.getOne (doc.draftTasks Ix.@= cfg.taskId)
    , solutions = Ix.toList (doc.solutions Ix.@= cfg.taskId)
    , hasFocusedStudent = maybe False (\u -> u.role == Student) mUser
    }

viewTask :: SyncContext -> TaskDetailedConfig -> Model -> Task -> M.View Model Action
viewTask r cfg m task =
  let displayName = ms (taskDisplayName task)
      annotations
        | cfg.settings.showAnnotations = headerAnnotations r cfg m task
        | otherwise = []
      body = taskBody r cfg m task
      expanded = Set.member cfg.taskId m.viewState.expandedTasks
   in if cfg.settings.collapsible
        then V.taskDisclosureView Nothing (ViewAction (V.ToggleTask cfg.taskId)) displayName annotations expanded body
        else V.taskOpenView displayName annotations body

headerAnnotations :: SyncContext -> TaskDetailedConfig -> Model -> Task -> [M.View Model Action]
headerAnnotations r cfg m task =
  concat
    [ [purposeBadge task.purpose | m.projection.hasFocusedStudent]
    , [assessmentStar task.purpose | m.projection.hasFocusedStudent]
    , [ entityMenu EntityMenuConfig
          { onEdit = Just (ViewAction (V.MenuEdit task.id))
          , onPin = Just (ViewAction (V.MenuPin task))
          , onGoTo = if cfg.settings.enableGoTo then Just (ViewAction (V.MenuGoTo task.id)) else Nothing
          , onDelete = if cfg.settings.enableDelete then Just (ViewAction (V.MenuDelete task.id)) else Nothing
          }
      | isTeacher r
      ]
    ]

taskBody :: SyncContext -> TaskDetailedConfig -> Model -> Task -> M.View Model Action
taskBody r cfg m task =
  MH.div_ [class_ "space-y-3"] $
    concat
      [ [taskContentRendered r task | hasContent task]
      , [ renderSolutionList r m.viewState ViewAction cfg.taskId m.projection.solutions
        | cfg.settings.showSolutions
        , not (null m.projection.solutions) || isTeacher r
        ]
      ]

hasContent :: Task -> Bool
hasContent task = case task.content of
  Nothing -> False
  Just c -> c /= mempty

taskContentRendered :: SyncContext -> Task -> M.View Model Action
taskContentRendered r task = case task.content of
  Nothing -> Layout.empty
  Just content ->
    if content == mempty
      then Layout.empty
      else V.taskContentView (renderRichTextWithFiles r.formulaCache r task.attachments content)
