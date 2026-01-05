module Competences.Command.Tasks
  ( TasksCommand (..)
  , TaskPatch (..)
  , TaskGroupPatch (..)
  , SubTaskPatch (..)
  , handleTasksCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), ModifyCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (EntityCommandContext (..), doLock, doRelease, mkEntityCommandContext)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence (..), Lock (..), Task (..), TaskGroup (..), TaskType (..), User (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskAttributesOverride (..)
  , TaskGroupIdentifier (..)
  , TaskGroupId
  , TaskId
  , TaskIdentifier (..)
  , TaskPurpose
  , taskGroupId
  )
import Competences.Document.User (UserId)
import Control.Monad (unless, (>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (.~), (^.))

-- | Patch for modifying a SelfContained Task
data TaskPatch = TaskPatch
  { identifier :: !(Change TaskIdentifier)
  , content :: !(Change (Maybe Text))
    -- Unwrapped TaskAttributes fields
  , primary :: !(Change [CompetenceLevelId])
  , secondary :: !(Change [CompetenceLevelId])
  , purpose :: !(Change TaskPurpose)
  , displayInResources :: !(Change Bool)
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying a TaskGroup
data TaskGroupPatch = TaskGroupPatch
  { identifier :: !(Change TaskGroupIdentifier)
  , contentBefore :: !(Change (Maybe Text))
  , contentAfter :: !(Change (Maybe Text))
    -- Unwrapped defaultTaskAttributes fields
  , primary :: !(Change [CompetenceLevelId])
  , secondary :: !(Change [CompetenceLevelId])
  , purpose :: !(Change TaskPurpose)
  , displayInResources :: !(Change Bool)
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying a SubTask
data SubTaskPatch = SubTaskPatch
  { identifier :: !(Change TaskIdentifier)
  , content :: !(Change (Maybe Text))
    -- TaskAttributesOverride fields (each is Maybe, so Change (Maybe X))
  , primary :: !(Change (Maybe [CompetenceLevelId]))
  , secondary :: !(Change (Maybe [CompetenceLevelId]))
  , purpose :: !(Change (Maybe TaskPurpose))
  , displayInResources :: !(Change (Maybe Bool))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Tasks context
data TasksCommand
  = OnTasks !(EntityCommand Task TaskPatch)
    -- ^ Commands for SelfContained tasks (uses TaskLock)
  | OnTaskGroups !(EntityCommand TaskGroup TaskGroupPatch)
    -- ^ Commands for TaskGroups (uses TaskGroupLock)
  | OnSubTasks !(EntityCommand Task SubTaskPatch)
    -- ^ Commands for SubTasks (uses TaskGroupLock on parent)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON TaskPatch
instance ToJSON TaskPatch
instance Binary TaskPatch

instance FromJSON TaskGroupPatch
instance ToJSON TaskGroupPatch
instance Binary TaskGroupPatch

instance FromJSON SubTaskPatch
instance ToJSON SubTaskPatch
instance Binary SubTaskPatch

instance FromJSON TasksCommand
instance ToJSON TasksCommand
instance Binary TasksCommand

-- Default instances
instance Default TaskPatch where
  def =
    TaskPatch
      { identifier = Nothing
      , content = Nothing
      , primary = Nothing
      , secondary = Nothing
      , purpose = Nothing
      , displayInResources = Nothing
      }

instance Default TaskGroupPatch where
  def =
    TaskGroupPatch
      { identifier = Nothing
      , contentBefore = Nothing
      , contentAfter = Nothing
      , primary = Nothing
      , secondary = Nothing
      , purpose = Nothing
      , displayInResources = Nothing
      }

instance Default SubTaskPatch where
  def =
    SubTaskPatch
      { identifier = Nothing
      , content = Nothing
      , primary = Nothing
      , secondary = Nothing
      , purpose = Nothing
      , displayInResources = Nothing
      }

-- | Apply a patch to a SelfContained Task, checking for conflicts
applyTaskPatch :: Task -> TaskPatch -> Either Text Task
applyTaskPatch task patch = do
  -- Verify this is a SelfContained task
  case task.taskType of
    SubTask _ _ -> Left "Cannot apply TaskPatch to a SubTask (use SubTaskPatch instead)"
    SelfContained attrs -> do
      -- Patch simple fields
      task' <-
        inContext "Task" task $
          patchField' @"identifier" patch
            >=> patchField' @"content" patch
      -- Patch TaskAttributes fields
      attrs' <-
        inContext "TaskAttributes" attrs $
          patchField' @"primary" patch
            >=> patchField' @"secondary" patch
            >=> patchField' @"purpose" patch
            >=> patchField' @"displayInResources" patch
      pure $ task' & #taskType .~ SelfContained attrs'

-- | Apply a patch to a TaskGroup, checking for conflicts
applyTaskGroupPatch :: TaskGroup -> TaskGroupPatch -> Either Text TaskGroup
applyTaskGroupPatch group patch = do
  -- Patch simple fields
  group' <-
    inContext "TaskGroup" group $
      patchField' @"identifier" patch
        >=> patchField' @"contentBefore" patch
        >=> patchField' @"contentAfter" patch
  -- Patch defaultTaskAttributes fields
  attrs' <-
    inContext "TaskAttributes" group.defaultTaskAttributes $
      patchField' @"primary" patch
        >=> patchField' @"secondary" patch
        >=> patchField' @"purpose" patch
        >=> patchField' @"displayInResources" patch
  pure $ group' & #defaultTaskAttributes .~ attrs'

-- | Apply a patch to a SubTask, checking for conflicts
applySubTaskPatch :: Task -> SubTaskPatch -> Either Text Task
applySubTaskPatch task patch = do
  -- Verify this is a SubTask
  case task.taskType of
    SelfContained _ -> Left "Cannot apply SubTaskPatch to a SelfContained task (use TaskPatch instead)"
    SubTask groupId override -> do
      -- Patch simple fields
      task' <-
        inContext "SubTask" task $
          patchField' @"identifier" patch
            >=> patchField' @"content" patch
      -- Patch TaskAttributesOverride fields
      override' <-
        inContext "TaskAttributesOverride" override $
          patchField' @"primary" patch
            >=> patchField' @"secondary" patch
            >=> patchField' @"purpose" patch
            >=> patchField' @"displayInResources" patch
      pure $ task' & #taskType .~ SubTask groupId override'

-- | Get all users (tasks affect all users because evidences reference tasks)
allUsers :: Document -> AffectedUsers
allUsers d = AffectedUsers $ map (.id) $ IxSet.toList $ d ^. #users

-- | Validate that a SubTask references an existing TaskGroup
validateSubTaskReferencesGroup :: Document -> Task -> Either Text ()
validateSubTaskReferencesGroup doc task = case task.taskType of
  SelfContained _ -> Right ()
  SubTask groupId _ -> do
    case Ix.getOne (Ix.getEQ groupId doc.taskGroups) of
      Nothing -> Left $ "TaskGroup not found: " <> T.pack (show groupId)
      Just _ -> Right ()

-- | Validate that no evidences reference this task
validateTaskNotReferencedInEvidences :: Document -> TaskId -> Either Text ()
validateTaskNotReferencedInEvidences doc taskId = do
  let referencingEvidences =
        filter (\e -> taskId `elem` e.tasks) $
          IxSet.toList doc.evidences
  unless (null referencingEvidences) $
    Left $ "Task is referenced by " <> T.pack (show (length referencingEvidences)) <> " evidence(s)"

-- | Delete a TaskGroup and all its SubTasks (cascading delete)
deleteTaskGroupCascading :: TaskGroupId -> Document -> Either Text (Document, TaskGroup)
deleteTaskGroupCascading groupId doc = do
  -- Fetch the TaskGroup
  group <- case Ix.getOne (Ix.getEQ groupId doc.taskGroups) of
    Nothing -> Left "TaskGroup not found"
    Just g -> Right g

  -- Find all SubTasks in this group
  let subTasks = IxSet.toList $ Ix.getEQ (Just groupId) doc.tasks

  -- Validate none of the SubTasks are referenced in evidences
  mapM_ (validateTaskNotReferencedInEvidences doc . (.id)) subTasks

  -- Delete all SubTasks
  let doc' = doc & #tasks %~ \tasks -> foldr IxSet.delete tasks subTasks

  -- Delete the TaskGroup
  let doc'' = doc' & #taskGroups %~ IxSet.delete group

  pure (doc'', group)

-- | Handle a Tasks context command
handleTasksCommand :: UserId -> TasksCommand -> Document -> UpdateResult
handleTasksCommand userId cmd d = case cmd of
  OnTasks c -> case c of
    Create task -> do
      -- Verify it's a SelfContained task
      case task.taskType of
        SubTask _ _ -> Left "Use OnSubTasks to create SubTasks"
        SelfContained _ ->
          (,taskContext.affectedUsers task d) <$> taskContext.create task d
    CreateAndLock task -> do
      -- Verify it's a SelfContained task
      case task.taskType of
        SubTask _ _ -> Left "Use OnSubTasks to create SubTasks"
        SelfContained _ -> do
          d' <- taskContext.create task d
          d'' <- doLock userId (TaskLock task.id) d'
          pure (d'', taskContext.affectedUsers task d)
    Delete taskId -> do
      -- Verify not referenced in evidences
      validateTaskNotReferencedInEvidences d taskId
      -- Fetch to check it's SelfContained
      task <- taskContext.fetch taskId d
      case task.taskType of
        SubTask _ _ -> Left "Use OnSubTasks to delete SubTasks"
        SelfContained _ -> do
          (d', task') <- taskContext.delete taskId d
          pure (d', taskContext.affectedUsers task' d)
    Modify taskId modCmd -> case modCmd of
      Lock -> do
        d' <- doLock userId (TaskLock taskId) d
        task <- taskContext.fetch taskId d'
        -- Verify it's SelfContained
        case task.taskType of
          SubTask _ _ -> Left "Cannot lock SubTasks (lock the parent TaskGroup instead)"
          SelfContained _ -> pure (d', taskContext.affectedUsers task d)
      Release patch -> do
        d' <- doRelease userId (TaskLock taskId) d
        taskCurrent <- taskContext.fetch taskId d'
        -- Verify it's SelfContained
        case taskCurrent.taskType of
          SubTask _ _ -> Left "Cannot modify SubTasks via OnTasks (use OnSubTasks instead)"
          SelfContained _ -> do
            taskModified <- applyTaskPatch taskCurrent patch
            (d'', taskOld) <- taskContext.delete taskId d'
            d''' <- taskContext.create taskModified d''
            pure (d''', taskContext.affectedUsers taskModified d <> taskContext.affectedUsers taskOld d)

  OnTaskGroups c -> case c of
    Create group ->
      (,taskGroupContext.affectedUsers group d) <$> taskGroupContext.create group d
    CreateAndLock group -> do
      d' <- taskGroupContext.create group d
      d'' <- doLock userId (TaskGroupLock group.id) d'
      pure (d'', taskGroupContext.affectedUsers group d)
    Delete groupId -> do
      -- Cascading delete: delete all SubTasks, then delete the group
      (d', group') <- deleteTaskGroupCascading groupId d
      pure (d', taskGroupContext.affectedUsers group' d)
    Modify groupId modCmd -> case modCmd of
      Lock -> do
        d' <- doLock userId (TaskGroupLock groupId) d
        group <- taskGroupContext.fetch groupId d'
        pure (d', taskGroupContext.affectedUsers group d)
      Release patch -> do
        d' <- doRelease userId (TaskGroupLock groupId) d
        groupCurrent <- taskGroupContext.fetch groupId d'
        groupModified <- applyTaskGroupPatch groupCurrent patch
        (d'', groupOld) <- taskGroupContext.delete groupId d'
        d''' <- taskGroupContext.create groupModified d''
        pure (d''', taskGroupContext.affectedUsers groupModified d <> taskGroupContext.affectedUsers groupOld d)

  OnSubTasks c -> case c of
    Create task -> do
      -- Verify it's a SubTask
      case task.taskType of
        SelfContained _ -> Left "Use OnTasks to create SelfContained tasks"
        SubTask _ _ -> do
          -- Verify TaskGroup exists
          validateSubTaskReferencesGroup d task
          (,subTaskContext.affectedUsers task d) <$> subTaskContext.create task d
    CreateAndLock task -> do
      -- Verify it's a SubTask
      case task.taskType of
        SelfContained _ -> Left "Use OnTasks to create SelfContained tasks"
        SubTask _ _ -> do
          -- Verify TaskGroup exists
          validateSubTaskReferencesGroup d task
          d' <- subTaskContext.create task d
          d'' <- doLock userId (TaskLock task.id) d'
          pure (d'', subTaskContext.affectedUsers task d)
    Delete taskId -> do
      -- Verify not referenced in evidences
      validateTaskNotReferencedInEvidences d taskId
      -- Fetch to check it's a SubTask
      task <- subTaskContext.fetch taskId d
      case task.taskType of
        SelfContained _ -> Left "Use OnTasks to delete SelfContained tasks"
        SubTask _ _ -> do
          (d', task') <- subTaskContext.delete taskId d
          pure (d', subTaskContext.affectedUsers task' d)
    Modify taskId modCmd -> do
      -- Fetch the task first
      taskCurrent <- subTaskContext.fetch taskId d
      -- Verify it's a SubTask
      case taskCurrent.taskType of
        SelfContained _ -> Left "Cannot modify SelfContained tasks via OnSubTasks (use OnTasks instead)"
        SubTask _groupId _ -> case modCmd of
          Lock -> do
            d' <- doLock userId (TaskLock taskId) d
            pure (d', subTaskContext.affectedUsers taskCurrent d)
          Release patch -> do
            -- Lock check: TaskLock must be held by this user
            unless (Map.member (TaskLock taskId) (d ^. #locks)) $
              Left "SubTask must be locked to modify"
            -- Apply patch
            taskModified <- applySubTaskPatch taskCurrent patch
            (d', taskOld) <- subTaskContext.delete taskId d
            d'' <- subTaskContext.create taskModified d'
            d''' <- doRelease userId (TaskLock taskId) d''
            pure (d''', subTaskContext.affectedUsers taskModified d <> subTaskContext.affectedUsers taskOld d)
  where
    taskContext =
      mkEntityCommandContext
        #tasks
        #id
        TaskLock
        applyTaskPatch
        (\_ d' -> allUsers d')

    taskGroupContext =
      mkEntityCommandContext
        #taskGroups
        #id
        TaskGroupLock
        applyTaskGroupPatch
        (\_ d' -> allUsers d')

    subTaskContext =
      mkEntityCommandContext
        #tasks
        #id
        (\tid -> case taskGroupId <$> Ix.getOne (Ix.getEQ tid d.tasks) of
            Just (Just gid) -> TaskGroupLock gid
            _ -> TaskLock tid  -- fallback, shouldn't happen
        )
        applySubTaskPatch
        (\_ d' -> allUsers d')
