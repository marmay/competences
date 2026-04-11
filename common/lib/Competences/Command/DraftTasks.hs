{-# LANGUAGE CPP #-}

module Competences.Command.DraftTasks
  ( DraftTasksCommand (..)
  , handleDraftTasksCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.Interpret (EntityCommandContext (..), doLock, doRelease, mkEntityCommandContext)
import Competences.Command.Tasks (TaskPatch (..), TaskGroupPatch (..), SubTaskPatch (..), applyTaskPatch, applyTaskGroupPatch, applySubTaskPatch)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), TaskGroup (..), TaskType (..), User (..))
import Competences.Document.Task (TaskGroupId, taskGroupId)
import Competences.Document.User (UserId, UserRole (..))
import Control.Monad (unless)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (^.))

-- | Commands for draft tasks (teacher-only, targeting draft collections)
data DraftTasksCommand
  = OnDraftTasks !(EntityCommand Task TaskPatch)
  | OnDraftTaskGroups !(EntityCommand TaskGroup TaskGroupPatch)
  | OnDraftSubTasks !(EntityCommand Task SubTaskPatch)
  deriving (Eq, Generic, Show)

instance Binary DraftTasksCommand
#ifdef WITH_AESON
instance FromJSON DraftTasksCommand
instance ToJSON DraftTasksCommand
#endif

-- | All teachers (draft entities only visible to teachers)
allTeachers :: Document -> AffectedUsers
allTeachers d = AffectedUsers $ map (.id) $ filter (\u -> u.role == Teacher) $ IxSet.toList $ d ^. #users

-- | Validate that a SubTask references an existing draft TaskGroup
validateDraftSubTaskReferencesGroup :: Document -> Task -> Either Text ()
validateDraftSubTaskReferencesGroup doc task = case task.taskType of
  SelfContained _ -> Right ()
  SubTask groupId _ -> do
    case Ix.getOne (Ix.getEQ groupId doc.draftTaskGroups) of
      Nothing -> Left $ "Draft TaskGroup not found: " <> T.pack (show groupId)
      Just _ -> Right ()

-- | Delete a draft TaskGroup and all its SubTasks (cascading delete)
deleteDraftTaskGroupCascading :: TaskGroupId -> Document -> Either Text (Document, TaskGroup)
deleteDraftTaskGroupCascading groupId doc = do
  group <- case Ix.getOne (Ix.getEQ groupId doc.draftTaskGroups) of
    Nothing -> Left "Draft TaskGroup not found"
    Just g -> Right g
  let subTasks = IxSet.toList $ Ix.getEQ (Just groupId) doc.draftTasks
  let doc' = doc & #draftTasks %~ \tasks -> foldr IxSet.delete tasks subTasks
  let doc'' = doc' & #draftTaskGroups %~ IxSet.delete group
  pure (doc'', group)

-- | Handle a DraftTasks context command
handleDraftTasksCommand :: UserId -> DraftTasksCommand -> Document -> UpdateResult
handleDraftTasksCommand userId cmd d = case cmd of
  OnDraftTasks c -> case c of
    Create task -> do
      case task.taskType of
        SubTask _ _ -> Left "Use OnDraftSubTasks to create SubTasks"
        SelfContained _ ->
          (,allTeachers d) <$> draftTaskContext.create task d
    CreateAndLock task lockUid lockSid -> do
      case task.taskType of
        SubTask _ _ -> Left "Use OnDraftSubTasks to create SubTasks"
        SelfContained _ -> do
          d' <- draftTaskContext.create task d
          d'' <- doLock lockUid lockSid (TaskLock task.id) d'
          pure (d'', allTeachers d)
    Delete taskId -> do
      task <- draftTaskContext.fetch taskId d
      case task.taskType of
        SubTask _ _ -> Left "Use OnDraftSubTasks to delete SubTasks"
        SelfContained _ -> do
          (d', _) <- draftTaskContext.delete taskId d
          pure (d', allTeachers d)
    Modify taskId modCmd -> case modCmd of
      Lock lockUid lockSid -> do
        d' <- doLock lockUid lockSid (TaskLock taskId) d
        task <- draftTaskContext.fetch taskId d'
        case task.taskType of
          SubTask _ _ -> Left "Cannot lock SubTasks (lock the parent TaskGroup instead)"
          SelfContained _ -> pure (d', allTeachers d)
      Release patch -> do
        d' <- doRelease userId (TaskLock taskId) d
        taskCurrent <- draftTaskContext.fetch taskId d'
        case taskCurrent.taskType of
          SubTask _ _ -> Left "Cannot modify SubTasks via OnDraftTasks (use OnDraftSubTasks instead)"
          SelfContained _ -> do
            taskModified <- applyTaskPatch taskCurrent patch
            (d'', _) <- draftTaskContext.delete taskId d'
            d''' <- draftTaskContext.create taskModified d''
            pure (d''', allTeachers d)

  OnDraftTaskGroups c -> case c of
    Create group ->
      (,allTeachers d) <$> draftTaskGroupContext.create group d
    CreateAndLock group lockUid lockSid -> do
      d' <- draftTaskGroupContext.create group d
      d'' <- doLock lockUid lockSid (TaskGroupLock group.id) d'
      pure (d'', allTeachers d)
    Delete groupId -> do
      (d', _) <- deleteDraftTaskGroupCascading groupId d
      pure (d', allTeachers d)
    Modify groupId modCmd -> case modCmd of
      Lock lockUid lockSid -> do
        d' <- doLock lockUid lockSid (TaskGroupLock groupId) d
        _group <- draftTaskGroupContext.fetch groupId d'
        pure (d', allTeachers d)
      Release patch -> do
        d' <- doRelease userId (TaskGroupLock groupId) d
        groupCurrent <- draftTaskGroupContext.fetch groupId d'
        groupModified <- applyTaskGroupPatch groupCurrent patch
        (d'', _) <- draftTaskGroupContext.delete groupId d'
        d''' <- draftTaskGroupContext.create groupModified d''
        pure (d''', allTeachers d)

  OnDraftSubTasks c -> case c of
    Create task -> do
      case task.taskType of
        SelfContained _ -> Left "Use OnDraftTasks to create SelfContained tasks"
        SubTask _ _ -> do
          validateDraftSubTaskReferencesGroup d task
          (,allTeachers d) <$> draftSubTaskContext.create task d
    CreateAndLock task lockUid lockSid -> do
      case task.taskType of
        SelfContained _ -> Left "Use OnDraftTasks to create SelfContained tasks"
        SubTask _ _ -> do
          validateDraftSubTaskReferencesGroup d task
          d' <- draftSubTaskContext.create task d
          d'' <- doLock lockUid lockSid (TaskLock task.id) d'
          pure (d'', allTeachers d)
    Delete taskId -> do
      task <- draftSubTaskContext.fetch taskId d
      case task.taskType of
        SelfContained _ -> Left "Use OnDraftTasks to delete SelfContained tasks"
        SubTask _ _ -> do
          (d', _) <- draftSubTaskContext.delete taskId d
          pure (d', allTeachers d)
    Modify taskId modCmd -> do
      taskCurrent <- draftSubTaskContext.fetch taskId d
      case taskCurrent.taskType of
        SelfContained _ -> Left "Cannot modify SelfContained tasks via OnDraftSubTasks (use OnDraftTasks instead)"
        SubTask _groupId _ -> case modCmd of
          Lock lockUid lockSid -> do
            d' <- doLock lockUid lockSid (TaskLock taskId) d
            pure (d', allTeachers d)
          Release patch -> do
            unless (Map.member (TaskLock taskId) (d ^. #locks)) $
              Left "SubTask must be locked to modify"
            taskModified <- applySubTaskPatch taskCurrent patch
            (d', _) <- draftSubTaskContext.delete taskId d
            d'' <- draftSubTaskContext.create taskModified d'
            d''' <- doRelease userId (TaskLock taskId) d''
            pure (d''', allTeachers d)
  where
    draftTaskContext =
      mkEntityCommandContext
        #draftTasks
        #id
        TaskLock
        applyTaskPatch
        (\_ d' -> allTeachers d')

    draftTaskGroupContext =
      mkEntityCommandContext
        #draftTaskGroups
        #id
        TaskGroupLock
        applyTaskGroupPatch
        (\_ d' -> allTeachers d')

    draftSubTaskContext =
      mkEntityCommandContext
        #draftTasks
        #id
        (\tid -> case taskGroupId <$> Ix.getOne (Ix.getEQ tid d.draftTasks) of
            Just (Just gid) -> TaskGroupLock gid
            _ -> TaskLock tid
        )
        applySubTaskPatch
        (\_ d' -> allTeachers d')
