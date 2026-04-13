{-# LANGUAGE CPP #-}

module Competences.Command.Tasks
  ( TasksCommand (..)
  , TaskPatch (..)
  , handleTasksCommand
  , applyTaskPatch
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (EntityCommandContext (..), doLock, doRelease, mkEntityCommandContext)
import Competences.Document (Document (..), Evidence (..), Lock (..), Task (..), User (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Task
  ( TaskId
  , TaskIdentifier (..)
  , TaskPurpose
  )
import Control.Monad (unless, (>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:?), (.!=))
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Map qualified as Map
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Patch for modifying a Task
data TaskPatch = TaskPatch
  { identifier :: !(Change TaskIdentifier)
  , title :: !(Change Text)
  , content :: !(Change (Maybe RichContent))
  , primary :: !(Change [CompetenceLevelId])
  , secondary :: !(Change [CompetenceLevelId])
  , purpose :: !(Change TaskPurpose)
  , displayInResources :: !(Change Bool)
  , attachments :: !(Change [FileRef])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Tasks context
data TasksCommand
  = OnTasks !(EntityCommand Task TaskPatch)
  deriving (Eq, Generic, Show)

instance Binary TaskPatch
#ifdef WITH_AESON
instance FromJSON TaskPatch where
  parseJSON = withObject "TaskPatch" $ \v ->
    TaskPatch
      <$> v .:? "identifier" .!= Nothing
      <*> v .:? "title" .!= Nothing
      <*> v .:? "content" .!= Nothing
      <*> v .:? "primary" .!= Nothing
      <*> v .:? "secondary" .!= Nothing
      <*> v .:? "purpose" .!= Nothing
      <*> v .:? "displayInResources" .!= Nothing
      <*> v .:? "attachments" .!= Nothing
instance ToJSON TaskPatch
#endif

instance Binary TasksCommand
#ifdef WITH_AESON
instance FromJSON TasksCommand
instance ToJSON TasksCommand
#endif

-- Default instances
instance Default TaskPatch where
  def =
    TaskPatch
      { identifier = Nothing
      , title = Nothing
      , content = Nothing
      , primary = Nothing
      , secondary = Nothing
      , purpose = Nothing
      , displayInResources = Nothing
      , attachments = Nothing
      }

-- | Apply a patch to a Task, checking for conflicts
applyTaskPatch :: Task -> TaskPatch -> Either Text Task
applyTaskPatch task patch =
  inContext "Task" task $
    patchField' @"identifier" patch
      >=> patchField' @"title" patch
      >=> patchField' @"content" patch
      >=> patchField' @"primary" patch
      >=> patchField' @"secondary" patch
      >=> patchField' @"purpose" patch
      >=> patchField' @"displayInResources" patch
      >=> patchField' @"attachments" patch

-- | Get all users (tasks affect all users because evidences reference tasks)
allUsers :: Document -> AffectedUsers
allUsers d = AffectedUsers $ map (.id) $ IxSet.toList $ d.users

-- | Validate that no evidences reference this task
validateTaskNotReferencedInEvidences :: Document -> TaskId -> Either Text ()
validateTaskNotReferencedInEvidences doc taskId = do
  let referencingEvidences =
        filter (\e -> Map.member taskId e.tasks) $
          IxSet.toList doc.evidences
  unless (null referencingEvidences) $
    Left $ "Task is referenced by " <> T.pack (show (length referencingEvidences)) <> " evidence(s)"

-- | Handle a Tasks context command
handleTasksCommand :: CommandContext -> TasksCommand -> Document -> UpdateResult
handleTasksCommand cmdCtx cmd d = case cmd of
  OnTasks c -> case c of
    Create task ->
      (,taskContext.affectedUsers task d) <$> taskContext.create task d
    CreateAndLock task -> do
      d' <- taskContext.create task d
      d'' <- doLock cmdCtx (TaskLock task.id) d'
      pure (d'', taskContext.affectedUsers task d)
    Delete taskId -> do
      validateTaskNotReferencedInEvidences d taskId
      (d', task') <- taskContext.delete taskId d
      pure (d', taskContext.affectedUsers task' d)
    Modify taskId modCmd -> case modCmd of
      Lock -> do
        d' <- doLock cmdCtx (TaskLock taskId) d
        task <- taskContext.fetch taskId d'
        pure (d', taskContext.affectedUsers task d)
      Release patch -> do
        d' <- doRelease cmdCtx (TaskLock taskId) d
        taskCurrent <- taskContext.fetch taskId d'
        taskModified <- applyTaskPatch taskCurrent patch
        (d'', taskOld) <- taskContext.delete taskId d'
        d''' <- taskContext.create taskModified d''
        pure (d''', taskContext.affectedUsers taskModified d <> taskContext.affectedUsers taskOld d)
  where
    taskContext =
      mkEntityCommandContext
        #tasks
        #id
        TaskLock
        applyTaskPatch
        (\_ d' -> allUsers d')
