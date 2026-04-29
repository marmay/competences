{-# LANGUAGE CPP #-}

module Competences.Command.DraftTasks
  ( DraftTasksCommand (..)
  , handleDraftTasksCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.Interpret (EntityCommandContext (..), doLock, doRelease, mkEntityCommandContext)
import Competences.Command.Tasks (TaskPatch (..), applyTaskPatch)
import Competences.Document (Document (..), Lock (..), Task (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import GHC.Generics (Generic)

-- | Commands for draft tasks (teacher-only, targeting draft collections)
data DraftTasksCommand
  = OnDraftTasks !(EntityCommand Task TaskPatch)
  deriving (Eq, Generic, Show)

instance Binary DraftTasksCommand
#ifdef WITH_AESON
instance FromJSON DraftTasksCommand
instance ToJSON DraftTasksCommand
#endif

-- | Handle a DraftTasks context command
handleDraftTasksCommand :: CommandContext -> DraftTasksCommand -> Document -> UpdateResult
handleDraftTasksCommand cmdCtx cmd d = case cmd of
  OnDraftTasks c -> case c of
    Create task ->
      (,AudienceTeachers) <$> draftTaskContext.create task d
    CreateAndLock task -> do
      d' <- draftTaskContext.create task d
      d'' <- doLock cmdCtx (TaskLock task.id) d'
      pure (d'', AudienceTeachers)
    Delete taskId -> do
      (d', _) <- draftTaskContext.delete taskId d
      pure (d', AudienceTeachers)
    Modify taskId modCmd -> case modCmd of
      Lock -> do
        d' <- doLock cmdCtx (TaskLock taskId) d
        pure (d', AudienceTeachers)
      Release patch -> do
        d' <- doRelease cmdCtx (TaskLock taskId) d
        taskCurrent <- draftTaskContext.fetch taskId d'
        taskModified <- applyTaskPatch taskCurrent patch
        (d'', _) <- draftTaskContext.delete taskId d'
        d''' <- draftTaskContext.create taskModified d''
        pure (d''', AudienceTeachers)
  where
    draftTaskContext =
      mkEntityCommandContext
        #draftTasks
        #id
        TaskLock
        applyTaskPatch
        (\_ _ -> AudienceTeachers)
