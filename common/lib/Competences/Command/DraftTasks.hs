{-# LANGUAGE CPP #-}

module Competences.Command.DraftTasks
  ( DraftTasksCommand (..)
  , handleDraftTasksCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.Interpret (EntityCommandContext (..), doLock, doRelease, mkEntityCommandContext)
import Competences.Command.Tasks (TaskPatch (..), applyTaskPatch)
import Competences.Document (Document (..), Lock (..), Task (..), User (..))
import Competences.Document.User (UserRole (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Commands for draft tasks (teacher-only, targeting draft collections)
data DraftTasksCommand
  = OnDraftTasks !(EntityCommand Task TaskPatch)
  deriving (Eq, Generic, Show)

instance Binary DraftTasksCommand
#ifdef WITH_AESON
instance FromJSON DraftTasksCommand
instance ToJSON DraftTasksCommand
#endif

-- | All teachers (draft entities only visible to teachers)
allTeachers :: Document -> AffectedUsers
allTeachers d = AffectedUsers $ map (.id) $ filter (\u -> u.role == Teacher) $ IxSet.toList $ d ^. #users

-- | Handle a DraftTasks context command
handleDraftTasksCommand :: CommandContext -> DraftTasksCommand -> Document -> UpdateResult
handleDraftTasksCommand cmdCtx cmd d = case cmd of
  OnDraftTasks c -> case c of
    Create task ->
      (,allTeachers d) <$> draftTaskContext.create task d
    CreateAndLock task -> do
      d' <- draftTaskContext.create task d
      d'' <- doLock cmdCtx (TaskLock task.id) d'
      pure (d'', allTeachers d)
    Delete taskId -> do
      (d', _) <- draftTaskContext.delete taskId d
      pure (d', allTeachers d)
    Modify taskId modCmd -> case modCmd of
      Lock -> do
        d' <- doLock cmdCtx (TaskLock taskId) d
        pure (d', allTeachers d)
      Release patch -> do
        d' <- doRelease cmdCtx (TaskLock taskId) d
        taskCurrent <- draftTaskContext.fetch taskId d'
        taskModified <- applyTaskPatch taskCurrent patch
        (d'', _) <- draftTaskContext.delete taskId d'
        d''' <- draftTaskContext.create taskModified d''
        pure (d''', allTeachers d)
  where
    draftTaskContext =
      mkEntityCommandContext
        #draftTasks
        #id
        TaskLock
        applyTaskPatch
        (\_ d' -> allTeachers d')
