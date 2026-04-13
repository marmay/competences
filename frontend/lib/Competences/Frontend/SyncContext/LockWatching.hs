-- | Wires up lock-watching for the WindowManager.
--
-- Separated from 'WindowManager' and 'SyncDocument' to avoid circular
-- module dependencies (both would need 'TaskPinEditor', which imports
-- 'SyncContext').
module Competences.Frontend.SyncContext.LockWatching
  ( initLockWatching
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.Id (idToText)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Component.TaskPinEditor (taskPinEditor)
import Competences.Frontend.SyncContext.SyncDocument
  ( DocumentChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , sendCommandOnly
  , subscribeDocumentIO
  )
import Competences.Frontend.SyncContext.WindowManager
  ( LockWatchConfig (..)
  , PinCategory (..)
  , PinId
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , WindowChrome (..)
  , mkPinId
  , pinDialogWith
  , pinIdKey
  , startLockWatching
  )
import Competences.Frontend.View.Icon qualified as Icon
import Data.Default (Default (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text qualified as T
import Competences.Document.Id (mkId)
import Miso.String (MisoString, ms)

-- | Initialize lock-watching for a 'SyncContext'.
-- Call this after 'mkSyncDocument' returns. Returns an unsubscribe action.
initLockWatching :: SyncContext -> IO (IO ())
initLockWatching r = do
  -- Pins the watcher is about to remove (lock gone). These should NOT
  -- trigger a Release command in onPinClosed.
  watcherRemovedRef <- newIORef Set.empty

  -- Install the onPinClosed callback that releases task locks
  writeIORef r.onPinClosedRef $ \pid -> do
    -- Check if this removal was initiated by the watcher (lock already gone)
    wasWatcherRemoval <- atomicModifyIORef' watcherRemovedRef $ \s ->
      (Set.delete pid s, Set.member pid s)
    if wasWatcherRemoval
      then pure () -- Lock already gone, no command needed
      else case parseTaskPinId pid of
        Just taskId -> sendCommandOnly r (Tasks (OnTasks (Modify taskId (Release def))))
        Nothing -> pure ()

  startLockWatching (mkLockWatchConfig r watcherRemovedRef) r.windowManager

-- | Build a 'LockWatchConfig' from a 'SyncContext', wiring up
-- document subscription, command sending, and pin creation.
mkLockWatchConfig :: SyncContext -> IORef (Set.Set PinId) -> LockWatchConfig
mkLockWatchConfig r watcherRemovedRef = LockWatchConfig
  { userId = r.env.connectedUser.id
  , sessionId = r.env.sessionId
  , subscribeDocChanges = \handler ->
      subscribeDocumentIO r (\change -> handler change.document)
  , ensurePin = \sink doc taskId ->
      let mTask = Ix.getOne (doc.tasks Ix.@= taskId)
          title = maybe ("Task" :: MisoString) (ms . taskDisplayName) mTask
          pid = mkPinId ("task-" <> idToText taskId)
          meta = PinMeta
            { key = "task-" <> idToText taskId
            , category = PinCatTask
            , sortKey = SortKey [SortAtom taskId]
            , context = Nothing
            }
          chrome = WindowChrome title Icon.IcnEdit
       in pinDialogWith sink meta chrome (taskPinEditor r taskId pid)
  , watcherRemovedRef = watcherRemovedRef
  }

-- | Try to extract a TaskId from a PinId.
parseTaskPinId :: PinId -> Maybe TaskId
parseTaskPinId pid = do
  rest <- T.stripPrefix "task-" (pinIdKey pid)
  mkId rest
