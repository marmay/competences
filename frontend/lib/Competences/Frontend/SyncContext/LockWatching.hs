-- | Wires up lock-watching for the WindowManager.
--
-- Separated from 'WindowManager' and 'SyncDocument' to avoid circular
-- module dependencies (both would need pin editor imports, which import
-- 'SyncContext').
module Competences.Frontend.SyncContext.LockWatching
  ( initLockWatching
  )
where

import Control.Applicative ((<|>))
import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), ResourcesCommand (..), SolutionsCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Resource (..), ResourceIdentifier (..), Solution (..), User (..))
import Competences.Document.Id (idToText)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Component.Resource.PinEditor (resourcePinEditor)
import Competences.Frontend.Component.Task.PinEditor (taskPinEditor)
import Competences.Frontend.Component.Task.SolutionPinEditor (solutionPinEditor)
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
  , WindowEventSink
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

  -- Install the onPinClosed callback that releases locks
  writeIORef r.onPinClosedRef $ \pid -> do
    -- Check if this removal was initiated by the watcher (lock already gone)
    wasWatcherRemoval <- atomicModifyIORef' watcherRemovedRef $ \s ->
      (Set.delete pid s, Set.member pid s)
    if wasWatcherRemoval
      then pure () -- Lock already gone, no command needed
      else case parsePinLock pid of
        Just lock -> sendCommandOnly r (releaseCommand lock)
        Nothing -> pure ()

  startLockWatching (mkLockWatchConfig r watcherRemovedRef) r.windowManager

-- ============================================================================
-- Lock → Pin mapping
-- ============================================================================

-- | Map a lock to a deterministic PinId.
lockPinId' :: Lock -> PinId
lockPinId' (TaskLock tid) = mkPinId ("task-" <> idToText tid)
lockPinId' (SolutionLock sid) = mkPinId ("solution-" <> idToText sid)
lockPinId' (ResourceLock rid) = mkPinId ("resource-" <> idToText rid)
lockPinId' lock = mkPinId (T.pack (show lock))

-- | Try to recover a Lock from a PinId (inverse of 'lockPinId'').
parsePinLock :: PinId -> Maybe Lock
parsePinLock pid =
  let key = pinIdKey pid
   in (TaskLock <$> (T.stripPrefix "task-" key >>= mkId))
        <|> (SolutionLock <$> (T.stripPrefix "solution-" key >>= mkId))
        <|> (ResourceLock <$> (T.stripPrefix "resource-" key >>= mkId))

-- | Build the command to release a lock.
releaseCommand :: Lock -> Command
releaseCommand (TaskLock tid) = Tasks (OnTasks (Modify tid (Release def)))
releaseCommand (SolutionLock sid) = Solutions (OnSolutions (Modify sid (Release def)))
releaseCommand (ResourceLock rid) = Resources (OnResources (Modify rid (Release def)))
releaseCommand _ = error "releaseCommand: unhandled lock type"

-- ============================================================================
-- Pin creation per lock type
-- ============================================================================

-- | Create a pin editor for a lock.
ensureLockPin :: SyncContext -> WindowEventSink -> Lock -> Document -> IO ()
ensureLockPin r sink lock doc = case lock of
  TaskLock tid -> ensureTaskPin r sink tid doc
  SolutionLock sid -> ensureSolutionPin r sink sid doc
  ResourceLock rid -> ensureResourcePin r sink rid doc
  _ -> pure () -- No pin editor for other lock types yet

ensureTaskPin :: SyncContext -> WindowEventSink -> TaskId -> Document -> IO ()
ensureTaskPin r sink taskId doc =
  let mTask = Ix.getOne (doc.tasks Ix.@= taskId)
      title = maybe ("Task" :: MisoString) (ms . taskDisplayName) mTask
      pid = lockPinId' (TaskLock taskId)
      meta = PinMeta
        { key = "task-" <> idToText taskId
        , category = PinCatTask
        , sortKey = SortKey [SortAtom taskId]
        , context = Nothing
        }
      chrome = WindowChrome title Icon.IcnTask (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (taskPinEditor r taskId pid)

ensureSolutionPin :: SyncContext -> WindowEventSink -> SolutionId -> Document -> IO ()
ensureSolutionPin r sink solId doc =
  let mSol = Ix.getOne (doc.solutions Ix.@= solId)
      mTask = mSol >>= \sol -> Ix.getOne (doc.tasks Ix.@= sol.taskId)
      title = maybe ("Lösung" :: MisoString) (\t -> ms (taskDisplayName t) <> " – Lösung") mTask
      pid = lockPinId' (SolutionLock solId)
      meta = PinMeta
        { key = "solution-" <> idToText solId
        , category = PinCatTask
        , sortKey = SortKey [SortAtom solId]
        , context = Nothing
        }
      chrome = WindowChrome title Icon.IcnSolution (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (solutionPinEditor r solId pid)

ensureResourcePin :: SyncContext -> WindowEventSink -> ResourceId -> Document -> IO ()
ensureResourcePin r sink resId doc =
  let mRes = Ix.getOne (doc.resources Ix.@= resId)
      title = case mRes of
        Just res -> let ResourceIdentifier t = res.identifier in ms t
        Nothing -> "Ressource" :: MisoString
      pid = lockPinId' (ResourceLock resId)
      meta = PinMeta
        { key = "resource-" <> idToText resId
        , category = PinCatResource
        , sortKey = SortKey [SortAtom resId]
        , context = Nothing
        }
      chrome = WindowChrome title Icon.IcnResources (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (resourcePinEditor r resId pid)

-- ============================================================================
-- Config
-- ============================================================================

mkLockWatchConfig :: SyncContext -> IORef (Set.Set PinId) -> LockWatchConfig
mkLockWatchConfig r watcherRemovedRef = LockWatchConfig
  { userId = r.env.connectedUser.id
  , sessionId = r.env.sessionId
  , subscribeDocChanges = \handler ->
      subscribeDocumentIO r (\change -> handler change.document)
  , ensurePin = \sink doc lock -> ensureLockPin r sink lock doc
  , lockPinId = lockPinId'
  , watcherRemovedRef = watcherRemovedRef
  }
