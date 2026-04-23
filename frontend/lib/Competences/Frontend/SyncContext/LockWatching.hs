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
import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..), LessonsCommand (..), ModifyCommand (..), ResourcesCommand (..), SolutionsCommand (..), TasksCommand (..))
import Competences.Command qualified as Cmd
import Competences.Command.LessonNotes (LessonNotesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), LessonNotes (..), Lock (..), Resource (..), ResourceIdentifier (..), Solution (..), Task (..), User (..))
import Competences.Document (TeachingNote (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Document.Id (idToText)
import Competences.Document.Lesson (Lesson (..), LessonId, LessonPhase (..))
import Data.Map.Strict qualified as Map
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Component.Assignment.PinEditor (assignmentPinEditor)
import Competences.Frontend.Component.Assignment.Detailed (pinAssignmentViewer)
import Competences.Frontend.Component.LessonNotes.Detailed qualified as LNComp
import Competences.Frontend.Component.LessonNotes.PinEditor (lessonNotesPinEditor)
import Competences.Frontend.Component.Planning.LessonPinEditor (lessonPinEditor)
import Competences.Frontend.Component.Resource.Detailed qualified as ResComp
import Competences.Frontend.Component.Resource.PinEditor (resourcePinEditor)
import Competences.Frontend.Component.Entity.Assembly (renderResolvedItem)
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.Component.Draft (EntityOrigin (..), retargetForDraft)
import Competences.Frontend.Component.Task.PinEditor (taskPinEditor)
import Competences.Frontend.Component.Task.SolutionPinEditor (solutionPinEditor)
import Competences.Frontend.SyncContext.SyncDocument
  ( DocumentChange (..)
  , DocumentChangeInfo (..)
  , PinViewerRequest (..)
  , SyncContext (..)
  , SyncDocument (..)
  , SyncDocumentEnv (..)
  , readSyncDocument
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
import Data.Maybe (isJust)
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
        Just lock -> do
          sd <- readSyncDocument r
          sendCommandOnly r (releaseCommand sd.localDocument lock)
        Nothing -> pure ()

  writeIORef r.onPinViewerRequestRef (handleViewerPin r)

  startLockWatching (mkLockWatchConfig r watcherRemovedRef) r.windowManager

-- ============================================================================
-- Lock → Pin mapping
-- ============================================================================

-- | Map a lock to a deterministic PinId.
lockPinId' :: Lock -> PinId
lockPinId' (TaskLock tid) = mkPinId ("task-" <> idToText tid)
lockPinId' (SolutionLock sid) = mkPinId ("solution-" <> idToText sid)
lockPinId' (ResourceLock rid) = mkPinId ("resource-" <> idToText rid)
lockPinId' (LessonNotesLock lnid) = mkPinId ("lesson-notes-" <> idToText lnid)
lockPinId' (LessonLock lid) = mkPinId ("lesson-" <> idToText lid)
lockPinId' (AssignmentLock aid) = mkPinId ("assignment-" <> idToText aid)
lockPinId' lock = mkPinId (T.pack (show lock))

-- | Try to recover a Lock from a PinId (inverse of 'lockPinId'').
parsePinLock :: PinId -> Maybe Lock
parsePinLock pid =
  let key = pinIdKey pid
   in (TaskLock <$> (T.stripPrefix "task-" key >>= mkId))
        <|> (SolutionLock <$> (T.stripPrefix "solution-" key >>= mkId))
        <|> (ResourceLock <$> (T.stripPrefix "resource-" key >>= mkId))
        <|> (LessonNotesLock <$> (T.stripPrefix "lesson-notes-" key >>= mkId))
        <|> (LessonLock <$> (T.stripPrefix "lesson-" key >>= mkId))
        <|> (AssignmentLock <$> (T.stripPrefix "assignment-" key >>= mkId))

-- | Build the command to release a lock, routing to draft collection when needed.
releaseCommand :: Document -> Lock -> Command
releaseCommand doc (TaskLock tid) =
  let cmd = Tasks (OnTasks (Modify tid (Release def)))
   in if Ix.null (doc.draftTasks Ix.@= tid) then cmd else retargetForDraft cmd
releaseCommand _doc (SolutionLock sid) = Solutions (OnSolutions (Modify sid (Release def)))
releaseCommand _doc (ResourceLock rid) = Resources (OnResources (Modify rid (Release def)))
releaseCommand _doc (LessonNotesLock lnid) = Cmd.LessonNotes (OnLessonNotes (Modify lnid (Release def)))
releaseCommand _doc (LessonLock lid) = Lessons (OnLessons (Modify lid (Release def)))
releaseCommand doc (AssignmentLock aid) =
  let cmd = Assignments (OnAssignments (Modify aid (Release def)))
   in if Ix.null (doc.draftAssignments Ix.@= aid) then cmd else retargetForDraft cmd
releaseCommand _ _ = error "releaseCommand: unhandled lock type"

-- ============================================================================
-- Pin creation per lock type
-- ============================================================================

-- | Create a pin editor for a lock.
ensureLockPin :: SyncContext -> WindowEventSink -> Lock -> Document -> Bool -> IO ()
ensureLockPin r sink lock doc followUp = case lock of
  TaskLock tid -> ensureTaskPin r sink tid doc followUp
  SolutionLock sid -> ensureSolutionPin r sink sid doc followUp
  ResourceLock rid -> ensureResourcePin r sink rid doc followUp
  LessonNotesLock lnid -> ensureLessonNotesPin r sink lnid doc followUp
  LessonLock lid -> ensureLessonPin r sink lid doc followUp
  AssignmentLock aid -> ensureAssignmentPin r sink aid doc followUp
  _ -> pure () -- No pin editor for other lock types yet

ensureTaskPin :: SyncContext -> WindowEventSink -> TaskId -> Document -> Bool -> IO ()
ensureTaskPin r sink taskId doc followUp =
  let mPublished = Ix.getOne (doc.tasks Ix.@= taskId)
      mDraft = Ix.getOne (doc.draftTasks Ix.@= taskId)
      mTask = mPublished <|> mDraft
      origin = if isJust mDraft then Draft else Published
      title = maybe ("Task" :: MisoString) (ms . taskDisplayName) mTask
      pid = lockPinId' (TaskLock taskId)
      meta = PinMeta
        { key = "task-" <> idToText taskId
        , category = PinCatTask
        , sortKey = SortKey [SortAtom taskId]
        , context = Nothing
        , isEditor = True
        , followUp = followUp
        }
      chrome = WindowChrome title Icon.IcnTask (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (taskPinEditor r taskId origin pid)

ensureSolutionPin :: SyncContext -> WindowEventSink -> SolutionId -> Document -> Bool -> IO ()
ensureSolutionPin r sink solId doc followUp =
  let mSol = Ix.getOne (doc.solutions Ix.@= solId)
      mTask = mSol >>= \sol -> Ix.getOne (doc.tasks Ix.@= sol.taskId)
      title = maybe ("Lösung" :: MisoString) (\t -> ms (taskDisplayName t) <> " – Lösung") mTask
      pid = lockPinId' (SolutionLock solId)
      meta = PinMeta
        { key = "solution-" <> idToText solId
        , category = PinCatTask
        , sortKey = SortKey [SortAtom solId]
        , context = Nothing
        , isEditor = True
        , followUp = followUp
        }
      chrome = WindowChrome title Icon.IcnSolution (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (solutionPinEditor r solId pid)

ensureResourcePin :: SyncContext -> WindowEventSink -> ResourceId -> Document -> Bool -> IO ()
ensureResourcePin r sink resId doc followUp =
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
        , isEditor = True
        , followUp = followUp
        }
      chrome = WindowChrome title Icon.IcnResources (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (resourcePinEditor r resId pid)

ensureLessonNotesPin :: SyncContext -> WindowEventSink -> LessonNotesId -> Document -> Bool -> IO ()
ensureLessonNotesPin r sink lnId doc followUp =
  let mLn = Ix.getOne (doc.lessonNotes Ix.@= lnId)
      title = maybe ("Unterrichtsnotiz" :: MisoString) (ms . (.title)) mLn
      pid = lockPinId' (LessonNotesLock lnId)
      meta = PinMeta
        { key = "lesson-notes-" <> idToText lnId
        , category = PinCatLessonNotes
        , sortKey = SortKey [SortAtom lnId]
        , context = Nothing
        , isEditor = True
        , followUp = followUp
        }
      chrome = WindowChrome title Icon.IcnLessonNotes (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (lessonNotesPinEditor r lnId pid)

ensureLessonPin :: SyncContext -> WindowEventSink -> LessonId -> Document -> Bool -> IO ()
ensureLessonPin r sink lid doc followUp =
  case Ix.getOne (doc.lessons Ix.@= lid) of
    Nothing -> pure ()
    Just lesson ->
      let title = if T.null lesson.title then ("Lektion" :: MisoString) else ms lesson.title
          pid = lockPinId' (LessonLock lid)
          meta = PinMeta
            { key = "lesson-" <> idToText lid
            , category = PinCatLesson
            , sortKey = SortKey [SortAtom lesson.order, SortAtom lid]
            , context = Nothing
            , isEditor = True
            , followUp = followUp
            }
          chrome = WindowChrome title Icon.IcnMesoPlan (Just Icon.IcnEdit)
          lookupContent nid = maybe mempty (.content) (Ix.getOne (doc.teachingNotes Ix.@= nid))
          lessonNoteContent = maybe mempty lookupContent lesson.privateNoteRef
          phaseNoteContents = Map.fromList
            [ (idx, lookupContent nid)
            | (idx, p) <- zip [0 ..] lesson.phases
            , Just nid <- [p.privateNoteRef]
            ]
       in pinDialogWith sink meta chrome
            (lessonPinEditor r lesson lessonNoteContent phaseNoteContents pid)

ensureAssignmentPin :: SyncContext -> WindowEventSink -> AssignmentId -> Document -> Bool -> IO ()
ensureAssignmentPin r sink aid doc followUp =
  let mPublished = Ix.getOne (doc.assignments Ix.@= aid)
      mDraft = Ix.getOne (doc.draftAssignments Ix.@= aid)
      mAssignment = mPublished <|> mDraft
      origin = if isJust mDraft then Draft else Published
      title = case mAssignment of
        Just a -> let AssignmentName t = a.name in ms t
        Nothing -> "Auftrag" :: MisoString
      pid = lockPinId' (AssignmentLock aid)
      meta = PinMeta
        { key = "assignment-" <> idToText aid
        , category = PinCatAssignment
        , sortKey = SortKey [SortAtom aid]
        , context = Nothing
        , isEditor = True
        , followUp = followUp
        }
      chrome = WindowChrome title Icon.IcnAssignment (Just Icon.IcnEdit)
   in pinDialogWith sink meta chrome (assignmentPinEditor r aid origin pid)

-- ============================================================================
-- Viewer Pins
-- ============================================================================

handleViewerPin :: SyncContext -> PinViewerRequest -> IO ()
handleViewerPin r (PinTaskViewer task) =
  pinDialogWith r.windowManager
    (PinMeta
      { key = "task-ref-" <> idToText task.id
      , category = PinCatTask
      , sortKey = SortKey [SortAtom task.id]
      , context = Nothing
      , isEditor = False
      , followUp = True
      })
    (WindowChrome (ms (taskDisplayName task)) Icon.IcnTask Nothing)
    (\_ (_ :: Maybe ()) -> TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig task.id Published TaskComp.defaultTaskDetailedSettings))
handleViewerPin r (PinResourceViewer res) =
  let ResourceIdentifier ident = res.identifier
      title = if T.null ident then "Ressource" else ident
   in pinDialogWith r.windowManager
        (PinMeta
          { key = "resource-ref-" <> idToText res.id
          , category = PinCatResource
          , sortKey = SortKey [SortAtom res.id]
          , context = Nothing
          , isEditor = False
          , followUp = True
          })
        (WindowChrome (ms title) Icon.IcnResources Nothing)
        (\_ (_ :: Maybe ()) -> ResComp.resourceDetailedComponent r (ResComp.ResourceDetailedConfig res.id ResComp.defaultResourceDetailedSettings))
handleViewerPin r (PinLessonNotesViewer ln) =
  pinDialogWith r.windowManager
    (PinMeta
      { key = "lesson-notes-ref-" <> idToText ln.id
      , category = PinCatLessonNotes
      , sortKey = SortKey [SortAtom ln.date, SortAtom ln.title, SortAtom ln.id]
      , context = Nothing
      , isEditor = False
      , followUp = True
      })
    (WindowChrome (ms ln.title) Icon.IcnLessonNotes Nothing)
    (\_ (_ :: Maybe ()) -> LNComp.lessonNotesDetailedComponent renderResolvedItem r (LNComp.LessonNotesDetailedConfig ln.id LNComp.defaultLessonNotesDetailedSettings))
handleViewerPin r (PinAssignmentViewer assignment) =
  pinAssignmentViewer r r.env.connectedUser assignment

-- ============================================================================
-- Config
-- ============================================================================

mkLockWatchConfig :: SyncContext -> IORef (Set.Set PinId) -> LockWatchConfig
mkLockWatchConfig r watcherRemovedRef = LockWatchConfig
  { userId = r.env.connectedUser.id
  , sessionId = r.env.sessionId
  , subscribeDocChanges = \handler ->
      subscribeDocumentIO r (\change -> handler change.document (allowsFollowUp change.change))
  , ensurePin = \sink doc lock followUp -> ensureLockPin r sink lock doc followUp
  , lockPinId = lockPinId'
  , watcherRemovedRef = watcherRemovedRef
  }
  where
    allowsFollowUp (DocumentChanged _ _) = True
    allowsFollowUp InitialUpdate = False
    allowsFollowUp DocumentReloaded = False

