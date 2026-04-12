-- | Periodic cleanup of locks held by stale sessions.
--
-- A background thread checks for sessions that have been disconnected
-- for longer than a threshold and releases their locks by submitting
-- Unlock commands through the normal command processor pipeline.
module Competences.Backend.StaleLockCleanup
  ( startCleanupThread
  )
where

import Competences.Backend.CommandProcessor (CommandProcessor, submitCommand)
import Competences.Backend.SessionRegistry (SessionRegistry, findStaleSessions)
import Competences.Command (Command (..))
import Competences.Document (Document (..))
import Competences.Document.Lock (LockHolder (..))
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad (forM_, forever)
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

-- | How often to check for stale sessions (5 minutes).
cleanupIntervalMicros :: Int
cleanupIntervalMicros = 5 * 60 * 1000000

-- | Start the stale lock cleanup background thread.
-- Checks every 5 minutes for sessions disconnected longer than the threshold.
startCleanupThread
  :: SessionRegistry
  -> TVar Document
  -> CommandProcessor
  -> NominalDiffTime
  -- ^ Stale threshold (e.g. 6 hours)
  -> IO ThreadId
startCleanupThread registry docVar proc threshold =
  forkIO $ forever $ do
    threadDelay cleanupIntervalMicros
    cleanupOnce registry docVar proc threshold

-- | Run one cleanup pass.
cleanupOnce
  :: SessionRegistry
  -> TVar Document
  -> CommandProcessor
  -> NominalDiffTime
  -> IO ()
cleanupOnce registry docVar proc threshold = do
  stale <- findStaleSessions registry threshold
  case stale of
    [] -> pure ()
    _ -> do
      doc <- readTVarIO docVar
      let locks = Map.toList (doc.locks)
      forM_ stale $ \(staleSid, _entry) -> do
        let staleLocks =
              [ (lock, holder)
              | (lock, holder) <- locks
              , holder.sessionId == staleSid
              ]
        forM_ staleLocks $ \(lock, holder) -> do
          putStrLn $ "Releasing stale lock: " <> show lock <> " (session " <> show staleSid <> ")"
          _result <- submitCommand proc holder.userId holder.sessionId (Unlock lock)
          pure ()
      putStrLn $ "Stale lock cleanup: checked " <> show (length stale) <> " expired sessions"
