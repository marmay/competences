-- | File watching with content hashing for cache busting.
--
-- Provides a bracket-style API that watches a file for changes and
-- maintains a TVar with the current content hash.
--
-- Example usage:
--
-- @
-- withHashedFile "static/app.wasm" $ \hashRef -> do
--   hash <- readTVarIO hashRef
--   putStrLn $ "Current hash: " <> Text.unpack hash
--   -- ... run server that reads hashRef on each request ...
-- @
module Competences.Backend.HashedFile
  ( FileHashRef
  , withHashedFile
  , readFileHash
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (void, when)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import System.FSNotify qualified as FS
import System.FilePath (takeDirectory, takeFileName)

-- | Reference to a file's content hash, updated automatically when the file changes.
type FileHashRef = TVar Text

-- | Bracket-style wrapper that watches a file and maintains its content hash.
--
-- * Computes initial hash on startup
-- * Watches for file changes using fsnotify
-- * Debounces rapid changes (500ms) to handle partial writes
-- * Cleans up the file watcher when the action completes
--
-- Returns empty string as hash if the file doesn't exist.
withHashedFile :: FilePath -> (FileHashRef -> IO a) -> IO a
withHashedFile path action = do
  -- Compute initial hash
  initialHash <- computeHash path
  hashRef <- newTVarIO initialHash
  putStrLn $ "File hash initialized: " <> Text.unpack initialHash <> " (" <> path <> ")"

  -- Use bracket to ensure watcher cleanup
  bracket (startWatcher path hashRef) stopWatcher $ \_ ->
    action hashRef

-- | Read the current file hash.
readFileHash :: FileHashRef -> IO Text
readFileHash = readTVarIO

-- | Internal: Start file watcher, returns manager for cleanup
startWatcher :: FilePath -> FileHashRef -> IO FS.WatchManager
startWatcher path hashRef = do
  let dir = takeDirectory path
      filename = takeFileName path

  -- Event ID for debouncing
  eventIdRef <- newIORef (0 :: Int)

  putStrLn $ "Starting file watcher: " <> path

  mgr <- FS.startManager
  void $ FS.watchDir mgr dir (isTargetFile filename) $ \event -> do
    putStrLn $ "File change detected: " <> show event

    -- Assign unique ID and debounce
    myEventId <- atomicModifyIORef' eventIdRef (\n -> (n + 1, n + 1))

    void $ forkIO $ do
      threadDelay 500000  -- 500ms debounce

      currentEventId <- readIORef eventIdRef
      when (myEventId == currentEventId) $ do
        newHash <- computeHash path
        atomically $ writeTVar hashRef newHash
        putStrLn $ "File hash updated: " <> Text.unpack newHash

  pure mgr
  where
    isTargetFile :: String -> FS.Event -> Bool
    isTargetFile name (FS.Modified p _ _) = takeFileName p == name
    isTargetFile name (FS.Added p _ _) = takeFileName p == name
    isTargetFile _ _ = False

-- | Internal: Stop file watcher
stopWatcher :: FS.WatchManager -> IO ()
stopWatcher mgr = do
  putStrLn "Stopping file watcher"
  FS.stopManager mgr

-- | Internal: Compute MD5 hash of file contents (first 8 hex chars)
computeHash :: FilePath -> IO Text
computeHash path = do
  result <- try $ BS.readFile path
  case result of
    Left (_ :: SomeException) -> do
      putStrLn $ "Warning: Could not read file for hashing: " <> path
      pure ""
    Right contents -> do
      let digest :: Digest MD5 = hash contents
          hashHex = decodeUtf8 $ convertToBase Base16 digest
      pure $ Text.take 8 hashHex
