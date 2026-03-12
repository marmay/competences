-- | Shared in-memory cache of recent commands for efficient broadcast.
--
-- Replaces per-client broadcast queues. All sender threads read from the same
-- log, filtering by their user's audience. Uses STM for signaling new entries.
module Competences.Backend.CommandLog
  ( CommandLog (..)
  , CommandEntry (..)
  , newCommandLog
  , appendCommand
  , readCommandsSince
  , lookupCommandGeneration
  , getLatestCommandId
  , waitForNewCommands
  )
where

import Competences.Backend.Database qualified as DB
import Competences.Command (Command)
import Competences.Command.Audience (CommandAudience (..), audienceRecipients)
import Competences.Document (UserRole (..))
import Competences.Document.User (UserId)
import Competences.Protocol (CommandId)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Database.PostgreSQL.Simple (Connection)

-- | A single entry in the command log.
data CommandEntry = CommandEntry
  { commandId :: !CommandId
  , generation :: !Int64
  , command :: !Command
  , audience :: !CommandAudience
  , recipients :: ![UserId]
  }

-- | Shared command log. All sender threads read from this.
data CommandLog = CommandLog
  { entries :: !(TVar (Seq CommandEntry))
  -- ^ Recent commands, newest last
  , latestGeneration :: !(TVar Int64)
  -- ^ Signals sender threads via STM retry
  , latestCommandId :: !(TVar (Maybe CommandId))
  -- ^ The ID of the most recently appended command
  , dbPool :: !(Pool Connection)
  -- ^ Fallback for older commands not in the cache
  }

-- | Maximum number of entries to keep in the in-memory cache.
maxCacheSize :: Int
maxCacheSize = 500

-- | Number of recent commands to load on startup.
startupCacheSize :: Int
startupCacheSize = 50

-- | Create a new CommandLog, primed with recent commands from the database.
newCommandLog :: Pool Connection -> Int64 -> IO CommandLog
newCommandLog pool currentGen = do
  -- Load recent commands to populate the cache
  recentCmds <- DB.loadRecentCommands pool startupCacheSize
  let entries = Seq.fromList
        [ CommandEntry
            { commandId = cmdId
            , generation = gen
            , command = cmd
            , audience = aud
            , recipients = audienceRecipients aud
            }
        | (cmdId, gen, cmd, aud) <- recentCmds
        ]
      latestCmdId = case recentCmds of
        [] -> Nothing
        _ -> let (cmdId, _, _, _) = last recentCmds in Just cmdId
  entriesVar <- newTVarIO entries
  genVar <- newTVarIO currentGen
  cmdIdVar <- newTVarIO latestCmdId
  putStrLn $ "CommandLog initialized with " <> show (Seq.length entries) <> " cached entries"
  pure $ CommandLog entriesVar genVar cmdIdVar pool

-- | Append a new command entry to the log.
-- Updates latestGeneration (waking sender threads) and evicts old entries.
appendCommand :: CommandLog -> CommandEntry -> IO ()
appendCommand cl entry = atomically $ do
  entries' <- readTVar cl.entries
  let entries'' = entries' Seq.|> entry
      -- Evict oldest entries if cache is too large
      trimmed
        | Seq.length entries'' > maxCacheSize =
            Seq.drop (Seq.length entries'' - maxCacheSize) entries''
        | otherwise = entries''
  writeTVar cl.entries trimmed
  writeTVar cl.latestGeneration entry.generation
  writeTVar cl.latestCommandId (Just entry.commandId)

-- | Read commands from the log since a given generation, filtered for a user.
--
-- First tries the in-memory cache. If the generation is older than the cache,
-- falls back to the database.
-- Returns list of (CommandId, Command) and the new generation position.
readCommandsSince :: CommandLog -> UserRole -> UserId -> Int64 -> IO (Int64, [(CommandId, Command)])
readCommandsSince cl role uid sinceGen = do
  -- Try to read from cache first
  (entries', latestGen) <- atomically $ do
    es <- readTVar cl.entries
    gen <- readTVar cl.latestGeneration
    pure (es, gen)

  -- Check if the cache covers our range
  let cacheStart = case Seq.viewl entries' of
        Seq.EmptyL -> sinceGen + 1  -- Empty cache covers nothing
        e Seq.:< _ -> e.generation

  if sinceGen < cacheStart && not (Seq.null entries')
    then do
      -- Cache doesn't cover this range, fall back to DB
      dbCmds <- DB.loadCommandsForUser cl.dbPool role uid sinceGen
      case dbCmds of
        [] -> pure (sinceGen, [])
        _ -> let (_lastCmdId, lastGen, _) = last dbCmds
              in pure (lastGen, [(cid, cmd) | (cid, _gen, cmd) <- dbCmds])
    else do
      -- Filter from cache
      let relevant = Seq.filter (\e -> e.generation > sinceGen) entries'
          filtered = Seq.filter (isVisibleTo role uid) relevant
          cmds = [(e.commandId, e.command) | e <- toList filtered]
      pure (latestGen, cmds)

-- | Look up the generation number for a given CommandId in the cache.
-- Falls back to the database if not found in cache.
lookupCommandGeneration :: CommandLog -> CommandId -> IO (Maybe Int64)
lookupCommandGeneration cl cmdId = do
  entries' <- atomically $ readTVar cl.entries
  case Seq.findIndexL (\e -> e.commandId == cmdId) entries' of
    Just idx -> pure $ Just (Seq.index entries' idx).generation
    Nothing -> DB.lookupCommandGeneration cl.dbPool cmdId

-- | Get the latest command ID from the log.
getLatestCommandId :: CommandLog -> IO (Maybe CommandId)
getLatestCommandId cl = atomically $ readTVar cl.latestCommandId

-- | Block (via STM retry) until latestGeneration exceeds the given generation.
-- Used by sender threads to wait for new commands.
waitForNewCommands :: CommandLog -> Int64 -> STM ()
waitForNewCommands cl gen = do
  latest <- readTVar cl.latestGeneration
  if latest > gen then pure () else retry

-- | Check if a command entry is visible to a specific user based on audience.
isVisibleTo :: UserRole -> UserId -> CommandEntry -> Bool
isVisibleTo Teacher uid e = case e.audience of
  AudienceAll -> True
  AudienceTeachers -> True
  AudienceTeachersAnd _ -> True
  AudienceOnly recipients -> uid `elem` recipients
isVisibleTo Student uid e = case e.audience of
  AudienceAll -> True
  AudienceTeachers -> False
  AudienceTeachersAnd recipients -> uid `elem` recipients
  AudienceOnly recipients -> uid `elem` recipients

-- | Convert Seq to list (helper)
toList :: Seq a -> [a]
toList = foldr (:) []
