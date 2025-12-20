{-# LANGUAGE QuasiQuotes #-}

-- | Database persistence module for command sourcing and snapshots
--
-- TODO: Add versioning envelope for commands and snapshots before production.
-- This will allow schema evolution and backward compatibility as the
-- application evolves.
module Competences.Backend.Database
  ( -- * Connection pool management
    initPool
  , closePool

    -- * Schema management
  , checkSchemaVersion
  , expectedSchemaVersion

    -- * Database state queries
  , isDatabaseEmpty
  , getMaxGeneration

    -- * Command persistence
  , saveCommand
  , loadCommandsSince

    -- * Snapshot persistence
  , saveSnapshot
  , loadLatestSnapshot
  , shouldTakeSnapshot

    -- * Startup logging
  , logStartup
  , logShutdown
  )
where

import Competences.Command (Command)
import Competences.Document (Document)
import Competences.Document.Id (Id (..))
import Competences.Document.User (UserId)
import Data.Aeson (Value, encode, fromJSON, Result(..))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Pool (Pool, createPool, destroyAllResources, withResource)
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , connectPostgreSQL
  , close
  , execute
  , query
  , query_
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import System.Exit (die)

-- | Expected database schema version
expectedSchemaVersion :: Int
expectedSchemaVersion = 1

-- | Initialize connection pool
--
-- Creates a pool with:
-- - 1 stripe (single resource pool)
-- - 60 second idle timeout
-- - 3 max connections (enough for command writes, snapshot writes, and reads)
initPool :: ByteString -> IO (Pool Connection)
initPool connStr = createPool (connectPostgreSQL connStr) close 1 60 3

-- | Close connection pool
closePool :: Pool Connection -> IO ()
closePool = destroyAllResources

-- | Check schema version matches expected version
--
-- Fails with error message if versions don't match.
checkSchemaVersion :: Pool Connection -> IO ()
checkSchemaVersion pool = withResource pool $ \conn -> do
  rows <- query_ conn [sql|
    SELECT version FROM schema_migrations
    ORDER BY version DESC LIMIT 1
  |]
  case rows of
    [] -> die "No schema migrations found. Please initialize database with schema.sql"
    [Only (version :: Int)] ->
      if version == expectedSchemaVersion
        then pure ()
        else
          die $
            "Schema version mismatch. Expected: "
              <> show expectedSchemaVersion
              <> ", Found: "
              <> show version
              <> ". Please run migrations."
    _ -> die "Multiple schema versions found. Database is in inconsistent state."

-- | Check if database is empty (no commands or snapshots)
isDatabaseEmpty :: Pool Connection -> IO Bool
isDatabaseEmpty pool = withResource pool $ \conn -> do
  [Only commandCount] <- query_ conn [sql|SELECT COUNT(*) FROM commands|] :: IO [Only Int]
  [Only snapshotCount] <- query_ conn [sql|SELECT COUNT(*) FROM snapshots|] :: IO [Only Int]
  pure (commandCount == 0 && snapshotCount == 0)

-- | Get maximum generation number from commands table
--
-- Returns 0 if no commands exist.
getMaxGeneration :: Pool Connection -> IO Int64
getMaxGeneration pool = withResource pool $ \conn -> do
  rows <- query_ conn [sql|SELECT MAX(generation) FROM commands|]
  case rows of
    [Only (Just gen)] -> pure gen
    _ -> pure 0

-- | Save a command to the database
--
-- The generation number is auto-assigned by the database BIGSERIAL.
saveCommand :: Pool Connection -> UserId -> Command -> IO Int64
saveCommand pool userId cmd = withResource pool $ \conn -> do
  commandId <- UUID.nextRandom
  let cmdJson = encode cmd
  [Only generation] <-
    query
      conn
      [sql|
      INSERT INTO commands (command_id, user_id, command_data)
      VALUES (?, ?, ?)
      RETURNING generation
    |]
      (commandId, userId.unId, cmdJson)
  pure generation

-- | Load commands since a given generation (exclusive)
--
-- Returns list of (generation, userId, command) tuples ordered by generation.
loadCommandsSince :: Pool Connection -> Int64 -> IO [(Int64, UserId, Command)]
loadCommandsSince pool sinceGen = withResource pool $ \conn -> do
  rows <-
    query
      conn
      [sql|
      SELECT generation, user_id, command_data
      FROM commands
      WHERE generation > ?
      ORDER BY generation ASC
    |]
      (Only sinceGen)
  pure
    [ (gen, Id userId, cmd)
    | (gen, userId, cmdValue :: Value) <- rows
    , Success cmd <- [fromJSON cmdValue]
    ]

-- | Save a snapshot of the document at a specific generation
saveSnapshot :: Pool Connection -> Document -> Int64 -> IO ()
saveSnapshot pool doc generation = withResource pool $ \conn -> do
  snapshotId <- UUID.nextRandom
  let docJson = encode doc
  _ <-
    execute
      conn
      [sql|
      INSERT INTO snapshots (snapshot_id, generation, document_data)
      VALUES (?, ?, ?)
    |]
      (snapshotId, generation, docJson)
  -- Update metadata for snapshot tracking
  now <- getCurrentTime
  _ <-
    execute
      conn
      [sql|
      UPDATE metadata
      SET value = ?, updated_at = ?
      WHERE key = 'last_snapshot_generation'
    |]
      (show generation :: String, now)
  _ <-
    execute
      conn
      [sql|
      UPDATE metadata
      SET value = ?, updated_at = ?
      WHERE key = 'last_snapshot_time'
    |]
      (show now :: String, now)
  pure ()

-- | Load the latest snapshot and its generation
--
-- Returns Nothing if no snapshots exist.
loadLatestSnapshot :: Pool Connection -> IO (Maybe (Document, Int64))
loadLatestSnapshot pool = withResource pool $ \conn -> do
  rows <-
    query_ conn [sql|
      SELECT generation, document_data
      FROM snapshots
      ORDER BY generation DESC
      LIMIT 1
    |]
  case rows of
    [] -> pure Nothing
    (generation, docValue :: Value) : _ ->
      case fromJSON docValue of
        Error err -> die $ "Failed to decode latest snapshot from database: " <> err
        Success doc -> pure $ Just (doc, generation)

-- | Check if a snapshot should be taken
--
-- Takes snapshot if either:
-- - 25 or more commands since last snapshot
-- - 15 minutes since last snapshot AND at least 1 command since last snapshot
shouldTakeSnapshot :: Pool Connection -> Int64 -> IO Bool
shouldTakeSnapshot pool currentGeneration = withResource pool $ \conn -> do
  [Only lastSnapGenText] <-
    query_ conn [sql|
      SELECT value FROM metadata WHERE key = 'last_snapshot_generation'
    |]
  [Only lastSnapTimeText] <-
    query_ conn [sql|
      SELECT value FROM metadata WHERE key = 'last_snapshot_time'
    |]

  let lastSnapGen = read lastSnapGenText :: Int64
  let commandsSince = currentGeneration - lastSnapGen

  -- Check if 25 commands have passed
  if commandsSince >= 25
    then pure True
    else do
      -- Check if 15 minutes have passed and at least 1 command
      now <- getCurrentTime
      let lastSnapTime = read lastSnapTimeText :: UTCTime
      let minutesSince = realToFrac (now `diffUTCTime` lastSnapTime) / 60 :: Double
      pure (minutesSince >= 15 && commandsSince > 0)

-- | Log backend startup to startup_log table
logStartup
  :: Pool Connection
  -> UUID
  -- ^ Instance ID
  -> Int64
  -- ^ Initial generation
  -> Bool
  -- ^ Was init document provided?
  -> Maybe Text
  -- ^ Optional metadata (JSON)
  -> IO ()
logStartup pool instanceId initialGen initDocProvided metadata = withResource pool $ \conn -> do
  _ <-
    execute
      conn
      [sql|
      INSERT INTO startup_log
        (instance_id, schema_version, initial_generation, init_document_provided, metadata)
      VALUES (?, ?, ?, ?, ?::jsonb)
    |]
      (instanceId, expectedSchemaVersion, initialGen, initDocProvided, metadata)
  pure ()

-- | Log backend shutdown
logShutdown :: Pool Connection -> UUID -> IO ()
logShutdown pool instanceId = withResource pool $ \conn -> do
  now <- getCurrentTime
  _ <-
    execute
      conn
      [sql|
      UPDATE startup_log
      SET stopped_at = ?
      WHERE instance_id = ? AND stopped_at IS NULL
    |]
      (now, instanceId)
  pure ()
