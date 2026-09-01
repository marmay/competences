{-# LANGUAGE QuasiQuotes #-}

-- | Database persistence module for command sourcing and snapshots
--
-- Commands and snapshots are stored with versioning envelopes to support
-- schema evolution and backward compatibility.
module Competences.Backend.Database
  ( -- * Connection pool management
    initPool
  , closePool

    -- * Schema management
  , runMigrations
  , expectedSchemaVersion

    -- * Database state queries
  , isDatabaseEmpty
  , getMaxGeneration
  , getLatestCommandId

    -- * Command persistence
  , saveCommand
  , saveCommandWithAudience
  , loadCommandsSince
  , loadCommandsForUser
  , countCommandsForUser
  , lookupCommandGeneration

    -- * Snapshot persistence
  , saveSnapshot
  , loadLatestSnapshot
  , shouldTakeSnapshot
  , pruneSnapshots

    -- * Startup logging
  , logStartup
  , logShutdown
  )
where

import Competences.Backend.Envelope
  ( CommandEnvelope (..)
  , unwrapCommand
  , unwrapSnapshot
  , wrapCommand
  , wrapSnapshot
  )
import Competences.Command (Command, CommandContext (..), handleCommand)
import Competences.Document.Session (SessionId)
import Competences.Command.Audience (CommandAudience, audienceRecipients, audienceToText)
import Competences.Document (Document, UserRole (..))
import Competences.Document.Id (Id (..))
import Competences.Document.User (UserId)
import Competences.Protocol (CommandId)
import Control.Monad (forM_, when)
import Data.Aeson (Value, eitherDecodeStrict, encode, fromJSON, Result(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Pool (Pool, newPool, defaultPoolConfig, setNumStripes, destroyAllResources, withResource)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.ByteString.Char8 qualified as BS
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , connectPostgreSQL
  , close
  , execute
  , execute_
  , query
  , query_
  , withTransaction
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (..))
import System.Exit (ExitCode (..), die)
import System.Process (readProcessWithExitCode)

-- | Expected database schema version
expectedSchemaVersion :: Int
expectedSchemaVersion = 5

-- | Initialize connection pool
--
-- Creates a pool with:
-- - 1 stripe (single resource pool)
-- - 60 second idle timeout
-- - 3 max connections (enough for command writes, snapshot writes, and reads)
initPool :: ByteString -> IO (Pool Connection)
initPool connStr =
  newPool $
    setNumStripes (Just 1) $
      defaultPoolConfig
        (connectPostgreSQL connStr)
        close
        60 -- idle timeout in seconds
        3 -- max resources per stripe

-- | Close connection pool
closePool :: Pool Connection -> IO ()
closePool = destroyAllResources

-- | Embedded schema migrations.
--
-- Each entry is (version, description, sql). Migration 1 is the initial schema
-- (applied by schema.sql on first deploy) and is never run by the migration runner.
-- Only migrations with version > current database version are applied.
migrations :: [(Int, String, ByteString)]
migrations =
  [
    ( 2
    , "Command audience tracking for incremental sync"
    , BS.intercalate "\n"
        [ "ALTER TABLE commands ADD COLUMN audience TEXT NOT NULL DEFAULT 'all';"
        , ""
        , "CREATE TABLE command_recipients ("
        , "  generation BIGINT NOT NULL REFERENCES commands(generation),"
        , "  user_id UUID NOT NULL,"
        , "  PRIMARY KEY (generation, user_id)"
        , ");"
        , "CREATE INDEX idx_command_recipients_user_gen ON command_recipients(user_id, generation);"
        ]
    )
  , ( 3
    , "Add protected flag for snapshot garbage collection"
    , "ALTER TABLE snapshots ADD COLUMN protected BOOLEAN NOT NULL DEFAULT FALSE;"
    )
  , ( 4
    , "Convert snapshot document_data from JSONB to TEXT for byte-exact comparison"
    , BS.intercalate "\n"
        [ "ALTER TABLE snapshots ALTER COLUMN document_data TYPE TEXT USING document_data::text;"
        , ""
        , "INSERT INTO commands (command_id, user_id, command_data)"
        , "VALUES ("
        , "  gen_random_uuid(),"
        , "  '00000000-0000-0000-0000-000000000000',"
        , "  '{\"payload\":{\"contents\":{\"tag\":\"SortAssignmentTasksByIdentifier\"},\"tag\":\"Migration\"},\"userId\":\"00000000-0000-0000-0000-000000000000\",\"version\":1}'"
        , ");"
        ]
    )
  , ( 5
    , "Add UNIQUE constraint on snapshots.generation to prevent duplicates"
    , BS.intercalate "\n"
        [ "DELETE FROM snapshots WHERE id NOT IN ("
        , "  SELECT MAX(id) FROM snapshots GROUP BY generation"
        , ");"
        , ""
        , "DROP INDEX IF EXISTS idx_snapshots_generation;"
        , "ALTER TABLE snapshots ADD CONSTRAINT snapshots_generation_unique UNIQUE (generation);"
        ]
    )
  ]

-- | Run pending database migrations automatically.
--
-- Reads the current schema version, applies any pending migrations in order
-- (each in its own transaction), and verifies the final version matches
-- 'expectedSchemaVersion'. Before applying migrations, creates a pg_dump
-- backup. Aborts startup if the database is not initialized or if backup fails.
runMigrations :: Pool Connection -> ByteString -> FilePath -> FilePath -> IO ()
runMigrations pool connStr backupDir pgDumpPath = withResource pool $ \conn -> do
  -- Get current schema version
  currentVersion <- getCurrentVersion conn

  let pending = filter (\(v, _, _) -> v > currentVersion) migrations
  if null pending
    then putStrLn $ "Database schema is up to date (version " <> show currentVersion <> ")"
    else do
      -- Create backup before applying migrations
      let nextVersion = case pending of ((v, _, _) : _) -> v; [] -> error "unreachable"
      let backupFile = backupDir <> "/backup-before-migration-" <> show nextVersion <> ".sql"
      putStrLn $ "Creating database backup: " <> backupFile
      (exitCode, _stdout, stderr) <-
        readProcessWithExitCode pgDumpPath ["--dbname=" <> BS.unpack connStr, "-f", backupFile] ""
      case exitCode of
        ExitSuccess -> putStrLn "Backup created successfully"
        ExitFailure code ->
          die $
            "pg_dump failed (exit code " <> show code <> "): " <> stderr
              <> "\nAborting migrations. Fix pg_dump or apply migrations manually."

      -- Apply each pending migration in its own transaction
      mapM_ (applyMigration conn) pending

  -- Verify final version
  finalVersion <- getCurrentVersion conn
  if finalVersion == expectedSchemaVersion
    then pure ()
    else
      die $
        "Schema version mismatch after migrations. Expected: "
          <> show expectedSchemaVersion
          <> ", Found: "
          <> show finalVersion

-- | Get the current schema version from the database.
getCurrentVersion :: Connection -> IO Int
getCurrentVersion conn = do
  rows <- query_ conn [sql|
    SELECT version FROM schema_migrations
    ORDER BY version DESC LIMIT 1
  |]
  case rows of
    [] -> die "No schema migrations found. Please initialize database with schema.sql"
    [Only version] -> pure version
    _ -> die "Unexpected result querying schema_migrations"

-- | Apply a single migration in a transaction.
applyMigration :: Connection -> (Int, String, ByteString) -> IO ()
applyMigration conn (version, description, migrationSql) = do
  putStrLn $ "Applying migration " <> show version <> ": " <> description
  _ <- withTransaction conn $ do
    _ <- execute_ conn (Query migrationSql)
    execute
      conn
      [sql|INSERT INTO schema_migrations (version, description) VALUES (?, ?)|]
      (version, description)
  putStrLn $ "Migration " <> show version <> " applied successfully"

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

-- | Get the command_id of the latest command.
--
-- Returns Nothing if no commands exist.
getLatestCommandId :: Pool Connection -> IO (Maybe CommandId)
getLatestCommandId pool = withResource pool $ \conn -> do
  rows <-
    query_ conn [sql|
      SELECT command_id FROM commands ORDER BY generation DESC LIMIT 1
    |] :: IO [Only UUID]
  case rows of
    [Only uuid] -> pure (Just (Id uuid))
    _ -> pure Nothing

-- | Save a command to the database
--
-- The command is wrapped in a versioned envelope before storage.
-- The generation number is auto-assigned by the database BIGSERIAL.
-- Uses default audience 'all'.
-- Returns (CommandId, generation).
saveCommand :: Pool Connection -> CommandContext -> Command -> IO (CommandId, Int64)
saveCommand pool ctx cmd = withResource pool $ \conn -> do
  commandId <- UUID.nextRandom
  let envelope = wrapCommand ctx cmd
  let envelopeJson = encode envelope
  [Only generation] <-
    query
      conn
      [sql|
      INSERT INTO commands (command_id, user_id, command_data)
      VALUES (?, ?, ?)
      RETURNING generation
    |]
      (commandId, ctx.userId.unId, envelopeJson)
  pure (Id commandId, generation)

-- | Save a command with explicit audience tracking.
--
-- Records the audience classification and any specific recipient user IDs
-- in the command_recipients table for efficient incremental sync queries.
-- The CommandId is pre-generated by the caller (so it can be shared with
-- in-memory broadcast before persistence).
-- Returns the generation number assigned by the database.
saveCommandWithAudience :: Pool Connection -> CommandId -> CommandContext -> Command -> CommandAudience -> IO Int64
saveCommandWithAudience pool cmdId ctx cmd audience = withResource pool $ \conn -> do
  let envelope = wrapCommand ctx cmd
  let envelopeJson = encode envelope
  let audienceText = audienceToText audience
  [Only generation] <-
    query
      conn
      [sql|
      INSERT INTO commands (command_id, user_id, command_data, audience)
      VALUES (?, ?, ?, ?)
      RETURNING generation
    |]
      (cmdId.unId, ctx.userId.unId, envelopeJson, audienceText)
  -- Insert specific recipients if needed
  let recipients = audienceRecipients audience
  forM_ recipients $ \recipientId ->
    execute
      conn
      [sql|
      INSERT INTO command_recipients (generation, user_id)
      VALUES (?, ?)
    |]
      (generation, recipientId.unId)
  pure generation

-- | Load commands since a given generation (exclusive)
--
-- Returns list of (generation, userId, sessionId, command) tuples ordered
-- by generation. Commands are unwrapped from versioned envelopes, with
-- migrations applied if needed. The original 'SessionId' from the envelope
-- is preserved so a subsequent replay can match the snapshot's lock
-- holders under 'doRelease''s strict session check.
loadCommandsSince :: Pool Connection -> Int64 -> IO [(Int64, UserId, SessionId, Command)]
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
      (Only sinceGen) ::
      IO [(Int64, UUID, Value)]
  pure
    [ (gen, envelope.userId, envelope.sessionId, cmd)
    | (gen, _userId, envelopeValue) <- rows
    , Success envelope <- [fromJSON envelopeValue]
    , Right cmd <- [unwrapCommand envelope]
    ]

-- | Load commands since a given generation that are relevant for a specific user.
--
-- For teachers: includes 'all', 'teachers', 'teachers_and_recipients',
-- and 'recipients' (only if the user is a specific recipient).
-- For students: includes 'all', and 'teachers_and_recipients'/'recipients'
-- only if the user is a specific recipient.
--
-- Returns (CommandId, generation, UserId, Command) tuples ordered by generation.
loadCommandsForUser :: Pool Connection -> UserRole -> UserId -> Int64 -> IO [(CommandId, Int64, CommandContext, Command)]
loadCommandsForUser pool role userId sinceGen = withResource pool $ \conn -> do
  rows <- case role of
    Teacher ->
      query
        conn
        [sql|
        SELECT c.command_id, c.generation, c.command_data
        FROM commands c
        LEFT JOIN command_recipients cr ON c.generation = cr.generation AND cr.user_id = ?
        WHERE c.generation > ?
          AND (c.audience IN ('all', 'teachers', 'teachers_and_recipients')
               OR (c.audience = 'recipients' AND cr.user_id IS NOT NULL))
        ORDER BY c.generation
      |]
        (userId.unId, sinceGen) ::
        IO [(UUID, Int64, Value)]
    Student ->
      query
        conn
        [sql|
        SELECT c.command_id, c.generation, c.command_data
        FROM commands c
        LEFT JOIN command_recipients cr ON c.generation = cr.generation AND cr.user_id = ?
        WHERE c.generation > ?
          AND (c.audience = 'all'
               OR (c.audience IN ('teachers_and_recipients', 'recipients') AND cr.user_id IS NOT NULL))
        ORDER BY c.generation
      |]
        (userId.unId, sinceGen) ::
        IO [(UUID, Int64, Value)]
  pure
    [ (Id cmdId, gen, CommandContext envelope.userId envelope.sessionId, cmd)
    | (cmdId, gen, envelopeValue) <- rows
    , Success envelope <- [fromJSON envelopeValue]
    , Right cmd <- [unwrapCommand envelope]
    ]

-- | Count commands since a given generation that are relevant for a specific user.
countCommandsForUser :: Pool Connection -> UserRole -> UserId -> Int64 -> IO Int
countCommandsForUser pool role userId sinceGen = withResource pool $ \conn -> do
  [Only count] <- case role of
    Teacher ->
      query
        conn
        [sql|
        SELECT COUNT(*)
        FROM commands c
        LEFT JOIN command_recipients cr ON c.generation = cr.generation AND cr.user_id = ?
        WHERE c.generation > ?
          AND (c.audience IN ('all', 'teachers', 'teachers_and_recipients')
               OR (c.audience = 'recipients' AND cr.user_id IS NOT NULL))
      |]
        (userId.unId, sinceGen)
    Student ->
      query
        conn
        [sql|
        SELECT COUNT(*)
        FROM commands c
        LEFT JOIN command_recipients cr ON c.generation = cr.generation AND cr.user_id = ?
        WHERE c.generation > ?
          AND (c.audience = 'all'
               OR (c.audience IN ('teachers_and_recipients', 'recipients') AND cr.user_id IS NOT NULL))
      |]
        (userId.unId, sinceGen)
  pure count

-- | Look up the generation for a command by its UUID.
lookupCommandGeneration :: Pool Connection -> CommandId -> IO (Maybe Int64)
lookupCommandGeneration pool cmdId = withResource pool $ \conn -> do
  rows <-
    query
      conn
      [sql|
      SELECT generation FROM commands WHERE command_id = ?
    |]
      (Only cmdId.unId) ::
      IO [Only Int64]
  case rows of
    [Only gen] -> pure (Just gen)
    _ -> pure Nothing

-- | Save a snapshot of the document at a specific generation
--
-- The document is wrapped in a versioned envelope before storage.
-- Uses ON CONFLICT DO NOTHING to silently skip if a snapshot already exists
-- at this generation (prevents duplicates from recovery + graceful shutdown).
saveSnapshot :: Pool Connection -> Document -> Int64 -> IO ()
saveSnapshot pool doc generation = withResource pool $ \conn -> do
  snapshotId <- UUID.nextRandom
  let envelope = wrapSnapshot doc
  let envelopeText = decodeUtf8 (LBS.toStrict (encode envelope))
  rowsAffected <-
    execute
      conn
      [sql|
      INSERT INTO snapshots (snapshot_id, generation, document_data)
      VALUES (?, ?, ?)
      ON CONFLICT (generation) DO NOTHING
    |]
      (snapshotId, generation, envelopeText)
  -- Only update metadata if the snapshot was actually inserted
  when (rowsAffected > 0) $ do
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
-- The snapshot is unwrapped from a versioned envelope, with migrations applied if needed.
-- Also returns any migration commands that must be persisted for replay safety.
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
    (generation, envelopeText :: Text) : _ ->
      case eitherDecodeStrict (encodeUtf8 envelopeText) of
        Left err -> die $ "Failed to parse snapshot JSON: " <> err
        Right envelope ->
          case unwrapSnapshot envelope of
            Left err -> die $ "Failed to unwrap snapshot: " <> T.unpack err
            Right doc -> pure $ Just (doc, generation)

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

-- | Prune old snapshots using age-based thresholds with reproducibility verification.
--
-- Walks snapshots oldest→newest, keeping the oldest and latest snapshots unconditionally.
-- Candidates in between are pruned if they can be reproduced by replaying commands from
-- the previous kept snapshot. Non-reproducible snapshots are permanently protected.
--
-- Only loads document_data on-demand for actual pruning candidates, keeping memory usage
-- bounded to at most 2 Documents at a time (anchor + candidate).
--
-- Returns the number of deleted snapshots.
pruneSnapshots :: Pool Connection -> IO Int
pruneSnapshots pool = withResource pool $ \conn -> do
  -- Load only metadata — no document_data
  rows <-
    query_ conn [sql|
      SELECT id, generation, created_at, protected
      FROM snapshots
      ORDER BY generation ASC
    |] :: IO [(Int64, Int64, UTCTime, Bool)]
  case rows of
    [] -> pure 0
    [_] -> pure 0
    -- Need at least 3 snapshots (oldest + candidate + newest) to prune anything
    (oldest : middle@(_ : _)) -> do
      now <- getCurrentTime
      let candidates = init middle -- everything except newest (always kept)
      let (_, oldestGen, _, _) = oldest
      deleted <- walkCandidates conn now Nothing oldestGen candidates
      let remaining = length rows - deleted
      when (deleted > 0) $
        putStrLn $
          "Snapshot GC: pruned " <> show deleted <> " snapshot(s), "
            <> show remaining <> " remaining"
      pure deleted

-- | Walk candidate snapshots, verifying and pruning where possible.
-- Carries forward the last kept generation and lazily loads the anchor document
-- only when needed for replay verification.
--
-- The @Maybe Document@ parameter is the cached anchor document. It is @Nothing@
-- when the anchor has not been loaded yet (deferred until a prune candidate is found).
walkCandidates :: Connection -> UTCTime -> Maybe Document -> Int64 -> [(Int64, Int64, UTCTime, Bool)] -> IO Int
walkCandidates _conn _now _lastDoc _lastGen [] = pure 0
walkCandidates conn now lastDoc lastGen ((snapId, candidateGen, createdAt, isProtected) : rest) = do
  let age = now `diffUTCTime` createdAt
      commandGap = candidateGen - lastGen
  if isProtected
    then do
      -- Protected: must keep, update carried state (discard cached doc)
      walkCandidates conn now Nothing candidateGen rest
    else if age < twoDays
      then do
        -- Too young: always keep (discard cached doc)
        walkCandidates conn now Nothing candidateGen rest
      else if not (isPruneCandidate commandGap)
        then do
          -- Gap too large: keep to limit replay distance (discard cached doc)
          walkCandidates conn now Nothing candidateGen rest
        else do
          -- Candidate for pruning: need anchor document for replay
          anchorResult <- case lastDoc of
            Just doc -> pure (Right doc)
            Nothing -> loadSnapshotDocumentById conn lastGen
          case anchorResult of
            Left err -> do
              putStrLn $ "Snapshot GC: failed to load anchor snapshot at gen " <> show lastGen <> ": " <> err
              -- Can't verify — skip this candidate and continue with it as anchor
              walkCandidates conn now Nothing candidateGen rest
            Right anchorDoc -> do
              -- Load commands between anchor and candidate
              cmds <-
                query
                  conn
                  [sql|
                  SELECT user_id, command_data
                  FROM commands
                  WHERE generation > ? AND generation <= ?
                  ORDER BY generation ASC
                |]
                  (lastGen, candidateGen) :: IO [(UUID, Value)]
              let parsedCmds =
                    [ (Id uid, envelope.sessionId, cmd)
                    | (uid, envelopeValue) <- cmds
                    , Success envelope <- [fromJSON envelopeValue]
                    , Right cmd <- [unwrapCommand envelope]
                    ]
              -- Load the candidate's raw TEXT for byte-exact comparison
              candidateTextResult <- loadSnapshotTextById conn candidateGen
              case candidateTextResult of
                Left err -> do
                  putStrLn $ "Snapshot GC: failed to load candidate snapshot at gen " <> show candidateGen <> ": " <> err
                  -- Can't verify — skip and continue
                  walkCandidates conn now Nothing candidateGen rest
                Right candidateText ->
                  case replayCommandsForGC anchorDoc parsedCmds of
                    Left err -> do
                      putStrLn $
                        "Snapshot GC WARNING: replay failed for gen " <> show candidateGen
                          <> ": " <> T.unpack err <> " — marking as protected"
                      _ <- execute conn [sql|UPDATE snapshots SET protected = TRUE WHERE id = ?|] (Only snapId)
                      walkCandidates conn now Nothing candidateGen rest
                    Right replayedDoc -> do
                      let replayedText = decodeUtf8 (LBS.toStrict (encode (wrapSnapshot replayedDoc)))
                      if replayedText == candidateText
                        then do
                          -- Reproducible: safe to delete
                          _ <- execute conn [sql|DELETE FROM snapshots WHERE id = ?|] (Only snapId)
                          -- Don't update lastGen — reuse anchor doc for next candidate
                          deletedRest <- walkCandidates conn now (Just anchorDoc) lastGen rest
                          pure (1 + deletedRest)
                        else do
                          putStrLn $
                            "Snapshot GC WARNING: document mismatch at gen " <> show candidateGen
                              <> " — marking as protected"
                          _ <- execute conn [sql|UPDATE snapshots SET protected = TRUE WHERE id = ?|] (Only snapId)
                          -- No deserialized candidate doc available; next iteration loads lazily
                          walkCandidates conn now Nothing candidateGen rest

-- | Is the snapshot a candidate for pruning given the command gap from the
-- previous kept snapshot?  We cap at 500 to limit replay distance on startup.
isPruneCandidate :: Int64 -> Bool
isPruneCandidate commandGap = commandGap < 500

twoDays :: NominalDiffTime
twoDays = 2 * 24 * 3600

-- | Load a single snapshot's document by generation, using a point query.
-- Reads TEXT, parses JSON, unwraps envelope to Document.
loadSnapshotDocumentById :: Connection -> Int64 -> IO (Either String Document)
loadSnapshotDocumentById conn gen = do
  rows <-
    query conn [sql|
      SELECT document_data FROM snapshots WHERE generation = ?
    |] (Only gen) :: IO [Only Text]
  case rows of
    [Only t] ->
      case eitherDecodeStrict (encodeUtf8 t) of
        Left err -> pure $ Left $ "Failed to decode envelope: " <> err
        Right envelope ->
          case unwrapSnapshot envelope of
            Left err -> pure $ Left $ "Failed to unwrap snapshot: " <> T.unpack err
            Right doc -> pure $ Right doc
    [] -> pure $ Left $ "No snapshot found at generation " <> show gen
    _ -> pure $ Left $ "Multiple snapshots at generation " <> show gen

-- | Load a single snapshot's raw TEXT by generation, for byte-exact comparison.
loadSnapshotTextById :: Connection -> Int64 -> IO (Either String Text)
loadSnapshotTextById conn gen = do
  rows <-
    query conn [sql|
      SELECT document_data FROM snapshots WHERE generation = ?
    |] (Only gen) :: IO [Only Text]
  case rows of
    [Only t] -> pure (Right t)
    [] -> pure $ Left $ "No snapshot found at generation " <> show gen
    _ -> pure $ Left $ "Multiple snapshots at generation " <> show gen

-- | Replay a sequence of commands for GC verification.
-- Returns the resulting document, or Left on first failure. Uses each
-- command's original 'SessionId' so 'doRelease' matches the snapshot's
-- lock holders exactly.
replayCommandsForGC :: Document -> [(UserId, SessionId, Command)] -> Either Text Document
replayCommandsForGC doc [] = Right doc
replayCommandsForGC doc ((userId, sessionId, cmd) : rest) =
  case handleCommand (CommandContext userId sessionId) cmd doc of
    Left err -> Left err
    Right (doc', _) -> replayCommandsForGC doc' rest

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
