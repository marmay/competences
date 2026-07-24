module Main where

import Competences.Backend.CAS (newCAS)
import Competences.Backend.CommandProcessor (startProcessor)
import Competences.Backend.Database qualified as DB
import Competences.Backend.HashedFile (withHashedFiles)
import Competences.Backend.HTTP (appAPI, server)
import Competences.Backend.SecurityConfig (loadSecurityConfig)
import Competences.Backend.SessionRegistry qualified as SR
import Competences.Backend.Shell (ShellHashes(..))
import Competences.Backend.StaleLockCleanup qualified as SLC
import Competences.Backend.State (AppState (..), initAppState, initRestState)
import Competences.Backend.WebSocket (wsHandler)
import Competences.Command (Command (..), CommandContext (..), MigrationCommand (..), handleCommand)
import Competences.Document.Session (SessionId, legacySessionId)
import Competences.Document (Document (..), emptyDocument)
import Competences.Document.Id (Id (..))
import Competences.Document.User qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import Control.Exception (finally)
import Control.Monad (foldM, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Handler.Warp (run)
import Options.Applicative qualified as Opt
import Servant (serve)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Numeric (readOct, showOct)
import System.Exit (die)
import System.IO (hFlush, stdout)
import System.Posix.Types (FileMode)

-- | Command-line options
data Options = Options
  { port :: !Int
  , dbConnString :: !ByteString
  , securityConfigPath :: !FilePath
  , staticDir :: !FilePath
  , casDir :: !FilePath
  , casFileMode :: !FileMode
  , backupDir :: !FilePath
  , pgDumpPath :: !FilePath
  , ensureTeacherO365 :: !(Maybe String)
  }

-- | Parse command-line options
optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.option
      Opt.auto
      ( Opt.long "port"
          <> Opt.short 'p'
          <> Opt.metavar "PORT"
          <> Opt.help "Port to listen on"
      )
    <*> ( BS.pack
            <$> Opt.strOption
              ( Opt.long "database"
                  <> Opt.short 'd'
                  <> Opt.metavar "CONNSTRING"
                  <> Opt.help "PostgreSQL connection string"
              )
        )
    <*> Opt.strOption
      ( Opt.long "security-config"
          <> Opt.short 'c'
          <> Opt.metavar "FILE"
          <> Opt.help "Configuration file (JSON) containing secrets"
      )
    <*> Opt.strOption
      ( Opt.long "static"
          <> Opt.short 's'
          <> Opt.metavar "DIR"
          <> Opt.help "Static files directory"
      )
    <*> Opt.strOption
      ( Opt.long "cas-dir"
          <> Opt.metavar "DIR"
          <> Opt.value "./files"
          <> Opt.showDefault
          <> Opt.help "Content-addressable store directory for uploaded files"
      )
    <*> Opt.option
      (Opt.eitherReader parseOctalMode)
      ( Opt.long "cas-file-mode"
          <> Opt.metavar "OCTAL"
          <> Opt.value 0o640
          <> Opt.showDefaultWith (\m -> "0o" <> showOct' m)
          <> Opt.help "File mode (octal) applied to stored CAS blobs; e.g. 640 (rw-r-----) or 644 (rw-r--r--)"
      )
    <*> Opt.strOption
      ( Opt.long "backup-dir"
          <> Opt.metavar "DIR"
          <> Opt.value "."
          <> Opt.showDefault
          <> Opt.help "Directory for database migration backups"
      )
    <*> Opt.strOption
      ( Opt.long "pg-dump"
          <> Opt.metavar "PATH"
          <> Opt.value "pg_dump"
          <> Opt.showDefault
          <> Opt.help "Path to pg_dump executable"
      )
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "ensure-teacher-o365"
              <> Opt.metavar "EMAIL"
              <> Opt.help "Ensure a Teacher user exists for this O365 email"
          )
      )

-- | Parser info with program description
parseOctalMode :: String -> Either String FileMode
parseOctalMode s = case readOct s of
  [(n, "")] | n >= 0 && n <= 0o7777 -> Right (fromIntegral (n :: Int))
  _ -> Left $ "invalid octal mode (expected e.g. 640): " <> s

showOct' :: FileMode -> String
showOct' m = showOct (fromIntegral m :: Int) ""

optsParserInfo :: Opt.ParserInfo Options
optsParserInfo =
  Opt.info
    (optionsParser Opt.<**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Competences Backend Server with PostgreSQL persistence"
        <> Opt.header "competences-backend - A collaborative competence tracking system"
    )

main :: IO ()
main = do
  -- Parse command line arguments
  opts <- Opt.execParser optsParserInfo

  -- Load configuration (secrets) from file
  putStrLn $ "Loading security configuration from: " <> opts.securityConfigPath
  securityConfig <- loadSecurityConfig opts.securityConfigPath

  putStrLn ""
  putStrLn "Competences Backend Server"
  putStrLn "=========================="
  putStrLn $ "Port: " <> show opts.port
  putStrLn $ "Database: " <> BS.unpack opts.dbConnString
  putStrLn $ "Static directory: " <> opts.staticDir
  putStrLn $ "CAS directory: " <> opts.casDir
  putStrLn ""

  -- Initialize database connection pool
  putStrLn "Initializing database connection pool..."
  pool <- DB.initPool opts.dbConnString

  -- Run pending database migrations (if any)
  putStrLn "Checking database schema..."
  DB.runMigrations pool opts.dbConnString opts.backupDir opts.pgDumpPath

  -- Generate instance ID for startup logging
  instanceId <- UUID.nextRandom

  -- Load document from database (or start with empty document)
  putStrLn "Loading document from database..."
  mSnapshot <- DB.loadLatestSnapshot pool
  (doc, initialGen, replayedCommands, migrationCmds) <- case mSnapshot of
    Nothing -> do
      putStrLn "No snapshot found, starting with empty document"
      pure (emptyDocument, 0, 0, [])
    Just (rawSnapshot, gen, migCmds) -> do
      putStrLn $ "Loaded snapshot at generation " <> show gen
      -- Apply migration commands first (v1→v2 schema upgrade)
      let systemUserId = Id UUID.nil
      snapshot <- applyMigrationCmds systemUserId rawSnapshot migCmds
      -- Then replay user commands since snapshot
      commands <- DB.loadCommandsSince pool gen
      putStrLn $ "Replaying " <> show (length commands) <> " commands since snapshot"
      doc' <- replayCommands snapshot commands
      pure (doc', gen + fromIntegral (length commands), length commands, migCmds)

  putStrLn $ "Document loaded (generation " <> show initialGen <> ")"

  -- Persist schema migration commands + new snapshot if any
  unless (null migrationCmds) $ do
    putStrLn $ "Schema migration produced " <> show (length migrationCmds) <> " compensating command(s)"
    let systemCtx = CommandContext (Id UUID.nil) legacySessionId
    latestGen <- foldM (\_ cmd -> snd <$> DB.saveCommand pool systemCtx cmd) initialGen migrationCmds
    DB.saveSnapshot pool doc latestGen
    putStrLn $ "Migration commands and snapshot saved at generation " <> show latestGen

  -- Create recovery snapshot if we replayed any commands (non-graceful shutdown recovery)
  when (replayedCommands > 0 && null migrationCmds) $ do
    putStrLn "Non-graceful shutdown detected: creating recovery snapshot..."
    DB.saveSnapshot pool doc initialGen
    putStrLn $ "Recovery snapshot created at generation " <> show initialGen

  -- Build and apply startup migration commands
  startupCmds <- buildStartupMigrations opts
  (doc', latestGen) <- applyStartupMigrations pool doc initialGen startupCmds

  -- Initialize CAS (content-addressable store for files)
  cas <- newCAS opts.casDir opts.casFileMode

  -- Derive instance ID from database name in connection string
  let instId = extractDbName (BS.unpack opts.dbConnString)

  -- Create shared TVars for document and generation
  docVar <- newTVarIO doc'
  genVar <- newTVarIO latestGen

  -- Start command processor (needs the TVars)
  proc <- startProcessor docVar genVar pool

  -- Initialize application state
  appState <- initAppState docVar genVar pool cas instId proc
  restState <- initRestState appState

  -- Seed session registry from pre-restart locks so their holders are
  -- visible to the stale-lock cleanup thread. Without this, locks from
  -- sessions that existed before this backend start would never be
  -- cleaned up (the registry is in-memory and otherwise starts empty).
  startupTime <- getCurrentTime
  SR.seedFromDocument appState.sessionRegistry doc' startupTime

  -- Start stale lock cleanup thread (6-hour threshold)
  _ <- SLC.startCleanupThread appState.sessionRegistry docVar proc (6 * 3600)

  -- Log startup
  DB.logStartup pool instanceId latestGen (opts.ensureTeacherO365 /= Nothing) Nothing
  putStrLn $ "Startup logged (instance: " <> UUID.toString instanceId <> ")"

  -- Set up graceful shutdown
  shutdown <- newEmptyMVar

  -- Periodic snapshot timer (every 15 minutes)
  _ <- forkIO $ snapshotTimer appState shutdown

  putStrLn $ "Starting WebSocket server on port " <> show opts.port
  putStrLn "Press Ctrl+C to stop"
  hFlush stdout

  -- Run server with file watchers for cache busting and graceful shutdown
  let frontendFiles =
        [ opts.staticDir <> "/app.wasm"
        , opts.staticDir <> "/index.js"
        , opts.staticDir <> "/ghc_wasm_jsffi.js"
        , opts.staticDir <> "/mathjax-tex-svg.js"
        , opts.staticDir <> "/output.css"
        ]
  withHashedFiles frontendFiles $ \hashRefs ->
    flip finally (gracefulShutdown appState pool instanceId shutdown) $ do
      let hashes = ShellHashes
            { wasmHash = hashRefs Map.! (opts.staticDir <> "/app.wasm")
            , indexJsHash = hashRefs Map.! (opts.staticDir <> "/index.js")
            , jsffiHash = hashRefs Map.! (opts.staticDir <> "/ghc_wasm_jsffi.js")
            , mathjaxHash = hashRefs Map.! (opts.staticDir <> "/mathjax-tex-svg.js")
            , outputCssHash = hashRefs Map.! (opts.staticDir <> "/output.css")
            }
          httpApp = serve appAPI (server securityConfig opts.staticDir hashes restState)
      run opts.port $
        websocketsOr
          defaultConnectionOptions
          (wsHandler appState securityConfig)
          httpApp

-- | Build startup migration commands from CLI options
buildStartupMigrations :: Options -> IO [Command]
buildStartupMigrations opts = do
  let initCmd = [Migration InitIfEmpty]
  teacherCmds <- case opts.ensureTeacherO365 of
    Nothing -> pure []
    Just email -> do
      newId <- Id <$> UUID.nextRandom
      pure [Migration (EnsureTeacherO365 newId (T.pack email))]
  pure $ initCmd <> teacherCmds

-- | Apply startup migration commands, persisting only those that succeed.
-- Commands that fail are silently skipped (they indicate no action needed).
applyStartupMigrations :: Pool Connection -> Document -> Int64 -> [Command] -> IO (Document, Int64)
applyStartupMigrations pool = go
  where
    systemCtx = CommandContext (Id UUID.nil) legacySessionId
    go doc gen [] = pure (doc, gen)
    go doc gen (cmd : rest) =
      case handleCommand systemCtx cmd doc of
        Left reason -> do
          putStrLn $ "Startup migration skipped: " <> T.unpack reason
          go doc gen rest
        Right (doc', _) -> do
          (_cmdId, gen') <- DB.saveCommand pool systemCtx cmd
          putStrLn $ "Startup migration applied at generation " <> show gen'
          go doc' gen' rest

-- | Replay commands on top of a document
-- Returns error if any command fails to apply. Each command is replayed
-- under its original 'SessionId' from the envelope so 'doRelease' matches
-- the snapshot's preserved lock holders.
replayCommands :: Document -> [(Int64, Competences.Document.User.UserId, SessionId, Command)] -> IO Document
replayCommands doc [] = pure doc
replayCommands doc ((gen, userId, sessionId, cmd) : rest) =
  case handleCommand (CommandContext userId sessionId) cmd doc of
    Left err -> die $ "Failed to replay command at generation " <> show gen <> ": " <> T.unpack err
    Right (doc', _) -> replayCommands doc' rest

-- | Apply migration commands to a document, aborting on failure
applyMigrationCmds :: Competences.Document.User.UserId -> Document -> [Command] -> IO Document
applyMigrationCmds _userId doc [] = pure doc
applyMigrationCmds userId doc (cmd : rest) =
  case handleCommand (CommandContext userId legacySessionId) cmd doc of
    Left err -> die $ "Failed to apply migration command: " <> T.unpack err
    Right (doc', _) -> applyMigrationCmds userId doc' rest

-- | Periodic snapshot timer (runs on startup, then every 12 hours)
-- Checks if snapshot should be taken based on time and command count
snapshotTimer :: AppState -> MVar () -> IO ()
snapshotTimer state _shutdown = go
  where
    go = do
      -- Get current generation
      maxGen <- DB.getMaxGeneration state.dbPool
      -- Check if snapshot should be taken
      shouldSnapshot <- DB.shouldTakeSnapshot state.dbPool maxGen
      when shouldSnapshot $ do
        putStrLn $ "Periodic snapshot timer: taking snapshot at generation " <> show maxGen
        doc <- atomically $ readTVar state.document
        DB.saveSnapshot state.dbPool doc maxGen
      -- Run snapshot garbage collection
      _ <- DB.pruneSnapshots state.dbPool
      threadDelay (12 * 60 * 60 * 1000000) -- 12 hours
      go

-- | Extract database name from PostgreSQL connection string.
-- Looks for "dbname=<name>" in the connection string.
-- Falls back to the full connection string if not found.
extractDbName :: String -> T.Text
extractDbName connStr =
  case lookup "dbname" pairs of
    Just name -> T.pack name
    Nothing -> T.pack connStr
  where
    pairs = map parseKV (words connStr)
    parseKV s = case break (== '=') s of
      (k, '=' : v) -> (k, v)
      (k, _) -> (k, "")

-- | Graceful shutdown: create final snapshot and log shutdown
gracefulShutdown :: AppState -> Pool Connection -> UUID.UUID -> MVar () -> IO ()
gracefulShutdown state pool instanceId shutdown = do
  putStrLn "\nShutting down..."

  -- Take final snapshot
  putStrLn "Creating final snapshot..."
  doc <- atomically $ readTVar state.document
  maxGen <- DB.getMaxGeneration pool
  DB.saveSnapshot pool doc maxGen
  putStrLn $ "Final snapshot saved at generation " <> show maxGen

  -- Log shutdown
  DB.logShutdown pool instanceId
  putStrLn "Shutdown logged"

  -- Close database pool
  DB.closePool pool
  putStrLn "Database connections closed"

  putStrLn "Goodbye!"
  putMVar shutdown ()
