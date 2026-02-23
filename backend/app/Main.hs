module Main where

import Competences.Backend.Config (loadConfig)
import Competences.Backend.Database qualified as DB
import Competences.Backend.HashedFile (withHashedFiles)
import Competences.Backend.HTTP (FrontendHashes (..), appAPI, server)
import Competences.Backend.State (AppState (..), initAppState)
import Competences.Backend.WebSocket (wsHandler)
import Competences.Command (Command (..), MigrationCommand (..), handleCommand)
import Competences.Document (Document (..), emptyDocument)
import Competences.Document.Id (Id (..))
import Competences.Document.User qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (finally)
import Control.Monad (foldM, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Handler.Warp (run)
import Options.Applicative qualified as Opt
import Servant (serve)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import System.Exit (die)
import System.IO (hFlush, stdout)

-- | Command-line options
data Options = Options
  { port :: !Int
  , dbConnString :: !ByteString
  , configPath :: !FilePath
  , staticDir :: !FilePath
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
      ( Opt.long "config"
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
    <*> Opt.optional
      ( Opt.strOption
          ( Opt.long "ensure-teacher-o365"
              <> Opt.metavar "EMAIL"
              <> Opt.help "Ensure a Teacher user exists for this O365 email"
          )
      )

-- | Parser info with program description
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
  putStrLn $ "Loading configuration from: " <> opts.configPath
  (jwtSecret, oauth2Config) <- loadConfig opts.configPath

  putStrLn ""
  putStrLn "Competences Backend Server"
  putStrLn "=========================="
  putStrLn $ "Port: " <> show opts.port
  putStrLn $ "Database: " <> BS.unpack opts.dbConnString
  putStrLn $ "Static directory: " <> opts.staticDir
  putStrLn ""

  -- Initialize database connection pool
  putStrLn "Initializing database connection pool..."
  pool <- DB.initPool opts.dbConnString

  -- Check schema version
  putStrLn "Checking database schema version..."
  DB.checkSchemaVersion pool

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
    let systemUserId = Id UUID.nil
    latestGen <- foldM (\_ cmd -> DB.saveCommand pool systemUserId cmd) initialGen migrationCmds
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

  -- Initialize application state
  state <- initAppState pool
  atomically $ writeTVar state.document doc'

  -- Log startup
  DB.logStartup pool instanceId latestGen (opts.ensureTeacherO365 /= Nothing) Nothing
  putStrLn $ "Startup logged (instance: " <> UUID.toString instanceId <> ")"

  -- Set up graceful shutdown
  shutdown <- newEmptyMVar

  -- Periodic snapshot timer (every 15 minutes)
  _ <- forkIO $ snapshotTimer state shutdown

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
    flip finally (gracefulShutdown state pool instanceId shutdown) $ do
      let hashes = FrontendHashes
            { wasmHash = hashRefs Map.! (opts.staticDir <> "/app.wasm")
            , indexJsHash = hashRefs Map.! (opts.staticDir <> "/index.js")
            , jsffiHash = hashRefs Map.! (opts.staticDir <> "/ghc_wasm_jsffi.js")
            , mathjaxHash = hashRefs Map.! (opts.staticDir <> "/mathjax-tex-svg.js")
            , outputCssHash = hashRefs Map.! (opts.staticDir <> "/output.css")
            }
          httpApp = serve appAPI (server state oauth2Config jwtSecret opts.staticDir hashes)
      run opts.port $
        websocketsOr
          defaultConnectionOptions
          (wsHandler state jwtSecret)
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
    systemUserId = Id UUID.nil
    go doc gen [] = pure (doc, gen)
    go doc gen (cmd : rest) =
      case handleCommand systemUserId cmd doc of
        Left reason -> do
          putStrLn $ "Startup migration skipped: " <> T.unpack reason
          go doc gen rest
        Right (doc', _) -> do
          gen' <- DB.saveCommand pool systemUserId cmd
          putStrLn $ "Startup migration applied at generation " <> show gen'
          go doc' gen' rest

-- | Replay commands on top of a document
-- Returns error if any command fails to apply
replayCommands :: Document -> [(Int64, Competences.Document.User.UserId, Command)] -> IO Document
replayCommands doc [] = pure doc
replayCommands doc ((gen, userId, cmd) : rest) =
  case handleCommand userId cmd doc of
    Left err -> die $ "Failed to replay command at generation " <> show gen <> ": " <> T.unpack err
    Right (doc', _) -> replayCommands doc' rest

-- | Apply migration commands to a document, aborting on failure
applyMigrationCmds :: Competences.Document.User.UserId -> Document -> [Command] -> IO Document
applyMigrationCmds _userId doc [] = pure doc
applyMigrationCmds userId doc (cmd : rest) =
  case handleCommand userId cmd doc of
    Left err -> die $ "Failed to apply migration command: " <> T.unpack err
    Right (doc', _) -> applyMigrationCmds userId doc' rest

-- | Periodic snapshot timer (every 15 minutes)
-- Checks if snapshot should be taken based on time and command count
snapshotTimer :: AppState -> MVar () -> IO ()
snapshotTimer state _shutdown = go
  where
    go = do
      threadDelay (15 * 60 * 1000000) -- 15 minutes
      -- Get current generation
      maxGen <- DB.getMaxGeneration state.dbPool
      -- Check if snapshot should be taken
      shouldSnapshot <- DB.shouldTakeSnapshot state.dbPool maxGen
      when shouldSnapshot $ do
        putStrLn $ "Periodic snapshot timer: taking snapshot at generation " <> show maxGen
        doc <- atomically $ readTVar state.document
        DB.saveSnapshot state.dbPool doc maxGen
      go

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
