module Main where

import Competences.Backend.Auth (JWTSecret (..), OAuth2Config (..))
import Competences.Backend.Config (loadConfig)
import Competences.Backend.Database qualified as DB
import Competences.Backend.HTTP (appAPI, server)
import Competences.Backend.State (AppState (..), initAppState)
import Competences.Backend.WebSocket (wsHandler)
import Competences.Command (Command (..), handleCommand)
import Competences.Document (Document)
import Competences.Document.Id (Id (..))
import Competences.Document.User qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int64)
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
  , initDocPath :: !(Maybe FilePath)
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
          ( Opt.long "init-document"
              <> Opt.metavar "FILE"
              <> Opt.help "Initial document JSON (used only if database is empty)"
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

  -- Initialize document from file if provided and database is empty
  case opts.initDocPath of
    Just path -> do
      putStrLn $ "Checking if database needs initialization from: " <> path
      isEmpty <- DB.isDatabaseEmpty pool
      if isEmpty
        then do
          putStrLn $ "Database is empty, initializing from " <> path
          docResult <- eitherDecodeFileStrict path
          case docResult of
            Left err -> die $ "Failed to parse init document: " <> err
            Right initDoc -> do
              -- Use system user ID (nil UUID) for SetDocument command
              let systemUserId = Competences.Document.Id.Id UUID.nil
              _ <- DB.saveCommand pool systemUserId (SetDocument initDoc)
              DB.saveSnapshot pool initDoc 1
              putStrLn "Database initialized with document"
        else putStrLn "Database not empty, skipping initialization"
    Nothing -> putStrLn "No init document provided"

  -- Load document from database
  putStrLn "Loading document from database..."
  mSnapshot <- DB.loadLatestSnapshot pool
  (doc, initialGen, replayedCommands) <- case mSnapshot of
    Nothing -> die "No document found in database. Provide --init-document to initialize."
    Just (snapshot, gen) -> do
      putStrLn $ "Loaded snapshot at generation " <> show gen
      -- Load and replay commands since snapshot
      commands <- DB.loadCommandsSince pool gen
      putStrLn $ "Replaying " <> show (length commands) <> " commands since snapshot"
      doc' <- replayCommands snapshot commands
      pure (doc', gen + fromIntegral (length commands), length commands)

  putStrLn $ "Document loaded (generation " <> show initialGen <> ")"

  -- Create recovery snapshot if we replayed any commands (non-graceful shutdown recovery)
  -- This avoids replaying the same commands again on next startup
  when (replayedCommands > 0) $ do
    putStrLn $ "Non-graceful shutdown detected: creating recovery snapshot..."
    DB.saveSnapshot pool doc initialGen
    putStrLn $ "Recovery snapshot created at generation " <> show initialGen

  -- Initialize application state
  state <- initAppState pool
  atomically $ writeTVar state.document doc

  -- Log startup
  DB.logStartup pool instanceId initialGen (opts.initDocPath /= Nothing) Nothing
  putStrLn $ "Startup logged (instance: " <> UUID.toString instanceId <> ")"

  -- Set up graceful shutdown
  shutdown <- newEmptyMVar

  -- Periodic snapshot timer (every 15 minutes)
  _ <- forkIO $ snapshotTimer state shutdown

  putStrLn $ "Starting WebSocket server on port " <> show opts.port
  putStrLn "Press Ctrl+C to stop"
  hFlush stdout

  -- Run server with graceful shutdown
  flip finally (gracefulShutdown state pool instanceId shutdown) $ do
    let httpApp = serve appAPI (server state oauth2Config jwtSecret opts.staticDir)
    run opts.port $
      websocketsOr
        defaultConnectionOptions
        (wsHandler state jwtSecret)
        httpApp

-- | Replay commands on top of a document
-- Returns error if any command fails to apply
replayCommands :: Document -> [(Int64, Competences.Document.User.UserId, Command)] -> IO Document
replayCommands doc [] = pure doc
replayCommands doc ((gen, userId, cmd) : rest) =
  case handleCommand userId cmd doc of
    Left err -> die $ "Failed to replay command at generation " <> show gen <> ": " <> T.unpack err
    Right (doc', _) -> replayCommands doc' rest

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
