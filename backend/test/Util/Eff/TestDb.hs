module Util.Eff.TestDb
  ( TestDbSettings (..)
  , defaultTestDbSettings
  , dbSettingsForTestDb
  , withTestDb
  ) where

import Effectful (Eff, (:>))
import Effectful.Exception (bracket)
import Effectful.Temporary (withSystemTempDirectory, Temporary)
import Effectful.Process (Process, ProcessHandle, terminateProcess, callProcess, spawnProcess, waitForProcess)
import System.Exit (ExitCode(..))
import Competences.Backend.Eff.Db (DbSettings(..))

data TestDbSettings = TestDbSettings
  { pgInitDbCmd :: !FilePath
  , pgPostgresCmd :: !FilePath
  , pgIsReadyCmd :: !FilePath
  , pgPsqlCmd :: !FilePath
  , pgDbName :: !String
  , pgDbDirTemplate :: !String
  , pgSchemaPath :: !FilePath
  } deriving (Eq, Show)

defaultTestDbSettings :: FilePath -> TestDbSettings
defaultTestDbSettings schemaPath = TestDbSettings
  { pgInitDbCmd = "initdb"
  , pgPostgresCmd = "postgres"
  , pgIsReadyCmd = "pg_isready"
  , pgPsqlCmd = "psql"
  , pgDbName = "testdb"
  , pgDbDirTemplate = "competences-testdb-XXXXXX"
  , pgSchemaPath = schemaPath
  }

dbSettingsForTestDb :: DbSettings
dbSettingsForTestDb = DbSettings
  { dbHost = "localhost"
  , dbPort = 5432
  , dbUser = "pgtest"
  , dbPassword = "pgtest"
  , dbDatabase = "testdb"
  , poolRetentionTime = 0.5
  , poolMaxConnections = 16
  }

withTestDb :: forall es. (Temporary :> es, Process :> es) => TestDbSettings -> Eff es () -> Eff es ()
withTestDb s a = do
  withSystemTempDirectory s.pgDbDirTemplate $
    \p -> bracket (createTestDb s p) (stopTestDb) (\_ -> a)

createTestDb :: forall es. (Process :> es) => TestDbSettings -> FilePath -> Eff es ProcessHandle
createTestDb s p = do
  -- We have to run initdb to set up the data directory.
  -- Then, we can start the database server. With the
  -- database server started, we can set up the database
  -- and run the schema file.
  callProcess s.pgInitDbCmd ["-D", p, "-U", "pgtest"]
  h <- spawnProcess s.pgPostgresCmd ["-D", p, "-k", p]
  waitForSuccess s.pgIsReadyCmd ["-h", "localhost", "-U", "pgtest", "-d", "postgres"]
  callProcess s.pgPsqlCmd ["-h", "localhost", "-U", "pgtest", "-d", "postgres", "-c", "CREATE DATABASE " <> s.pgDbName <> ";"]
  callProcess s.pgPsqlCmd ["-h", "localhost", "-U", "pgtest", "-d", s.pgDbName, "-f", s.pgSchemaPath]
  callProcess s.pgPsqlCmd ["-h", "localhost", "-U", "pgtest", "-d", s.pgDbName, "-c", "ALTER ROLE pgtest WITH PASSWORD 'pgtest';"]
  pure h

stopTestDb :: forall es. (Process :> es) => ProcessHandle -> Eff es ()
stopTestDb h = do
  terminateProcess h
  _ <- waitForProcess h
  pure ()

waitForSuccess :: forall es. (Process :> es) => FilePath -> [String] -> Eff es ()
waitForSuccess cmd args = do
  exitCode <- waitForProcess =<< spawnProcess cmd args
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> waitForSuccess cmd args
