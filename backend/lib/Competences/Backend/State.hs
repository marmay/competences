module Competences.Backend.State
  ( AppState (..)
  , initAppState
  , getDocument
  , RestState (..)
  , initRestState
  )
where

import Competences.Auth.ReplayProtection (mkConsumedLog, ConsumedLog)
import Competences.Backend.CAS (CAS, InstanceId)
import Competences.Backend.CommandProcessor (CommandProcessor)
import Competences.Backend.SessionRegistry (SessionRegistry)
import Competences.Backend.SessionRegistry qualified as SR
import Competences.Document (Document)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Data.Int (Int64)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

-- | Application state containing the document and connected clients
data AppState = AppState
  { document :: !(TVar Document)
  -- ^ Current document state
  , nextConnectionId :: !(TVar Int)
  -- ^ Counter for generating unique connection IDs
  , dbPool :: !(Pool Connection)
  -- ^ Database connection pool for command/snapshot persistence
  , cas :: !CAS
  -- ^ Content-addressable store for file storage
  , instanceId :: !InstanceId
  -- ^ Instance identifier (database name) for CAS ownership tracking
  , processor :: !CommandProcessor
  -- ^ Single-threaded command processor with per-client queues
  , currentGeneration :: !(TVar Int64)
  -- ^ Current command generation number (tracks latest DB generation)
  , sessionRegistry :: !SessionRegistry
  -- ^ Tracks active sessions for stale lock cleanup
  }

-- | Initialize application state with pre-created TVars.
-- The document and generation TVars are created externally so they can be
-- shared with the CommandProcessor before AppState is assembled.
initAppState :: TVar Document -> TVar Int64 -> Pool Connection -> CAS -> InstanceId -> CommandProcessor -> IO AppState
initAppState docVar genVar pool cas' instId proc = do
  nextId <- newTVarIO 0
  registry <- SR.newRegistry
  pure $ AppState docVar nextId pool cas' instId proc genVar registry

-- | Get current document (read-only)
getDocument :: AppState -> IO Document
getDocument = readTVarIO . (.document)

data RestState = RestState
  { document :: !(TVar Document)
  -- ^ In order to keep track of users, we need the document in the
  -- rest end points too.
  , consumedAssertionIds :: !ConsumedLog
  -- ^ Replay protection for /api/login; semantics live in
  -- Competences.Auth.ReplayProtection.
  }

-- | Initializes the RestState from the AppState and possibly from
-- other values.
initRestState :: AppState -> IO RestState
initRestState AppState{document} =
  mkConsumedLog >>= pure . RestState document
