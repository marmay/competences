module Competences.Backend.State
  ( AppState (..)
  , initAppState
  , getDocument
  )
where

import Competences.Backend.CAS (CAS, InstanceId)
import Competences.Backend.CommandProcessor (CommandProcessor)
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
  }

-- | Initialize application state with pre-created TVars.
-- The document and generation TVars are created externally so they can be
-- shared with the CommandProcessor before AppState is assembled.
initAppState :: TVar Document -> TVar Int64 -> Pool Connection -> CAS -> InstanceId -> CommandProcessor -> IO AppState
initAppState docVar genVar pool cas' instId proc = do
  nextId <- newTVarIO 0
  pure $ AppState docVar nextId pool cas' instId proc genVar

-- | Get current document (read-only)
getDocument :: AppState -> IO Document
getDocument = readTVarIO . (.document)
