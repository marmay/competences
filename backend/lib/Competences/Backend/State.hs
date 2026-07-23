module Competences.Backend.State
  ( AppState (..)
  , initAppState
  , getDocument
  , RestState (..)
  , initRestState
  , ensureUnconsumed
  )
where

import Competences.Backend.CAS (CAS, InstanceId)
import Competences.Backend.CommandProcessor (CommandProcessor)
import Competences.Backend.SessionRegistry (SessionRegistry)
import Competences.Backend.SessionRegistry qualified as SR
import Competences.Document (Document)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, stateTVar)
import Data.Int (Int64)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.UUID (UUID)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.STM (atomically)

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
  , consumedAssertionIds :: !(TVar [(UUID, UTCTime)])
  -- ^ This map ensures that assertions can't be used multiple times
  -- to mint session tokens. Holds a list of consumed ids, along with
  -- the time, they were consumed at. ensureUnconsumed single-handedly
  -- keeps that list short and returns whether assertion are
  -- unconsumed.
  }

-- | Initializes the RestState from the AppState and possibly from
-- other values.
initRestState :: AppState -> IO RestState
initRestState AppState{document} = do
  consumedAssertionIds <- newTVarIO []
  pure $ RestState document consumedAssertionIds

-- | Helper function to use consumedAssertionIds in RestState.
ensureUnconsumed :: UUID -> UTCTime -> TVar [(UUID, UTCTime)] -> IO Bool
ensureUnconsumed assertionId validUntil unconsumed = do
  now <- getCurrentTime
  atomically $ stateTVar unconsumed $ \unconsumed' ->
    let stillUnconsumed = filter ((>= now) . snd) unconsumed'
     in if assertionId `elem` map fst stillUnconsumed
           then (False, stillUnconsumed)
           else (True, (assertionId, validUntil) : stillUnconsumed)

