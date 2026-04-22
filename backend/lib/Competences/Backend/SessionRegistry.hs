-- | Server-side session registry.
--
-- Tracks which sessions have active WebSocket connections and when
-- they last disconnected. Used for stale lock cleanup and lock
-- stealing validation.
module Competences.Backend.SessionRegistry
  ( SessionEntry (..)
  , SessionRegistry
  , newRegistry
  , registerSession
  , unregisterConnection
  , isSessionAlive
  , findStaleSessions
  , removeStaleSessions
  , seedFromDocument
  )
where

import Competences.Backend.CommandProcessor (ConnectionId)
import Competences.Document (Document (..))
import Competences.Document.Lock (LockHolder (..))
import Competences.Document.Session (SessionId)
import Competences.Document.User (UserId)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

-- | Server-side state for a session.
data SessionEntry = SessionEntry
  { userId :: !UserId
  , connections :: !(Set ConnectionId)
  , lastDisconnect :: !(Maybe UTCTime)
  -- ^ Nothing = has active connections; Just t = all connections dropped at t
  }

type SessionRegistry = TVar (Map SessionId SessionEntry)

-- | Create a new empty session registry.
newRegistry :: IO SessionRegistry
newRegistry = newTVarIO Map.empty

-- | Register a connection for a session.
-- Creates the session entry if it doesn't exist.
registerSession :: SessionRegistry -> SessionId -> UserId -> ConnectionId -> IO ()
registerSession registry sid uid connId = atomically $ do
  reg <- readTVar registry
  let entry = case Map.lookup sid reg of
        Just e -> e { connections = Set.insert connId e.connections, lastDisconnect = Nothing }
        Nothing -> SessionEntry uid (Set.singleton connId) Nothing
  writeTVar registry (Map.insert sid entry reg)

-- | Remove a connection from a session.
-- If no connections remain, records the disconnect time.
unregisterConnection :: SessionRegistry -> SessionId -> ConnectionId -> IO ()
unregisterConnection registry sid connId = do
  now <- getCurrentTime
  atomically $ do
    reg <- readTVar registry
    case Map.lookup sid reg of
      Nothing -> pure ()
      Just entry ->
        let conns' = Set.delete connId entry.connections
            entry' = if Set.null conns'
              then entry { connections = conns', lastDisconnect = Just now }
              else entry { connections = conns' }
        in writeTVar registry (Map.insert sid entry' reg)

-- | Check if a session has at least one active connection.
isSessionAlive :: SessionRegistry -> SessionId -> IO Bool
isSessionAlive registry sid = do
  reg <- readTVarIO registry
  pure $ case Map.lookup sid reg of
    Just entry -> not (Set.null entry.connections)
    Nothing -> False

-- | Find sessions with no active connections that have been
-- disconnected for longer than the given threshold.
findStaleSessions :: SessionRegistry -> NominalDiffTime -> IO [(SessionId, SessionEntry)]
findStaleSessions registry threshold = do
  now <- getCurrentTime
  reg <- readTVarIO registry
  pure
    [ (sid, entry)
    | (sid, entry) <- Map.toList reg
    , Set.null entry.connections
    , Just disconnectTime <- [entry.lastDisconnect]
    , diffUTCTime now disconnectTime > threshold
    ]

-- | Seed the registry from sessionIds found in @doc.locks@.
--
-- Called once on startup so that locks held by sessions from a
-- previous backend run become visible to 'findStaleSessions'.
-- Without this, the in-memory registry starts empty and
-- pre-restart sessions are invisible to the cleanup thread,
-- leaving their locks as forever-ghosts.
--
-- Seeded entries look like a just-happened disconnect:
-- @connections = empty@, @lastDisconnect = Just seedTime@. If a
-- client later reconnects with a matching sessionId,
-- 'registerSession' clears @lastDisconnect@ and the session keeps
-- its locks. Otherwise the entry ages out via the normal
-- threshold.
seedFromDocument :: SessionRegistry -> Document -> UTCTime -> IO ()
seedFromDocument registry doc seedTime = atomically $ do
  reg <- readTVar registry
  let seeded = foldr insertIfAbsent reg (Map.elems doc.locks)
      insertIfAbsent holder m =
        let entry = SessionEntry holder.userId Set.empty (Just seedTime)
         in Map.insertWith (\_new old -> old) holder.sessionId entry m
  writeTVar registry seeded

-- | Remove session entries that still have no active connections.
-- Called after stale lock cleanup to prevent unbounded registry growth.
removeStaleSessions :: SessionRegistry -> [SessionId] -> IO ()
removeStaleSessions registry sids = atomically $ do
  reg <- readTVar registry
  let reg' = foldl' (\m sid -> case Map.lookup sid m of
                Just entry | Set.null entry.connections -> Map.delete sid m
                _ -> m  -- reconnected since scan, keep it
              ) reg sids
  writeTVar registry reg'
