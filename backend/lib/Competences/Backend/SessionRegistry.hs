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
  )
where

import Competences.Backend.CommandProcessor (ConnectionId)
import Competences.Document.Session (SessionId)
import Competences.Document.User (UserId)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
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
  reg <- atomically $ readTVar registry
  pure $ case Map.lookup sid reg of
    Just entry -> not (Set.null entry.connections)
    Nothing -> False

-- | Find sessions with no active connections that have been
-- disconnected for longer than the given threshold.
findStaleSessions :: SessionRegistry -> NominalDiffTime -> IO [(SessionId, SessionEntry)]
findStaleSessions registry threshold = do
  now <- getCurrentTime
  reg <- atomically $ readTVar registry
  pure
    [ (sid, entry)
    | (sid, entry) <- Map.toList reg
    , Set.null entry.connections
    , Just disconnectTime <- [entry.lastDisconnect]
    , diffUTCTime now disconnectTime > threshold
    ]
