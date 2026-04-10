{-# LANGUAGE CPP #-}

module Competences.Document.Session
  ( Session
  , SessionId
  , legacySessionId
  )
where

import Competences.Document.Id (Id, nilId)

-- | Phantom type for session identifiers
data Session

-- | A session identifies a single browser tab / client instance.
-- Persists across WebSocket reconnections (stored in sessionStorage).
type SessionId = Id Session

-- | Well-known session ID for pre-session locks and v1 command replay.
legacySessionId :: SessionId
legacySessionId = nilId
