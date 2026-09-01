{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}

{- |
Module: Competences.Backend.Envelope
Description: Versioned envelopes for commands and snapshots

This module provides a simple versioning mechanism for commands and snapshots
stored in PostgreSQL. Each envelope contains:
  - version: Schema version number (starts at 1)
  - userId: The user who executed the command (or system user for snapshots)
  - payload: The actual command or document as JSON Value

When the Command or Document structure changes, increment the version and add
migration functions to handle old versions.
-}
module Competences.Backend.Envelope
  ( CommandEnvelope (..)
  , SnapshotEnvelope (..)
  , currentCommandVersion
  , currentSnapshotVersion
  , wrapCommand
  , unwrapCommand
  , wrapSnapshot
  , unwrapSnapshot
  )
where

import Competences.Command (Command (..), CommandContext (..))
import Competences.Document (Document (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Session (SessionId, legacySessionId)
import Competences.Document.User (UserId)
import Data.Aeson
  ( FromJSON (..)
  , Result (..)
  , ToJSON (..)
  , Value (..)
  , fromJSON
  , object
  , toJSON
  , withObject
  , (.:)
  , (.=)
  )
import Data.Text (Text, pack)
import Data.UUID.Types qualified as UUID
import GHC.Generics (Generic)

-- | Current version of command envelope schema
-- V1: userId + payload
-- V2: userId + sessionId + payload
currentCommandVersion :: Int
currentCommandVersion = 2

-- | Current version of snapshot envelope schema
-- V1: Assignment had lessonId
-- V2: Lesson.assignments added; locks as [(Lock, UserId)]
-- V3: locks as [(Lock, LockHolder)]
currentSnapshotVersion :: Int
currentSnapshotVersion = 3

-- | Envelope for storing commands with version and metadata
data CommandEnvelope = CommandEnvelope
  { version :: !Int
  -- ^ Schema version number
  , userId :: !UserId
  -- ^ User who executed the command
  , sessionId :: !SessionId
  -- ^ Session that executed the command (legacySessionId for v1)
  , payload :: !Value
  -- ^ The actual command as JSON
  }
  deriving (Eq, Show, Generic)

-- | Envelope for storing document snapshots with version
data SnapshotEnvelope = SnapshotEnvelope
  { version :: !Int
  -- ^ Schema version number
  , payload :: !Value
  -- ^ The actual document as JSON
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommandEnvelope where
  toJSON env =
    object
      [ "version" .= env.version
      , "userId" .= UUID.toText env.userId.unId
      , "sessionId" .= UUID.toText env.sessionId.unId
      , "payload" .= env.payload
      ]

instance FromJSON CommandEnvelope where
  parseJSON = withObject "CommandEnvelope" $ \v -> do
    ver <- v .: "version"
    userIdText <- v .: "userId"
    payload <- v .: "payload"
    uid <- case UUID.fromText userIdText of
      Nothing -> fail $ "Invalid userId UUID: " <> show userIdText
      Just uuid -> pure (Id uuid)
    sid <- case ver of
      1 -> pure legacySessionId
      _ -> do
        sidText <- v .: "sessionId"
        case UUID.fromText sidText of
          Nothing -> fail $ "Invalid sessionId UUID: " <> show sidText
          Just uuid -> pure (Id uuid)
    pure $ CommandEnvelope ver uid sid payload

instance ToJSON SnapshotEnvelope where
  toJSON env =
    object
      [ "version" .= env.version
      , "payload" .= env.payload
      ]

instance FromJSON SnapshotEnvelope where
  parseJSON = withObject "SnapshotEnvelope" $ \v ->
    SnapshotEnvelope
      <$> v .: "version"
      <*> v .: "payload"

-- | Wrap a command in an envelope at the current version
wrapCommand :: CommandContext -> Command -> CommandEnvelope
wrapCommand ctx cmd =
  CommandEnvelope
    { version = currentCommandVersion
    , userId = ctx.userId
    , sessionId = ctx.sessionId
    , payload = toJSON cmd
    }

-- | Unwrap a command envelope. Only the current version is supported:
-- the 2026-09 identity rework (oid-keyed users, SystemCommand rename)
-- reset all databases, so older envelopes cannot exist.
unwrapCommand :: CommandEnvelope -> Either Text Command
unwrapCommand env = case env.version of
  2 ->
    case fromJSON env.payload of
      Success cmd -> Right cmd
      Error err -> Left $ "Failed to parse command v2: " <> pack err
  v ->
    Left $ "Unsupported command version: " <> pack (show v)

-- | Wrap a document snapshot in an envelope at the current version
wrapSnapshot :: Document -> SnapshotEnvelope
wrapSnapshot doc =
  SnapshotEnvelope
    { version = currentSnapshotVersion
    , payload = toJSON doc
    }

-- | Unwrap a snapshot envelope. Only the current version is supported:
-- the 2026-09 identity rework (oid-keyed users, SystemCommand rename)
-- reset all databases, so older envelopes cannot exist.
unwrapSnapshot :: SnapshotEnvelope -> Either Text Document
unwrapSnapshot env = case env.version of
  3 ->
    case fromJSON env.payload of
      Success doc -> Right doc
      Error err -> Left $ "Failed to parse snapshot v3: " <> pack err
  v ->
    Left $ "Unsupported snapshot version: " <> pack (show v)

