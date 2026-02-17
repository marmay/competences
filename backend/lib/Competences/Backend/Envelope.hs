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

import Competences.Command (Command (..), MigrationCommand (..))
import Competences.Document (Document (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Id (Id (..))
import Competences.Document.Lesson (LessonId)
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
  , (.:?)
  , (.=)
  )
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.UUID.Types qualified as UUID
import GHC.Generics (Generic)

-- | Current version of command envelope schema
currentCommandVersion :: Int
currentCommandVersion = 1

-- | Current version of snapshot envelope schema
currentSnapshotVersion :: Int
currentSnapshotVersion = 2

-- | Envelope for storing commands with version and metadata
data CommandEnvelope = CommandEnvelope
  { version :: !Int
  -- ^ Schema version number
  , userId :: !UserId
  -- ^ User who executed the command
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
      , "payload" .= env.payload
      ]

instance FromJSON CommandEnvelope where
  parseJSON = withObject "CommandEnvelope" $ \v -> do
    version <- v .: "version"
    userIdText <- v .: "userId"
    payload <- v .: "payload"
    case UUID.fromText userIdText of
      Nothing -> fail $ "Invalid userId UUID: " <> show userIdText
      Just uuid -> pure $ CommandEnvelope version (Id uuid) payload

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
wrapCommand :: UserId -> Command -> CommandEnvelope
wrapCommand userId cmd =
  CommandEnvelope
    { version = currentCommandVersion
    , userId = userId
    , payload = toJSON cmd
    }

-- | Unwrap a command envelope, applying migrations if needed
unwrapCommand :: CommandEnvelope -> Either Text Command
unwrapCommand env = case env.version of
  1 ->
    -- Current version: direct parse
    case fromJSON env.payload of
      Success cmd -> Right cmd
      Error err -> Left $ "Failed to parse command v1: " <> pack err
  v ->
    Left $ "Unknown command version: " <> pack (show v)

-- Future versions example:
--  2 -> do
--    -- Parse as CommandV1 and migrate to current
--    cmdV1 <- case fromJSON env.payload of
--      Success cmd -> Right cmd
--      Error err -> Left $ "Failed to parse command v2: " <> show err
--    Right $ migrateCommandV1toV2 cmdV1

-- | Wrap a document snapshot in an envelope at the current version
wrapSnapshot :: Document -> SnapshotEnvelope
wrapSnapshot doc =
  SnapshotEnvelope
    { version = currentSnapshotVersion
    , payload = toJSON doc
    }

-- | Unwrap a snapshot envelope, applying migrations if needed.
-- Returns the migrated document and any compensating commands that must be
-- persisted to the command log to make the migration reproducible on replay.
unwrapSnapshot :: SnapshotEnvelope -> Either Text (Document, [Command])
unwrapSnapshot env = case env.version of
  1 -> do
    -- V1 snapshot: Assignment had lessonId, Lesson had no assignments field.
    -- Parse the document (FromJSON backward compat handles missing/extra fields),
    -- then migrate lessonId links into Lesson.assignments.
    doc <- case fromJSON env.payload of
      Success d -> Right d
      Error err -> Left $ "Failed to parse snapshot v1: " <> pack err
    let linkMap = extractAssignmentLessonLinks env.payload
        cmds
          | Map.null linkMap = []
          | otherwise = [Migration (UpdateLessonAssignments (Map.toList linkMap))]
    Right (doc, cmds)
  2 ->
    -- Current version: direct parse
    case fromJSON env.payload of
      Success doc -> Right (doc, [])
      Error err -> Left $ "Failed to parse snapshot v2: " <> pack err
  v ->
    Left $ "Unknown snapshot version: " <> pack (show v)

-- | Extract Assignment.lessonId links from a v1 snapshot's raw JSON.
-- Returns a map from LessonId to list of AssignmentIds that referenced it.
extractAssignmentLessonLinks :: Value -> Map.Map LessonId [AssignmentId]
extractAssignmentLessonLinks payload =
  case payload of
    Object docObj ->
      case KM.lookup "assignments" docObj of
        Nothing -> Map.empty
        Just assignmentsVal ->
          let links = parseMaybe parseLinks assignmentsVal
           in maybe Map.empty id links
    _ -> Map.empty
  where
    parseLinks = withObject "AssignmentIxSet" $ \v -> do
      -- IxSet serializes as {"ixSet": [...]}
      items <- v .: "ixSet" :: Parser [Value]
      pairs <- mapM parseLink items
      pure $ Map.fromListWith (<>) [(lid, [aid]) | (aid, Just lid) <- pairs]

    parseLink = withObject "Assignment" $ \v -> do
      aid <- v .: "id"
      mLid <- v .:? "lessonId"
      pure (aid :: AssignmentId, mLid :: Maybe LessonId)

