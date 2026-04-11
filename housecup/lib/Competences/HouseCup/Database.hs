{-# LANGUAGE QuasiQuotes #-}

module Competences.HouseCup.Database
  ( loadDocumentAt
  )
where

import Competences.Command (Command, handleCommand)
import Competences.Document.Session (legacySessionId)
import Competences.Document (Document (..), emptyDocument)
import Competences.Document.Id (Id (..))
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), Result (..), Value, eitherDecodeStrict, fromJSON, withObject, (.:))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..))
import Data.UUID.Types qualified as UUID
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import System.Exit (die)

-- | Envelope for deserializing snapshot JSONB from the database.
data SnapshotEnvelope = SnapshotEnvelope
  { version :: !Int
  , payload :: !Value
  }

instance FromJSON SnapshotEnvelope where
  parseJSON = withObject "SnapshotEnvelope" $ \v ->
    SnapshotEnvelope
      <$> v .: "version"
      <*> v .: "payload"

-- | Envelope for deserializing command JSONB from the database.
data CommandEnvelope = CommandEnvelope
  { version :: !Int
  , userId :: !UserId
  , payload :: !Value
  }

instance FromJSON CommandEnvelope where
  parseJSON = withObject "CommandEnvelope" $ \v -> do
    ver <- v .: "version"
    userIdText <- v .: "userId"
    p <- v .: "payload"
    case UUID.fromText userIdText of
      Nothing -> fail $ "Invalid userId UUID: " <> show userIdText
      Just uuid -> pure $ CommandEnvelope ver (Id uuid) p

unwrapSnapshot :: SnapshotEnvelope -> Either Text Document
unwrapSnapshot env = case env.version of
  1 -> case fromJSON env.payload of
    Success doc -> Right doc
    Error err -> Left $ "Failed to parse snapshot v1: " <> T.pack err
  2 -> case fromJSON env.payload of
    Success doc -> Right doc
    Error err -> Left $ "Failed to parse snapshot v2: " <> T.pack err
  3 -> case fromJSON env.payload of
    Success doc -> Right doc
    Error err -> Left $ "Failed to parse snapshot v3: " <> T.pack err
  v -> Left $ "Unknown snapshot version: " <> T.pack (show v)

unwrapCommand :: CommandEnvelope -> Either Text Command
unwrapCommand env = case env.version of
  1 -> case fromJSON env.payload of
    Success cmd -> Right cmd
    Error err -> Left $ "Failed to parse command v1: " <> T.pack err
  2 -> case fromJSON env.payload of
    Success cmd -> Right cmd
    Error err -> Left $ "Failed to parse command v2: " <> T.pack err
  v -> Left $ "Unknown command version: " <> T.pack (show v)

-- | Find the latest snapshot with created_at <= target time.
loadSnapshotBefore :: Connection -> UTCTime -> IO (Maybe (Document, Int64))
loadSnapshotBefore conn cutoff = do
  rows <-
    query
      conn
      [sql|
        SELECT generation, document_data
        FROM snapshots
        WHERE created_at <= ?
        ORDER BY generation DESC
        LIMIT 1
      |]
      (Only cutoff) ::
      IO [(Int64, Text)]
  case rows of
    [] -> pure Nothing
    (generation, envelopeText) : _ ->
      case eitherDecodeStrict (encodeUtf8 envelopeText) of
        Left err -> die $ "Failed to decode snapshot envelope: " <> err
        Right envelope ->
          case unwrapSnapshot envelope of
            Left err -> die $ "Failed to unwrap snapshot: " <> T.unpack err
            Right doc -> pure $ Just (doc, generation)

-- | Load commands with generation > g AND created_at <= target time.
loadCommandsUntil :: Connection -> Int64 -> UTCTime -> IO [(UserId, Command)]
loadCommandsUntil conn sinceGen cutoff = do
  rows <-
    query
      conn
      [sql|
        SELECT command_data
        FROM commands
        WHERE generation > ? AND created_at <= ?
        ORDER BY generation ASC
      |]
      (sinceGen, cutoff) ::
      IO [Only Value]
  pure
    [ (envelope.userId, cmd)
    | Only envelopeValue <- rows
    , Success envelope <- [fromJSON envelopeValue]
    , Right cmd <- [unwrapCommand envelope]
    ]

-- | Convert a Day to end-of-day UTCTime (23:59:59).
dayToEndOfDay :: Day -> UTCTime
dayToEndOfDay day = UTCTime day 86399

-- | Reconstruct the document at a point in time.
--
-- Finds the closest snapshot before end-of-day of the given Day,
-- then replays all commands up to that time using handleCommand.
-- Falls back to emptyDocument if no snapshot exists before the target date.
loadDocumentAt :: Connection -> Day -> IO Document
loadDocumentAt conn day = do
  let cutoff = dayToEndOfDay day
  mSnapshot <- loadSnapshotBefore conn cutoff
  let (baseDoc, sinceGen) = case mSnapshot of
        Just (doc, gen) -> (doc, gen)
        Nothing -> (emptyDocument, 0)
  commands <- loadCommandsUntil conn sinceGen cutoff
  pure $ replayCommands baseDoc commands

-- | Replay a list of commands on a document, skipping any that fail.
replayCommands :: Document -> [(UserId, Command)] -> Document
replayCommands = foldl applyCommand
  where
    applyCommand doc (uid, cmd) =
      case handleCommand uid legacySessionId cmd doc of
        Right (doc', _) -> doc'
        Left _err -> doc -- skip failed commands (mirrors backend behavior)
