module Competences.HouseCup.Config
  ( HouseEntry (..)
  , HouseConfig
  , ResolvedConfig (..)
  , resolveHouseConfig
  , ExtraPointsEntry (..)
  , ResolvedExtraPoints (..)
  , resolveExtraPoints
  )
where

import Competences.Document (Document (..), User (..))
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (die)

data HouseEntry = HouseEntry
  { name :: !Text
  , students :: ![Text]
  }
  deriving (Eq, Show)

instance FromJSON HouseEntry where
  parseJSON = withObject "HouseEntry" $ \v ->
    HouseEntry
      <$> v .: "name"
      <*> v .: "students"

type HouseConfig = [HouseEntry]

newtype ResolvedConfig = ResolvedConfig [(Text, [UserId])]
  deriving (Eq, Show)

-- | Resolve student display names to UserIds using the Document's User index.
-- Dies if any student name doesn't match exactly one user.
resolveHouseConfig :: Document -> HouseConfig -> IO ResolvedConfig
resolveHouseConfig doc entries = do
  resolved <- mapM resolveEntry entries
  pure $ ResolvedConfig resolved
  where
    resolveEntry :: HouseEntry -> IO (Text, [UserId])
    resolveEntry entry = do
      userIds <- mapM (resolveStudent entry.name) entry.students
      pure (entry.name, userIds)

    resolveStudent :: Text -> Text -> IO UserId
    resolveStudent houseName studentName = do
      let matches = doc.users Ix.@= studentName
      case Ix.getOne matches of
        Just user -> pure user.id
        Nothing ->
          die $
            T.unpack $
              "House '"
                <> houseName
                <> "': student '"
                <> studentName
                <> "' not found or ambiguous (matched "
                <> T.pack (show (Ix.size matches))
                <> " users)"

data ExtraPointsEntry = ExtraPointsEntry
  { student :: !Text
  , points :: !Integer
  , reason :: !Text
  }
  deriving (Eq, Show)

instance FromJSON ExtraPointsEntry where
  parseJSON = withObject "ExtraPointsEntry" $ \v ->
    ExtraPointsEntry
      <$> v .: "student"
      <*> v .: "points"
      <*> v .: "reason"

newtype ResolvedExtraPoints = ResolvedExtraPoints (Map UserId Integer)
  deriving (Eq, Show)

-- | Resolve extra points entries, mapping student names to UserIds and summing duplicates.
resolveExtraPoints :: Document -> [ExtraPointsEntry] -> IO ResolvedExtraPoints
resolveExtraPoints doc entries = do
  pairs <- mapM resolveEntry entries
  pure $ ResolvedExtraPoints $ Map.fromListWith (+) pairs
  where
    resolveEntry :: ExtraPointsEntry -> IO (UserId, Integer)
    resolveEntry entry = do
      let matches = doc.users Ix.@= entry.student
      case Ix.getOne matches of
        Just user -> pure (user.id, entry.points)
        Nothing ->
          die $
            T.unpack $
              "Extra points: student '"
                <> entry.student
                <> "' not found or ambiguous (matched "
                <> T.pack (show (Ix.size matches))
                <> " users)"
