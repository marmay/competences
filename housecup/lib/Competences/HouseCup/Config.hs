module Competences.HouseCup.Config
  ( HouseEntry (..)
  , HouseConfig
  , ResolvedConfig (..)
  , resolveHouseConfig
  )
where

import Competences.Document (Document (..), User (..))
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.IxSet.Typed qualified as Ix
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
