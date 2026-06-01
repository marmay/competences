{-# LANGUAGE CPP #-}

module Competences.Document.Competence
  ( CompetenceId
  , Competence (..)
  , CompetenceLevelId
  , CompetenceIxs
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , levelToText
  , levelFromText
  , competenceLevelIdsOf
  , getLevelInfo
  , levelDescription
  , isLevelLocked
  , hasLevelContent
#ifdef WITH_AESON
  , levelMapToJSON
  , parseLevelMap
#endif
  )
where

import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey, Value (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Set qualified as S
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

type CompetenceId = Id Competence

-- | Level of a competence.
data Level
  = -- | Basic level of competence; the essentials.
    BasicLevel
  | -- | Intermediate level; slightly going above the essentials.
    IntermediateLevel
  | -- | Advanced level; mastering the given competence in terms
    -- of the current curriculum.
    AdvancedLevel
  deriving (Eq, Generic, Ord, Read, Show)

-- | Information about a competence level (description and locked status).
-- Invariant: A level entry exists in the map iff it has a non-empty description.
data LevelInfo = LevelInfo
  { description :: !Text
  , locked :: !Bool
  }
  deriving (Eq, Generic, Ord, Show)

data Competence = Competence
  { id :: !CompetenceId
  , competenceGridId :: !CompetenceGridId
  , order :: !Order
  , description :: !Text
  , levels :: !(M.Map Level LevelInfo)
  }
  deriving (Eq, Generic, Ord, Show)

-- | List of all levels in increasing order of competence.
allLevels :: [Level]
allLevels = [BasicLevel, IntermediateLevel, AdvancedLevel]

-- | Canonical text form of a 'Level', matching the constructor name
-- and the string produced by 'ToJSON Level'. Used as the object key
-- in the new level-keyed-map encoding.
levelToText :: Level -> Text
levelToText BasicLevel = "BasicLevel"
levelToText IntermediateLevel = "IntermediateLevel"
levelToText AdvancedLevel = "AdvancedLevel"

-- | Inverse of 'levelToText'.
levelFromText :: Text -> Maybe Level
levelFromText "BasicLevel" = Just BasicLevel
levelFromText "IntermediateLevel" = Just IntermediateLevel
levelFromText "AdvancedLevel" = Just AdvancedLevel
levelFromText _ = Nothing

competenceLevelIdsOf :: Competence -> [CompetenceLevelId]
competenceLevelIdsOf competence =
   map (competence.id,) $ M.keys competence.levels

-- | Get level info (empty if not present)
getLevelInfo :: Level -> Competence -> LevelInfo
getLevelInfo lvl c = M.findWithDefault (LevelInfo T.empty False) lvl c.levels

-- | Get description for a level (empty if not present)
levelDescription :: Level -> Competence -> Text
levelDescription lvl c = maybe T.empty (.description) (c.levels M.!? lvl)

-- | Check if level is locked
isLevelLocked :: Level -> Competence -> Bool
isLevelLocked lvl c = maybe False (.locked) (c.levels M.!? lvl)

-- | Check if level has content (description)
hasLevelContent :: Level -> Competence -> Bool
hasLevelContent lvl c = M.member lvl c.levels

type CompetenceLevelId = (CompetenceId, Level)

type CompetenceIxs = '[CompetenceId, Order, CompetenceGridId]

instance Ix.Indexable CompetenceIxs Competence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.order))
      (Ix.ixFun $ singleton . (.competenceGridId))

#ifdef WITH_AESON
instance FromJSON Level

instance ToJSON Level

instance FromJSONKey Level

instance ToJSONKey Level

-- | Encode a level-keyed map as a JSON *object* keyed by level name
-- (e.g. @{ "BasicLevel": v }@) — the new, human-friendly format.
--
-- Note: this is deliberately a standalone helper rather than a text
-- 'ToJSONKey'/'FromJSONKey' instance on 'Level'. Those instances are
-- shared by every @Map Level _@ in the persisted document and command
-- log, which is still on the legacy array-of-pairs encoding; flipping
-- them globally would break reading that data. Sites migrate to this
-- helper one batch at a time (see docs/TODO.md).
levelMapToJSON :: ToJSON v => M.Map Level v -> Value
levelMapToJSON m =
  object [Key.fromText (levelToText k) .= v | (k, v) <- M.toAscList m]

-- | Parse a level-keyed map, accepting *both* the new object form and
-- the legacy array-of-pairs form (@[["BasicLevel", v], ...]@). The
-- array fallback delegates to the stock @FromJSON (Map Level v)@
-- instance, so it keeps working as long as 'FromJSONKey Level' stays
-- on its default (value) encoding.
parseLevelMap :: FromJSON v => Value -> Parser (M.Map Level v)
parseLevelMap = \case
  Object o -> fmap M.fromList (traverse parseEntry (KM.toList o))
    where
      parseEntry (k, v) = do
        lvl <-
          maybe (fail ("Unknown level key: " <> Key.toString k)) pure $
            levelFromText (Key.toText k)
        (,) lvl <$> parseJSON v
  arr@(Array _) -> parseJSON arr
  other -> typeMismatch "level-keyed map (object or array of pairs)" other

instance FromJSON LevelInfo

instance ToJSON LevelInfo
#endif

instance Binary Level

instance Binary LevelInfo

#ifdef WITH_AESON
-- | Custom instance for backward-compatible parsing.
-- Handles both old format (levelDescriptions + lockedLevels) and new format (levels).
instance FromJSON Competence where
  parseJSON = withObject "Competence" $ \v -> do
    cId <- v .: "id"
    gridId <- v .: "competenceGridId"
    cOrder <- v .: "order"
    desc <- v .: "description"
    -- Try new format first, fall back to old format
    mLevels <- v .:? "levels"
    lvls <- case mLevels of
      Just l -> pure l
      Nothing -> do
        -- Old format: convert levelDescriptions + lockedLevels to levels
        levelDescs <- v .: "levelDescriptions"
        lockedSet <- v .:? "lockedLevels" .!= S.empty
        pure $ M.mapWithKey (\lvl d -> LevelInfo d (S.member lvl lockedSet)) levelDescs
    pure $ Competence cId gridId cOrder desc lvls

instance ToJSON Competence
#endif

instance Binary Competence

instance Orderable Competence
