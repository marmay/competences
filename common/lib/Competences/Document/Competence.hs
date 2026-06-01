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
  )
where

import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (typeMismatch)
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

-- | Level-keyed maps serialise as a JSON *object* keyed by level name
-- (e.g. @{ "BasicLevel": v }@) rather than aeson's default array of
-- @[key, value]@ pairs. This @OVERLAPPING@ instance specialises the
-- stock @Map k v@ instance for @k ~ Level@; it lives alongside 'Level'
-- so it is not an orphan (and is therefore in scope everywhere 'Level'
-- is). The companion 'FromJSON' below reads /both/ the new object form
-- and the legacy array form, so existing snapshots and command logs
-- keep parsing.
instance {-# OVERLAPPING #-} (ToJSON a) => ToJSON (M.Map Level a) where
  toJSON m = object [Key.fromText (levelToText k) .= v | (k, v) <- M.toAscList m]

instance {-# OVERLAPPING #-} (FromJSON a) => FromJSON (M.Map Level a) where
  parseJSON = \case
    Object o -> fmap M.fromList (traverse parseEntry (KM.toList o))
      where
        parseEntry (k, v) = do
          lvl <-
            maybe (fail ("Unknown level key: " <> Key.toString k)) pure $
              levelFromText (Key.toText k)
          (lvl,) <$> parseJSON v
    -- Legacy array-of-pairs form. Parsing as a list of @(Level, a)@
    -- pairs goes through the list/tuple/'FromJSON' 'Level' instances,
    -- so it never recurses back into this instance.
    arr@(Array _) -> fmap M.fromList (parseJSON arr)
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
