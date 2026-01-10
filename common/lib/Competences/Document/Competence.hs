module Competences.Document.Competence
  ( CompetenceId
  , Competence (..)
  , CompetenceLevelId
  , CompetenceIxs
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , competenceLevelIdsOf
  , levelDescription
  , isLevelLocked
  , hasLevelContent
  )
where

import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey, (.:), (.:?), (.!=), withObject)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Map qualified as M
import Data.Set qualified as S
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
  deriving (Eq, Generic, Ord, Show)

-- | Information about a competence level (description and locked status).
-- A level entry exists in the map iff it has a non-empty description.
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

competenceLevelIdsOf :: Competence -> [CompetenceLevelId]
competenceLevelIdsOf competence =
   map (competence.id,) $ M.keys competence.levels

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

instance FromJSON Level

instance ToJSON Level

instance Binary Level

instance FromJSONKey Level

instance ToJSONKey Level

instance FromJSON LevelInfo

instance ToJSON LevelInfo

instance Binary LevelInfo

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

instance Binary Competence

instance Orderable Competence
