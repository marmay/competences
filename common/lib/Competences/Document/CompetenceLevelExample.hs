{-# LANGUAGE CPP #-}

module Competences.Document.CompetenceLevelExample
  ( -- * IDs
    CompetenceLevelExampleId
    -- * Entity
  , CompetenceLevelExample (..)
  , CompetenceLevelExampleIxs
  , mkCompetenceLevelExample
  )
where

import Competences.Document.Competence (CompetenceId, CompetenceLevelId, Level)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable, orderMax)
import Competences.TaskContent.RichContent (RichContent)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)

-- | ID for a CompetenceLevelExample.
type CompetenceLevelExampleId = Id CompetenceLevelExample

-- | A short, concrete example ("teaser") for a competence level.
-- Designed to be compact enough to render in grid cells.
data CompetenceLevelExample = CompetenceLevelExample
  { id :: !CompetenceLevelExampleId
  , competenceId :: !CompetenceId
  , level :: !Level
  , order :: !Order
  , content :: !RichContent
  , attachments :: ![FileRef]
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON CompetenceLevelExample where
  parseJSON = withObject "CompetenceLevelExample" $ \v ->
    CompetenceLevelExample
      <$> v .: "id"
      <*> v .: "competenceId"
      <*> v .: "level"
      <*> v .: "order"
      <*> v .: "content"
      <*> v .:? "attachments" .!= []

instance ToJSON CompetenceLevelExample where
  toJSON e =
    object
      [ "id" .= e.id
      , "competenceId" .= e.competenceId
      , "level" .= e.level
      , "order" .= e.order
      , "content" .= e.content
      , "attachments" .= e.attachments
      ]
#endif

instance Binary CompetenceLevelExample

-- | IxSet indices for CompetenceLevelExample.
-- Indexed by:
--   CompetenceLevelExampleId (unique)
--   CompetenceLevelId = (CompetenceId, Level) — primary lookup for "all examples for this level"
--   CompetenceId — "does this competence have any examples?"
--   Order — for sorted retrieval
type CompetenceLevelExampleIxs = '[CompetenceLevelExampleId, CompetenceLevelId, CompetenceId, Order]

instance Ix.Indexable CompetenceLevelExampleIxs CompetenceLevelExample where
  indices =
    Ix.ixList
      (Ix.ixFun $ \e -> [e.id])
      (Ix.ixFun $ \e -> [(e.competenceId, e.level)])
      (Ix.ixFun $ \e -> [e.competenceId])
      (Ix.ixFun $ \e -> [e.order])

instance Orderable CompetenceLevelExample

-- | Create a new empty CompetenceLevelExample with the given ID, competence, and level.
mkCompetenceLevelExample :: CompetenceLevelExampleId -> CompetenceId -> Level -> CompetenceLevelExample
mkCompetenceLevelExample eid cid lvl =
  CompetenceLevelExample
    { id = eid
    , competenceId = cid
    , level = lvl
    , order = orderMax
    , content = mempty
    , attachments = []
    }
