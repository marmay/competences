-- | A small wrapper that pairs an entity with its 'EntityOrigin'
-- (published vs. draft).
--
-- Used by entity selectors so that draft and published variants live in
-- a single 'IxSet' rather than two parallel collections plus a side-set
-- of draft IDs. The 'Ord' instance defers to the wrapped entity's order
-- and uses 'EntityOrigin' only as a tiebreaker — keeping per-entity sort
-- order natural.
--
-- Each entity defines its own 'Indexable' instance for
-- @WithOrigin entity@. The pattern is straightforward:
--
-- @
-- type TaskWithOriginIxs = '[TaskId, TaskIdentifier, EntityOrigin]
-- instance Indexable TaskWithOriginIxs (WithOrigin Task) where
--   indices = ixList
--     (ixFun $ singleton . (.value.id))
--     (ixFun $ singleton . (.value.identifier))
--     (ixFun $ singleton . (.origin))
-- @
module Competences.Frontend.Common.WithOrigin
  ( WithOrigin (..)
  )
where

import Competences.Frontend.Component.Draft (EntityOrigin)
import GHC.Generics (Generic)

data WithOrigin a = WithOrigin
  { origin :: !EntityOrigin
  , value :: !a
  }
  deriving (Eq, Show, Generic)

instance Ord a => Ord (WithOrigin a) where
  compare (WithOrigin o1 v1) (WithOrigin o2 v2) =
    compare v1 v2 <> compare o1 o2
