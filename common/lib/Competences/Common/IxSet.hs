{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Reexports ixset-typed with some additional instances for integrating
-- with optics. When it comes to the At and Ix instances, the first IxSet
-- index is assumed to be the primary key that can be used with those.
module Competences.Common.IxSet
  ( module Data.IxSet.Typed
  , replacePrimary
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (Binary (..))
import Data.IxSet.Typed
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Optics.Core (An_AffineTraversal, At (..), Index, IxValue, Ixed (..), atraversal, lens)

class In (a :: Type) (as :: [Type])

instance a `In` (a ': as)

instance (a `In` as) => a `In` (b ': as)

type instance Index (Ix.IxSet (ix ': _) a) = ix

type instance IxValue (Ix.IxSet (_ ': _) a) = a

instance (Ix.Indexable (ix0 ': ixs) a) => Ixed (Ix.IxSet (ix0 ': ixs) a) where
  type IxKind (Ix.IxSet (ix0 ': ixs) a) = An_AffineTraversal
  ix i = atraversal lensGet lensSet
    where
      lensGet s = maybe (Left s) Right $ Ix.getOne $ s Ix.@= i
      lensSet s v = case Ix.getOne (s Ix.@= i) of
        Just _ -> Ix.insert v $ Ix.deleteIx i s
        Nothing -> s

instance (Ix.Indexable (ix0 ': ixs) a) => At (Ix.IxSet (ix0 ': ixs) a) where
  at i = lens lensGet lensSet
    where
      lensGet s = Ix.getOne $ s Ix.@= i
      lensSet s Nothing = Ix.deleteIx i s
      lensSet s (Just v) = Ix.insert v $ Ix.deleteIx i s

replacePrimary
  :: forall ix ixs a
   . (Ix.Indexable (ix ': ixs) a) => ix -> (a -> a) -> Ix.IxSet (ix ': ixs) a -> Ix.IxSet (ix ': ixs) a
replacePrimary i f s = foldr (Ix.insert . f) (Ix.deleteIx i s) (Ix.toList $ s Ix.@= i)

instance (FromJSON a, Ix.Indexable ixs a) => FromJSON (Ix.IxSet ixs a) where
  parseJSON = fmap Ix.fromList . parseJSON

instance (ToJSON a, Ix.Indexable ixs a) => ToJSON (Ix.IxSet ixs a) where
  toJSON = toJSON . Ix.toList

instance (Binary a, Ix.Indexable ixs a) => Binary (Ix.IxSet ixs a) where
  put = put . Ix.toList
  get = fmap Ix.fromList get
