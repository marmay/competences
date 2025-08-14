{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Reexports ixset-typed with some additional instances for integrating
-- with optics. When it comes to the At and Ix instances, the first IxSet
-- index is assumed to be the primary key that can be used with those.
module Competences.Common.IxSet
  ( module Data.IxSet.Typed 
  )
where

import Data.IxSet.Typed
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Optics.Core (At(..), Ixed(..), Index, IxValue, An_AffineTraversal, atraversal, lens)

class In (a :: Type) (as :: [Type])

instance a `In` (a ': as)

instance (a `In` as) => a `In` (b ': as)

type instance Index (Ix.IxSet (ix ': _) a) = ix
type instance IxValue (Ix.IxSet (_ ': _) a) = a

instance (Ix.Indexable (ix0 ': ixs) a) => Ixed (Ix.IxSet (ix0 ': ixs) a) where
  type IxKind (Ix.IxSet (ix0 ': ixs) a) = An_AffineTraversal
  ix i = atraversal get set
    where
      get s = maybe (Left s) Right $ Ix.getOne $ s Ix.@= i
      set s v = case Ix.getOne (s Ix.@= i) of
        Just _ -> Ix.insert v $ Ix.deleteIx i s
        Nothing -> s

instance (Ix.Indexable (ix0 ': ixs) a) => At (Ix.IxSet (ix0 ': ixs) a) where
  at i = lens get set
    where
      get s = Ix.getOne $ s Ix.@= i
      set s Nothing = Ix.deleteIx i s
      set s (Just v) = Ix.insert v $ Ix.deleteIx i s
