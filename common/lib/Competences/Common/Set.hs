module Competences.Common.Set
  ( toggle
  )
where

import Data.Set (Set)
import Data.Set qualified as Set

-- | Insert if absent, delete if present.
toggle :: (Ord a) => a -> Set a -> Set a
toggle x s
  | Set.member x s = Set.delete x s
  | otherwise = Set.insert x s
