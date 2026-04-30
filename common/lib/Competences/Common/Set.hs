module Competences.Common.Set
  ( toggle
  , memberLens
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Optics.Core (Lens', lens)

-- | Insert if absent, delete if present.
toggle :: (Ord a) => a -> Set a -> Set a
toggle x s
  | Set.member x s = Set.delete x s
  | otherwise = Set.insert x s

-- | Lens onto a set's membership predicate for a given element.
-- Setting 'True' inserts; setting 'False' deletes.
memberLens :: (Ord a) => a -> Lens' (Set a) Bool
memberLens x =
  lens
    (Set.member x)
    (\s b -> if b then Set.insert x s else Set.delete x s)
