module Competences.Frontend.Component.Editor.Types
  ( Model (..)
  , Action (..)
  , Reorder' (..)
  , translateReorder'
  )
where

import Competences.Document (User, UserId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Reorder (..))
import Competences.Frontend.SyncContext (DocumentChange)
import Data.Foldable (toList)
import Data.Map qualified as Map
import GHC.Generics (Generic)

data Model a patch f = Model
  { entries :: !(Maybe (f (a, Maybe UserId)))
  , patches :: !(Map.Map a patch)
  , reorderFrom :: !(Maybe a)
  , refocusTarget :: !(Maybe a)
  , users :: !(Map.Map UserId User)
  }
  deriving (Generic)

instance (Eq a, Eq patch, Functor f, Foldable f) => Eq (Model a patch f) where
  a == b =
    fmap toList a.entries == fmap toList b.entries
      && a.patches == b.patches
      && a.reorderFrom == b.reorderFrom

data Action a patch
  = StartEditing !a
  | CancelEditing !a
  | FinishEditing !a
  | StartMoving !a
  | CancelMoving
  | FinishMoving !(Reorder' a)
  | Delete !a
  | UpdatePatch !a !patch
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

data Reorder' a
  = Front'
  | Back'
  | Forward'
  | Backward'
  | Before' !a
  | After' !a
  deriving (Eq, Show)

translateReorder' :: (a -> Id a) -> Reorder' a -> Reorder a
translateReorder' _ Front' = Front
translateReorder' _ Back' = Back
translateReorder' _ Forward' = Forward
translateReorder' _ Backward' = Backward
translateReorder' f (Before' a) = Before (f a)
translateReorder' f (After' a) = After (f a)
