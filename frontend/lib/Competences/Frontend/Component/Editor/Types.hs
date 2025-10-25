module Competences.Frontend.Component.Editor.Types
  ( Model (..)
  , Action (..)
  , ReorderAction (..)
  )
where

import Competences.Document (UserId, User)
import Competences.Frontend.SyncDocument (DocumentChange)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Data.Foldable (toList)

data Model a f = Model
  { entries :: !(Maybe (f (a, Maybe UserId)))
  , patches :: !(Map.Map a a)
  , reorderFrom :: !(Maybe a)
  , users :: !(Map.Map UserId User)
  }
  deriving (Generic)

instance (Eq a, Functor f, Foldable f) => Eq (Model a f) where
  a == b =
    fmap toList a.entries == fmap toList b.entries
      && a.patches == b.patches
      && a.reorderFrom == b.reorderFrom

data Action a
  = StartEditing !a
  | CancelEditing !a
  | FinishEditing !a
  | StartMoving !a
  | CancelMoving
  | FinishMoving !(ReorderAction a)
  | Delete !a
  | UpdatePatch !a !a
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

data ReorderAction a
  = ReorderToFront
  | ReorderToBack
  | ReorderForward
  | ReorderBackward
  | ReorderBefore !a
  | ReorderAfter !a
  deriving (Eq, Show)
