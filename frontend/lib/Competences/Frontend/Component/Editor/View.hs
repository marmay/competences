module Competences.Frontend.Component.Editor.View
  ( EditorView
  , EditorViewData (..)
  , EditorViewItem (..)
  , EditState (..)
  , DeleteState (..)
  , MoveState (..)
  , compactButtons
  )
where

import Competences.Document (User)
import Competences.Frontend.Component.Editor.Types (Action, Model)
import GHC.Generics (Generic)
import Miso qualified as M

data EditorViewData a f n = EditorViewData
  { fields :: ![n]
  , items :: !(f (EditorViewItem a f n))
  }
  deriving (Generic)

data EditState
  = EditingNotAvailable
  | NotEditableBecauseLocked (Maybe User)
  | NotEditing
  | Editing
  deriving (Eq, Show)

data DeleteState
  = DeleteNotAvailable
  | Deletable
  deriving (Eq, Show)

data MoveState
  = MovingNotAvailable
  | NotMoving
  | MoveSource
  | PotentialMoveTarget
  deriving (Eq, Show)

data EditorViewItem a f n = EditorViewItem
  { item :: !a
  , fieldData :: ![(n, M.View (Model a f) (Action a))]
  , editState :: !EditState
  , deleteState :: !DeleteState
  , moveState :: !MoveState
  }
  deriving (Generic)

compactButtons :: EditorViewItem a f n -> [M.View (Model a f) (Action a)]
compactButtons item =
  compactEditButtons item.editState item.item
    <> compactDeleteButtons item.deleteState item.item
    <> compactMoveButtons item.moveState item.item

compactEditButtons :: EditState -> a -> [M.View (Model a f) (Action a)]
compactEditButtons EditingNotAvailable _ = []
compactEditButtons (NotEditableBecauseLocked _) _ = []
compactEditButtons NotEditing _ = []
compactEditButtons Editing _ = []

compactDeleteButtons :: DeleteState -> a -> [M.View (Model a f) (Action a)]
compactDeleteButtons DeleteNotAvailable _ = []
compactDeleteButtons Deletable _ = []

compactMoveButtons :: MoveState -> a -> [M.View (Model a f) (Action a)]
compactMoveButtons MovingNotAvailable _ = []
compactMoveButtons NotMoving _ = []
compactMoveButtons MoveSource _ = []
compactMoveButtons PotentialMoveTarget _ = []

type EditorView a f n = EditorViewData a f n -> M.View (Model a f) (Action a)
