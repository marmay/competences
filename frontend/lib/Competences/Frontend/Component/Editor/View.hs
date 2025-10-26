module Competences.Frontend.Component.Editor.View
  ( EditorView
  , EditorViewData (..)
  , EditorViewItem (..)
  , EditState (..)
  , DeleteState (..)
  , MoveState (..)
  , ViewButtonStyle (..)
  , compactButtons
  , extendedButtons
  , buttons
  , refocusTargetString
  )
where

import Competences.Document (User)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.Types (Action (..), Model, Reorder' (..))
import Competences.Frontend.View qualified as V
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))

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

type EditorView a f n = EditorViewData a f n -> M.View (Model a f) (Action a)

compactButtons :: EditorViewItem a f n -> [M.View (Model a f) (Action a)]
compactButtons = buttons Compact

extendedButtons :: EditorViewItem a f n -> [M.View (Model a f) (Action a)]
extendedButtons = buttons Extended

data ViewButtonStyle
  = Compact
  | Extended
  deriving (Eq, Show)

buttons :: ViewButtonStyle -> EditorViewItem a f n -> [M.View (Model a f) (Action a)]
buttons s item =
  case (item.editState, item.moveState, item.deleteState) of
    (_, MoveSource, _) ->
      [V.viewButtons (buttonLayout s) [moveToTopButton s a, cancelMoveButton s a, moveToBottomButton s a]]
    (_, PotentialMoveTarget, _) ->
      [V.viewButtons (buttonLayout s) [moveBeforeButton s a, moveAfterButton s a]]
    (Editing, _, _) -> [V.viewButtons (buttonLayout s) [finishEditButton s a, cancelEditButton s a]]
    (e, m, d) -> [V.viewButtons (buttonLayout s) (concat [editButtons e, moveButtons m, deleteButtons d])]

  where
    a = item.item
    editButtons NotEditing = [editButton s a]
    editButtons _ = []
    moveButtons NotMoving = [moveButton s a]
    moveButtons _ = []
    deleteButtons Deletable = [deleteButton s a]
    deleteButtons _ = []

buttonLayout :: ViewButtonStyle -> V.Buttons
buttonLayout Compact = V.hButtons & #compact .~ True
buttonLayout Extended = V.hButtons

editButton
  , finishEditButton
  , cancelEditButton
  , deleteButton
  , moveButton
  , cancelMoveButton
  , moveBeforeButton
  , moveAfterButton
  , moveToTopButton
  , moveToBottomButton
    :: ViewButtonStyle -> a -> V.Button () (Action a)
editButton s a = V.contentsButton (mkContents s V.IcnEdit C.LblEdit) () (StartEditing a)
finishEditButton s a = V.contentsButton (mkContents s V.IcnApply C.LblApply) () (FinishEditing a)
cancelEditButton s a = V.contentsButton (mkContents s V.IcnCancel C.LblCancel) () (CancelEditing a)
                         & (#buttonType .~ V.AlertButton)
deleteButton s a = V.contentsButton (mkContents s V.IcnDelete C.LblDelete) () (Delete a)
                         & (#buttonType .~ V.AlertButton)
moveButton s a = V.contentsButton (mkContents s V.IcnReorder C.LblMove) () (StartMoving a)
cancelMoveButton s _ = V.contentsButton (mkContents s V.IcnCancel C.LblCancel) () CancelMoving
                         & (#buttonType .~ V.AlertButton)
moveBeforeButton s a =
  V.contentsButton (mkContents s V.IcnArrowUp C.LblInsertBefore) () (FinishMoving (Before' a))
moveAfterButton s a =
  V.contentsButton (mkContents s V.IcnArrowDown C.LblInsertAfter) () (FinishMoving (After' a))
moveToTopButton s _ =
  V.contentsButton (mkContents s V.IcnDoubleArrowUp C.LblInsertAtTop) () (FinishMoving Front')
moveToBottomButton s _ =
  V.contentsButton
    (mkContents s V.IcnDoubleArrowDown C.LblInsertAtBottom)
    ()
    (FinishMoving Back')

mkContents :: ViewButtonStyle -> V.Icon -> C.Label -> V.ButtonContents
mkContents Compact icn lbl = V.ButtonIcon icn (C.translate' lbl)
mkContents Extended icn lbl = V.ButtonIconAndText icn (C.translate' lbl)

refocusTargetString :: M.MisoString
refocusTargetString = "editor-refocus-target"
