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
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import GHC.Generics (Generic)
import Miso qualified as M


data EditorViewData a patch f n = EditorViewData
  { fields :: ![n]
  , items :: !(f (EditorViewItem a patch f n))
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

data EditorViewItem a patch f n = EditorViewItem
  { item :: !a
  , fieldData :: ![(n, M.View (Model a patch f) (Action a patch))]
  , editState :: !EditState
  , deleteState :: !DeleteState
  , moveState :: !MoveState
  , canApply :: !Bool
  }
  deriving (Generic)

type EditorView a patch f n = EditorViewData a patch f n -> M.View (Model a patch f) (Action a patch)

compactButtons :: EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
compactButtons = buttons Button.IconOnlyS

extendedButtons :: EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
extendedButtons = buttons Button.IconTextS

data ViewButtonStyle
  = Compact
  | Extended
  deriving (Eq, Show)

buttons :: Button.ButtonContentsStyle -> EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
buttons s item =
  case (item.editState, item.moveState, item.deleteState) of
    (_, MoveSource, _) ->
      [buttonRow s [moveToTopButton s a, cancelMoveButton s a, moveToBottomButton s a]]
    (_, PotentialMoveTarget, _) ->
      [buttonRow s [moveBeforeButton s a, moveAfterButton s a]]
    (Editing, _, _) ->
      [buttonRow s $ [finishEditButton s a | item.canApply] ++ [cancelEditButton s a]]
    (e, m, d) -> [buttonRow s (concat [editButtons e, moveButtons m, deleteButtons d])]

  where
    a = item.item
    editButtons NotEditing = [editButton s a]
    editButtons _ = []
    moveButtons NotMoving = [moveButton s a]
    moveButtons _ = []
    deleteButtons Deletable = [deleteButton s a]
    deleteButtons _ = []

-- | Render a row of buttons using appropriate layout
-- Compact mode uses buttonGroup (connected edges), Extended uses flow with gap
buttonRow :: Button.ButtonContentsStyle -> [M.View m a] -> M.View m a
buttonRow Button.IconTextS btns = Layout.hFlow Layout.gapS btns
buttonRow _ btns = Button.buttonGroup btns

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
    :: forall a patch f. Button.ButtonContentsStyle -> a -> M.View (Model a patch f) (Action a patch)
editButton s a = Button.secondary (Button.button (s, Icon.IcnEdit, C.LblEdit) (StartEditing a :: Action a patch))
finishEditButton s a = Button.primary (Button.button (s, Icon.IcnApply, C.LblApply) (FinishEditing a :: Action a patch))
cancelEditButton s a = Button.destructive (Button.button (s, Icon.IcnCancel, C.LblCancel) (CancelEditing a :: Action a patch))
deleteButton s a = Button.destructive (Button.button (s, Icon.IcnDelete, C.LblDelete) (Delete a :: Action a patch))
moveButton s a = Button.secondary (Button.button (s, Icon.IcnReorder, C.LblMove) (StartMoving a :: Action a patch))
cancelMoveButton s _ = Button.destructive (Button.button (s, Icon.IcnCancel, C.LblCancel) (CancelMoving :: Action a patch))
moveBeforeButton s a = Button.secondary (Button.button (s, Icon.IcnArrowUp, C.LblInsertBefore) (FinishMoving (Before' a) :: Action a patch))
moveAfterButton s a = Button.secondary (Button.button (s, Icon.IcnArrowDown, C.LblInsertAfter) (FinishMoving (After' a) :: Action a patch))
moveToTopButton s _ = Button.secondary (Button.button (s, Icon.IcnDoubleArrowUp, C.LblInsertAtTop) (FinishMoving Front' :: Action a patch))
moveToBottomButton s _ = Button.secondary (Button.button (s, Icon.IcnDoubleArrowDown, C.LblInsertAtBottom) (FinishMoving Back' :: Action a patch))

refocusTargetString :: M.MisoString
refocusTargetString = "editor-refocus-target"
