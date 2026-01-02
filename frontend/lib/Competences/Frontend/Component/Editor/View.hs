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
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Layout qualified as Layout
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))

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
  }
  deriving (Generic)

type EditorView a patch f n = EditorViewData a patch f n -> M.View (Model a patch f) (Action a patch)

compactButtons :: EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
compactButtons = buttons Compact

extendedButtons :: EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
extendedButtons = buttons Extended

data ViewButtonStyle
  = Compact
  | Extended
  deriving (Eq, Show)

buttons :: ViewButtonStyle -> EditorViewItem a patch f n -> [M.View (Model a patch f) (Action a patch)]
buttons s item =
  case (item.editState, item.moveState, item.deleteState) of
    (_, MoveSource, _) ->
      [buttonRow s [moveToTopButton s a, cancelMoveButton s a, moveToBottomButton s a]]
    (_, PotentialMoveTarget, _) ->
      [buttonRow s [moveBeforeButton s a, moveAfterButton s a]]
    (Editing, _, _) -> [buttonRow s [finishEditButton s a, cancelEditButton s a]]
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
buttonRow :: ViewButtonStyle -> [M.View m a] -> M.View m a
buttonRow Compact btns = Button.buttonGroup btns
buttonRow Extended btns = Layout.viewFlow (Layout.hFlow & (#gap .~ Layout.SmallSpace)) btns

-- Button helper that creates icon-only or icon+text based on style
mkButton
  :: Button.ButtonVariant
  -> ViewButtonStyle
  -> Icon
  -> C.Label
  -> action
  -> M.View model action
mkButton variant Compact icn _lbl action =
  Button.buttonIcon variant icn
    & Button.withClick action
    & Button.renderButton
mkButton variant Extended icn lbl action =
  Button.button variant (C.translate' lbl)
    & Button.withIcon icn
    & Button.withClick action
    & Button.renderButton

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
    :: ViewButtonStyle -> a -> M.View (Model a patch f) (Action a patch)
editButton s a = mkButton Button.Secondary s IcnEdit C.LblEdit (StartEditing a)
finishEditButton s a = mkButton Button.Primary s IcnApply C.LblApply (FinishEditing a)
cancelEditButton s a = mkButton Button.Destructive s IcnCancel C.LblCancel (CancelEditing a)
deleteButton s a = mkButton Button.Destructive s IcnDelete C.LblDelete (Delete a)
moveButton s a = mkButton Button.Secondary s IcnReorder C.LblMove (StartMoving a)
cancelMoveButton s _ = mkButton Button.Destructive s IcnCancel C.LblCancel CancelMoving
moveBeforeButton s a = mkButton Button.Secondary s IcnArrowUp C.LblInsertBefore (FinishMoving (Before' a))
moveAfterButton s a = mkButton Button.Secondary s IcnArrowDown C.LblInsertAfter (FinishMoving (After' a))
moveToTopButton s _ = mkButton Button.Secondary s IcnDoubleArrowUp C.LblInsertAtTop (FinishMoving Front')
moveToBottomButton s _ = mkButton Button.Secondary s IcnDoubleArrowDown C.LblInsertAtBottom (FinishMoving Back')

refocusTargetString :: M.MisoString
refocusTargetString = "editor-refocus-target"
