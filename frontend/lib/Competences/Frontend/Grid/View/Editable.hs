module Competences.Frontend.Grid.View.Editable
  ( editable
  )
where

import Competences.Command (Command (..))
import Competences.Frontend.App.Action (ComponentAction (..))
import Competences.Frontend.Common.Button (iconButton)
import Competences.Frontend.Common.Icon (Icon (..))
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.Common.Translate (Label (..))
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (State (..))
import Competences.Frontend.Grid.State.Edit (Editability (..), editability)
import Competences.Frontend.Grid.View.ViewCtx
import Competences.Model (fieldATraversal)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (User (..))
import Data.Maybe (fromMaybe)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core qualified as O

editable :: [M.Attribute Action] -> ChangableField -> ViewM (M.View Action)
editable attrs f = withViewCtx $ \ctx ->
  let render :: M.View Action -> [M.View Action] -> ViewM (M.View Action)
      render content buttons =
        pure $
          M.div_
            (styledClass ClsEditableContainer : attrs)
            [ M.span_ [styledClass ClsEditableContent] [content]
            , M.span_ [styledClass ClsEditableButtons] buttons
            ]
      fieldText = fromMaybe "" $ O.preview (fieldATraversal f) ctx.renderModel
      inputField t = M.input_ [M.id_ (ms $ show f), M.value_ (ms t), M.onInput (EditField f)]
      editButton =
        iconButton
          [M.onClick $ ChangeApp $ Trigger (LockField f ctx.state.user.id fieldText)]
          IcnEdit
          (ctx.translate LblEdit)
      applyButton t =
        iconButton
          [M.onClick $ ChangeApp $ Trigger (ReleaseField f (Just t))]
          IcnApply
          (ctx.translate LblApplyChange)
      cancelButton =
        iconButton
          [M.onClick $ ChangeApp $ Trigger (ReleaseField f Nothing)]
          IcnCancel
          (ctx.translate LblCancelChange)
   in case editability ctx.renderModel ctx.state.editFields f ctx.state.user of
        NotEditable -> render (M.text fieldText) []
        Editable -> render (M.text fieldText) [editButton]
        LockedByUs t -> render (inputField t) [applyButton t, cancelButton]
        LockedBy u -> render (M.text fieldText) [] -- [lockedBy u]
