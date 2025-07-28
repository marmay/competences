module Competences.Frontend.Grid.View.Editable
  ( editable
  )
where

import Competences.Command (Command (..))
import Competences.Frontend.Common.Button (iconButton)
import Competences.Frontend.Common.Icon (Icon (..))
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.Common.Translate (Label (..), translate)
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (State (..))
import Competences.Frontend.Grid.State.Edit (Editability (..), editability)
import Competences.Model (fieldATraversal)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (User (..))
import Data.Maybe (fromMaybe)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core qualified as O

editable :: State -> [M.Attribute Action] -> ChangableField -> M.View Action
editable s attrs f =
    let render :: M.View Action -> [M.View Action] -> M.View Action
        render content buttons =
          M.div_
            (styledClass ClsEditableContainer : attrs)
            [ M.span_ [styledClass ClsEditableContent] [content]
            , M.span_ [styledClass ClsEditableButtons] buttons
            ]
        fieldText = fromMaybe "" $ O.preview (fieldATraversal f) s.model
        inputField t = M.input_ [M.id_ (ms $ show f), M.value_ (ms t), M.onInput (EditField f)]
        editButton =
          iconButton
            [M.onClick $ IssueCommand (LockField f s.user.id fieldText)]
            IcnEdit
            (translate s LblEdit)
        applyButton t =
          iconButton
            [M.onClick $ IssueCommand (ReleaseField f (Just t))]
            IcnApply
            (translate s LblApplyChange)
        cancelButton =
          iconButton
            [M.onClick $ IssueCommand (ReleaseField f Nothing)]
            IcnCancel
            (translate s LblCancelChange)
     in case editability s.model s.editFields f s.user of
          NotEditable -> render (M.text fieldText) []
          Editable -> render (M.text fieldText) [editButton]
          LockedByUs t -> render (inputField t) [applyButton t, cancelButton]
          LockedBy _u -> render (M.text fieldText) [] -- [lockedBy u]
