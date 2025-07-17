module Competences.Frontend.View
  ( viewState
  )
where

import Competences.Event (ChangableField (..), Event (..))
import Competences.Frontend.Action (Action (..))
import Competences.Frontend.State (State (..), modelOf)
import Competences.Model (Model (..))
import Competences.Model.CompetenceGrid (CompetenceGrid (..))
import Competences.Model.User (UserId)
import Miso qualified as M
import Miso.String qualified as M
import Control.Monad.Reader
import Competences.Frontend.View.Icon
import Competences.Frontend.View.Button (iconButton)

data ViewCtx = ViewCtx
  { renderModel :: !Model
  , canEdit :: !(ChangableField -> Editability)
  , state :: !State
  }

data Editability
  = Editable
  | LockedBy !UserId
  | ThisLocked !M.MisoString
  | OtherLocked
  | NotEditable

type ViewM = Reader ViewCtx

runViewM :: ViewCtx -> ViewM a -> a
runViewM ctx a = runReader a ctx

withCtx :: (ViewCtx -> ViewM a) -> ViewM a
withCtx f = ask >>= f

mkViewCtx :: State -> Maybe ViewCtx
mkViewCtx s = do
  renderModel <- modelOf s
  pure $ ViewCtx { renderModel = renderModel, canEdit = const Editable, state = s}

viewState :: State -> M.View Action
viewState s =
  case mkViewCtx s of
    Just ctx -> runViewM ctx viewModel
    Nothing -> M.div_ [] [M.text "No data available."]

viewModel :: ViewM (M.View Action)
viewModel = withCtx $ \ctx -> do
  title <- editable [] (ctx.renderModel.competenceGrid.title) "No title" CompetenceGridTitle
  pure $ M.div_ [] [iconDefs, title]

editable :: [M.Attribute Action] -> M.MisoString -> M.MisoString -> ChangableField -> ViewM (M.View Action)
editable attrs text defaultText field = do
  btns <- editableBtns field
  pure $ M.span_ attrs $ [M.text text] <> [btns]

editableBtns :: ChangableField -> ViewM (M.View Action)
editableBtns f = withCtx $ \ctx -> do
  case ctx.canEdit f of
    Editable ->
      pure $
        M.span_
          []
          [ iconButton [M.onClick (Trigger $ FieldLocked f ctx.state.myUserId)] IcnEdit "Edit"
          ]
