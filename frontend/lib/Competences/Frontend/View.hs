module Competences.Frontend.View
  ( viewState
  )
where

import Competences.Command (Command (..))
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
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.ChangableField (ChangableField(..))

data ViewCtx = ViewCtx
  { renderModel :: !Model
  , state :: !State
  }

type ViewM = Reader ViewCtx

runViewM :: ViewCtx -> ViewM a -> a
runViewM ctx a = runReader a ctx

withCtx :: (ViewCtx -> ViewM a) -> ViewM a
withCtx f = ask >>= f

mkViewCtx :: State -> Maybe ViewCtx
mkViewCtx s = do
  renderModel <- modelOf s
  pure $ ViewCtx { renderModel = renderModel, state = s}

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
  -- btns <- editableBtns field
  pure $ M.span_ attrs $ [M.text text] -- <> [btns]

editableBtns :: ChangableField -> ViewM (M.View Action)
editableBtns f = withCtx $ \ctx -> do undefined
  -- case ctx.canEdit f of
  --   Editable ->
  --     pure $
  --       M.span_
  --         []
  --         [ iconButton [M.onClick (Trigger $ FieldLocked f ctx.state.myUserId)] IcnEdit "Edit"
  --         ]
