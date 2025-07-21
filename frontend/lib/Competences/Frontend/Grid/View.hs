module Competences.Frontend.Grid.View
  ( viewState
  )
where

import Competences.Frontend.Common.Button (iconLabelButton)
import Competences.Frontend.Common.Icon
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (State (..))
import Competences.Frontend.Grid.View.Editable (editable)
import Competences.Frontend.Grid.View.ViewCtx
import Competences.Model.ChangableField (ChangableField (..))
import Miso qualified as M

viewState :: State -> M.View Action
viewState s =
  case mkViewCtx s of
    Just ctx -> runViewM ctx viewModel
    Nothing -> M.div_ [] [M.text "No data available."]

viewModel :: ViewM (M.View Action)
viewModel = do
  title <- editable [styledClass ClsTitle] CompetenceGridTitle
  description <- editable [styledClass ClsDescription] CompetenceGridDescription
  competences <- viewCompetences
  let competenceAddButton = iconLabelButton [M.onClick $ PushModal competenceEditor] IcnAdd "Add competence"
  pure $ M.div_ [] [iconDefs, title, description, competences, competenceAddButton]

viewCompetences :: ViewM (M.View Action)
viewCompetences = pure $ M.div_ [] []
