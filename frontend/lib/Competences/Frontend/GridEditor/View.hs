module Competences.Frontend.GridEditor.View
  ( viewState
  )
where

import Competences.Document.ChangableField (ChangableField (..))
import Competences.Frontend.Common.Button (iconLabelButton)
import Competences.Frontend.Common.Icon
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.Component.CompetenceEditor qualified as CE
import Competences.Frontend.GridEditor.Action (Action (..))
import Competences.Frontend.GridEditor.State (State (..))
import Competences.Frontend.GridEditor.View.Editable (editable)
import Miso qualified as M

viewState :: State -> M.View Action
viewState s =
  let title = editable s [styledClass ClsTitle] CompetenceGridTitle
      description = editable s [styledClass ClsDescription] CompetenceGridDescription
      competences = viewCompetences
   in M.div_ [] [iconDefs, title, description, competences, viewNewCompetenceEditor s]

viewCompetences :: M.View Action
viewCompetences = M.div_ [] []

viewNewCompetenceEditor :: State -> M.View Action
viewNewCompetenceEditor s =
  case s.newCompetenceEditor of
    Nothing -> iconLabelButton [M.onClick SpawnNewCompetenceEditor] IcnAdd "New competence"
    Just m -> NewCompetenceEditorAction <$> CE.view m
