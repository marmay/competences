module Competences.Frontend.GridEditor.View
  ( viewState
  )
where

import Competences.Document.ChangableField (ChangableField (..))
import Competences.Document.Competence (Competence)
import Competences.Frontend.Common.Button (iconLabelButton)
import Competences.Frontend.Common.Icon
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.CompetenceEditor (competenceEditor)
import Competences.Frontend.GridEditor.Action (Action (..), InMail (..))
import Competences.Frontend.GridEditor.State (State (..))
import Competences.Frontend.GridEditor.View.Editable (editable)
import Miso ((+>))
import Miso qualified as M

viewState :: State -> M.View Action
viewState s =
  let title = editable s [styledClass ClsTitle] CompetenceGridTitle
      description = editable s [styledClass ClsDescription] CompetenceGridDescription
      competences = viewCompetences
   in M.div_ [] [iconDefs, title, description, competences, viewNewCompetenceData s s.newCompetence]

viewCompetences :: M.View Action
viewCompetences = M.div_ [] []

viewNewCompetenceData :: State -> Maybe Competence -> M.View Action
viewNewCompetenceData _ Nothing = iconLabelButton [M.onClick NewCompetenceEditor] IcnAdd "New competence"
viewNewCompetenceData s (Just c) =
  M.div_ [M.onMountedWith NewCompetenceEditorMounted] +> competenceEditor c s.translationData CompetenceEditor
