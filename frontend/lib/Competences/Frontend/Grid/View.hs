module Competences.Frontend.Grid.View
  ( viewState
  )
where

import Competences.Frontend.Common.Button (iconLabelButton)
import Competences.Frontend.Common.Icon
import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (NewCompetenceData (..), State (..))
import Competences.Frontend.Grid.View.Editable (editable)
import Competences.Document.ChangableField (ChangableField (..))
import Miso qualified as M

viewState :: State -> M.View Action
viewState s =
  let title = editable s [styledClass ClsTitle] CompetenceGridTitle
      description = editable s [styledClass ClsDescription] CompetenceGridDescription
      competences = viewCompetences
      newCompetenceBox = case s.newCompetenceData of
        Nothing -> iconLabelButton [M.onClick NewCompetence] IcnAdd "New competence"
        Just newCompetenceData -> viewNewCompetenceData newCompetenceData
   in M.div_ [] [iconDefs, title, description, competences, newCompetenceBox]

viewCompetences :: M.View Action
viewCompetences = M.div_ [] []

viewNewCompetenceData :: NewCompetenceData -> M.View Action
viewNewCompetenceData n =
  M.div_
    [styledClass ClsNewCompetenceRow]
    [ M.span_
        [styledClass ClsCompetenceDescription]
        [ M.input_
            [ M.onInput SetNewCompetenceDescription
            , M.value_ n.description
            ]
        ]
    , iconLabelButton [M.onClick CancelNewCompetence] IcnCancel "Cancel"
    , iconLabelButton [M.onClick AddNewCompetence] IcnAdd "Add"
    ]
