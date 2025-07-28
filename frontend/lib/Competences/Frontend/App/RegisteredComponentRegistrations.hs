module Competences.Frontend.App.RegisteredComponentRegistrations
  ( mkRegisteredComponent
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent (..))
import Competences.Frontend.App.State
import Competences.Frontend.Document (DocumentRef)
import Competences.Frontend.Grid.App (grid)
import Competences.Frontend.Grid.State qualified as G
import Competences.Frontend.Modal.CompetenceEditor (competenceEditor)
import Miso (SomeComponent (..))
import System.Random (StdGen)

mkRegisteredComponent
  :: DocumentRef -> State -> StdGen -> RegisteredComponent -> SomeComponent
mkRegisteredComponent r s g MainGrid =
  SomeComponent $ grid r $ G.mkState s.sessionState.user s.sessionState.translationData g
mkRegisteredComponent _ _ _ (CompetenceEditor competence) =
  SomeComponent $ competenceEditor competence
