module Competences.Frontend.App.RegisteredComponentRegistrations
  ( mkRegisteredComponent
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent (..))
import Competences.Frontend.App.State
import Competences.Frontend.GridEditor.App (grid)
import Competences.Frontend.GridEditor.State qualified as G
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Miso (SomeComponent (..))
import System.Random (StdGen)
import Competences.Frontend.Page.EditCompetenceGridPage (editCompetenceGridPage)

mkRegisteredComponent
  :: SyncDocumentRef -> State -> StdGen -> RegisteredComponent -> SomeComponent
mkRegisteredComponent r s g MainGrid =
  -- SomeComponent $ grid r $ G.mkState s.sessionState.user s.sessionState.translationData g
  SomeComponent $ editCompetenceGridPage r g s.sessionState.user s.sessionState.translationData
