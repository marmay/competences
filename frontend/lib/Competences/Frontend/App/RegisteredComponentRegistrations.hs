module Competences.Frontend.App.RegisteredComponentRegistrations
  ( mkRegisteredComponent
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent (..))
import Competences.Frontend.App.State
import Competences.Frontend.Grid.App (grid)
import Competences.Frontend.Grid.State qualified as G
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Miso (SomeComponent (..))
import System.Random (StdGen)

mkRegisteredComponent
  :: SyncDocumentRef -> State -> StdGen -> RegisteredComponent -> SomeComponent
mkRegisteredComponent r s g MainGrid =
  SomeComponent $ grid r $ G.mkState s.sessionState.user s.sessionState.translationData g
