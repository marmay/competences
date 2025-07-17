module Competences.Frontend.App (mkApp, runApp) where

import Competences.Frontend.Action (Action (..))
import Competences.Frontend.State (State (..))
import Competences.Frontend.View (viewState)
import Competences.Frontend.View.Style (styleSheet)
import Competences.Model (emptyModel)
import Language.Javascript.JSaddle (JSM)
import Miso (CSS (..), Component (..), Effect, LogLevel (..), defaultEvents, startComponent)
import Miso.State (modify)

runApp :: Component State Action -> JSM ()
runApp = startComponent

mkApp :: State -> Component State Action
mkApp initialState =
  Component
    { model = initialState
    , update = updateState
    , view = viewState
    , subs = []
    , styles = [Sheet styleSheet]
    , events = defaultEvents
    , initialAction = Just LoadEmptyModel
    , mountPoint = Nothing
    , logLevel = Off
    }

updateState :: Action -> Effect State Action
updateState LoadEmptyModel = modify $ \state -> state {serverModel = Just emptyModel}
updateState _ = pure ()
