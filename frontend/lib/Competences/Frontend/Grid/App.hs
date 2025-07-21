module Competences.Frontend.Grid.App (mkApp, runApp) where

import Competences.Command (Command (..), handleCommand)
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (State (..))
import Competences.Frontend.Grid.View (viewState)
import Competences.Frontend.Common.Style (styleSheet)
import Competences.Model (Model, emptyModel, fieldATraversal)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , Effect
  , LogLevel (..)
  , consoleLog
  , defaultEvents
  , focus
  , get
  , io_
  , modify
  , startComponent,
  )
import Miso.String (ms)
import Optics.Core qualified as O

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
    , initialAction = Just $ Init [LoadEmptyModel]
    , mountPoint = Nothing
    , logLevel = Off
    }

updateState :: Action -> Effect State Action
updateState LoadEmptyModel = modify $ \state -> state {serverModel = Just emptyModel, localModel = Just emptyModel}
updateState (Trigger cmd) = do
  io_ $ consoleLog $ ms $ show cmd
  handleCommand' cmd
updateState (EditField f t) = do
  modify $ \state -> state {editFields = Map.insert f t state.editFields}
  io_ $ focus $ ms $ show f
updateState _ = pure ()

handleCommand' :: Command -> Effect State Action
handleCommand' cmd = do
  state <- get
  case state.localModel of
    Nothing -> pure ()
    Just oldModel ->
      case handleCommand cmd oldModel of
        Left _ -> handleFailure cmd
        Right (m, _) -> do
          modify $ \s -> s{localModel = Just m}
          handleSuccess cmd oldModel

handleFailure :: Command -> Effect State Action
handleFailure _ = pure ()

handleSuccess :: Command -> Model -> Effect State Action
handleSuccess (LockField f _ _) m = do
  modify $ \s -> s {editFields = Map.insert f (fromMaybe "" $ O.preview (fieldATraversal f) m) s.editFields}
  io_ $ focus $ ms $ show f
handleSuccess (ReleaseField f _) _ = modify $ \s -> s {editFields = Map.delete f s.editFields}
handleSuccess _ _ = pure ()
