module Competences.Frontend.Grid.App (mkApp, grid, runApp) where

import Competences.Command (Command (..), handleCommand)
import Competences.Frontend.App.ComponentRegistry
  ( ComponentRegistry
  , handleChannelMessages
  , loadModel
  )
import Competences.Frontend.App.Topics (componentAction)
import Competences.Frontend.Common.Style (styleSheet)
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (State (..))
import Competences.Frontend.Grid.View (viewState)
import Competences.Model (Model (..), emptyModel, fieldATraversal)
import Competences.Model.User (User (..))
import Data.IORef (IORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , Effect
  , LogLevel (..)
  , component
  , consoleLog
  , defaultEvents
  , focus
  , get
  , io_
  , modify
  , startComponent, io
  )
import Miso.String (ms)
import Optics.Core ((%~), (&), (.~))
import Optics.Core qualified as O

runApp :: Component State Action -> JSM ()
runApp = startComponent

grid :: IORef ComponentRegistry -> State -> Component State Action
grid = mkApp

mkApp :: IORef ComponentRegistry -> State -> Component State Action
mkApp reg initialState =
  (component initialState (updateState reg) viewState)
    { mailbox = handleChannelMessages @Action
    }

-- Component
--   { model = initialState
--   , update = updateState
--   , view = viewState
--   , subs = []
--   , styles = [Sheet styleSheet]
--   , scripts = []
--   , events = defaultEvents
--   , initialAction = Just $ Init [LoadEmptyModel]
--   , mountPoint = Nothing
--   , logLevel = Off
--   }

updateState :: IORef ComponentRegistry -> Action -> Effect State Action
updateState _ (ChangeApp cmd) = do
  io_ $ consoleLog "ChangeApp"
  componentAction cmd
updateState _ (EditField f t) = do
  modify $ \state -> state {editFields = Map.insert f t state.editFields}
  io_ $ focus $ ms $ show f
updateState r (LoadModel c) = loadModel r c
updateState _ (UpdateModel m) = do
  io_ $ consoleLog "UpdateModel"
  modify $ \state ->
    state
      & (#model .~ m)
      & (#editFields %~ (updateEditFields state.user.id))
    where
      updateEditFields u fs =
        Map.union fs (editFields u)
      editFields u =
        Map.fromList
          [ (f, fromMaybe "" $ O.preview (fieldATraversal f) m)
          | f <-
              Map.toList m.lockedFields
                & filter (\(_, u') -> u == u')
                & map fst
          ]

handleFailure :: Command -> Effect State Action
handleFailure _ = pure ()

handleSuccess :: Command -> Model -> Effect State Action
handleSuccess (LockField f _ _) m = do
  modify $ \s -> s {editFields = Map.insert f (fromMaybe "" $ O.preview (fieldATraversal f) m) s.editFields}
  io_ $ focus $ ms $ show f
handleSuccess (ReleaseField f _) _ = modify $ \s -> s {editFields = Map.delete f s.editFields}
handleSuccess _ _ = pure ()
