module Competences.Frontend.App
  ( runApp
  , mkApp
  )
where

import Competences.Command (handleCommand)
import Competences.Frontend.App.Action (Action (..), ComponentAction (..), UiAction (..))
import Competences.Frontend.App.ComponentRegistry (ComponentRegistry, mkComponentRegistry, updateModel)
import Competences.Frontend.App.RegisteredComponent (RegisteredComponent (..))
import Competences.Frontend.App.RegisteredComponentRegistrations (mkRegisteredComponent)
import Competences.Frontend.App.State
  ( LocalModelState (..)
  , ModelState (..)
  , SessionState (..)
  , State (..)
  , UiState (..)
  )
import Competences.Frontend.App.Topics (changeApplicationStateTopic, componentAction)
import Competences.Frontend.Common.Style (ClassName (..), styleSheet, styledClass)
import Data.Aeson (Result (..))
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , LogLevel (Off)
  , NS (HTML)
  , SomeComponent (..)
  , View (VComp)
  , consoleLog
  , defaultEvents
  , div_
  , get
  , io_
  , issue
  , key_
  , modify
  , p_
  , put
  , startComponent
  , subscribe
  , text
  , (+>), onClick, component, io, withSink
  )
import Miso.Effect (Effect)
import Miso.String (MisoString, ms)
import Optics.Core ((%), (%~), (&), (.~), (^.))
import System.Random (StdGen, splitGen)
import Miso.Html (button_)
import Control.Monad.IO.Class (liftIO)

type App = Component State Action

runApp :: Component State Action -> JSM ()
runApp c = startComponent $ component () (const $ pure ()) $ \() ->
  div_ [] +> c

mkApp :: State -> IO App
mkApp initialState = do
  r <- mkComponentRegistry >>= newIORef
  pure $
    Component
      { model = initialState
      , update = updateState r
      , view = viewState r
      , subs = []
      , styles = [Sheet styleSheet]
      , events = defaultEvents
      , scripts = []
      , initialAction = Nothing
      , mountPoint = Nothing
      , mailbox = const Nothing
      , logLevel = Off
      }

updateState :: IORef ComponentRegistry -> Action -> Effect State Action
updateState _ Initialize = do
  io_ $ consoleLog "Initializing ..."
  subscribe changeApplicationStateTopic handleChangeApplicationState
  componentAction $ ChangeUi $ PushModal MainGrid
  issue $ ComponentAction $ ChangeUi $ SetMain MainGrid
updateState reg (ComponentAction (Trigger cmd)) = do
  io_ $ consoleLog $ "Triggering command: " <> ms (show cmd)
  s <- get
  let localModelState = fromMaybe (LocalModelState s.modelState.remoteModel []) s.modelState.localModelState
  case handleCommand cmd localModelState.localModel of
    Left err -> do
      issue $ LogError $ "Could not handle command: " <> ms err
      pure ()
    Right (model', _) -> do
      -- io_ $ liftIO $ updateModel reg model'
      let updatedModelState =
            localModelState
              & (#localModel .~ model')
              & (#localChanges %~ (cmd :))
      put $ s & (#modelState % #localModelState .~ Just updatedModelState)
updateState _ (ComponentAction (ChangeUi (PushModal c))) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #modal) %~ ((k, c, g) :)
updateState _ (ComponentAction (ChangeUi PopModal)) = modify $ \s ->
  s & (#uiState % #modal %~ drop 1)
updateState _ (ComponentAction (ChangeUi (SetMain c))) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #main .~ Just (k, c, g))
updateState _ (Process cmd) = pure ()
updateState _ (LogError msg) = io_ $ consoleLog msg

withUiKey :: ((State, MisoString, StdGen) -> State) -> Effect State Action
withUiKey f = modify $ \s ->
  let key = ms $ "dynamic-component-" <> show (s ^. #uiState % #nextKeyIn)
      (g, g') = splitGen $ s ^. #sessionState % #random
   in f (s & (#uiState % #nextKeyIn %~ (+ 1)) & (#sessionState % #random .~ g), key, g')

viewState :: IORef ComponentRegistry -> State -> View Action
viewState r s =
  case s ^. #uiState % #main of
    Nothing -> div_ [] [button_ [onClick Initialize] [text "Initialize"]]
    Just (k, c, g) ->
      let c' = mkRegisteredComponent r s g c
       in div_ [styledClass ClsApp] [VComp HTML "div" [key_ k] c']

handleChangeApplicationState :: Result ComponentAction -> Action
handleChangeApplicationState (Error e) = LogError $ "Received invalid payload via changeApplicationStateTopic: " <> ms e
handleChangeApplicationState (Success a) = ComponentAction a
