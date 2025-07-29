module Competences.Frontend.App
  ( runApp
  , mkApp
  )
where

import Competences.Frontend.App.Action (Action (..), UiAction (..))
import Competences.Frontend.App.RegisteredComponent (RegisteredComponent (..))
import Competences.Frontend.App.RegisteredComponentRegistrations (mkRegisteredComponent)
import Competences.Frontend.App.State
  ( SessionState (..)
  , State (..)
  , UiState (..)
  )
import Competences.Frontend.App.Topics (changeUiTopic)
import Competences.Frontend.Common.Style (ClassName (..), styleSheet, styledClass)
import Competences.Frontend.SyncDocument (SyncDocumentRef, mkSyncDocument)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , LogLevel (Off)
  , NS (HTML)
  , View (VComp)
  , consoleLog
  , defaultEvents
  , div_
  , io_
  , issue
  , key_
  , modify
  , startComponent
  , subscribe
  , text
  )
import Miso.Effect (Effect)
import Miso.String (MisoString, ms)
import Optics.Core ((%), (%~), (&), (.~), (^.))
import System.Random (StdGen, splitGen)

type App = Component State Action

runApp :: Component State Action -> JSM ()
runApp = startComponent

mkApp :: State -> JSM App
mkApp initialState = do
  r <- mkSyncDocument
  pure $
    Component
      { model = initialState
      , update = updateState
      , view = viewState r
      , subs = []
      , styles = [Sheet styleSheet]
      , events = defaultEvents
      , scripts = []
      , initialAction = Just Initialize
      , mountPoint = Nothing
      , mailbox = const Nothing
      , logLevel = Off
      }

updateState :: Action -> Effect State Action
updateState Initialize = do
  subscribe changeUiTopic ChangeUi LogError
  issue $ ChangeUi $ SetMain MainGrid
updateState (ChangeUi (PushModal c)) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #modal) %~ ((k, c, g) :)
updateState (ChangeUi PopModal) = modify $ \s ->
  s & (#uiState % #modal %~ drop 1)
updateState (ChangeUi (SetMain c)) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #main .~ Just (k, c, g))
updateState (LogError msg) = io_ $ consoleLog msg

withUiKey :: ((State, MisoString, StdGen) -> State) -> Effect State Action
withUiKey f = modify $ \s ->
  let key = ms $ "dynamic-component-" <> show (s ^. #uiState % #nextKeyIn)
      (g, g') = splitGen $ s ^. #sessionState % #random
   in f (s & (#uiState % #nextKeyIn %~ (+ 1)) & (#sessionState % #random .~ g), key, g')

viewState :: SyncDocumentRef -> State -> View Action
viewState r s =
  case s ^. #uiState % #main of
    Nothing -> div_ [] [text "Initialize"]
    Just (k, c, g) ->
      let c' = mkRegisteredComponent r s g c
       in div_ [styledClass ClsApp] [VComp HTML "div" [key_ k] c']
