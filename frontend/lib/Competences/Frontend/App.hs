module Competences.Frontend.App
  ( runApp
  , mkApp
  , run
  , withTailwindPlay
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
import Competences.Frontend.SyncDocument (SyncDocumentRef, issueInitialUpdate)
import Competences.Frontend.View (iconDefs)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( Component (..)
  , JS (..)
  , LogLevel (Off)
  , NS (HTML)
  , ROOT
  , View (VComp)
  , consoleLog
  , defaultEvents
  , div_
  , io_
  , issue
  , key_
  , modify
  , onMounted
  , run
  , startComponent
  , subscribe
  , text
  )
import Miso.Effect (Effect)
import Miso.String (MisoString, ms)
import Optics.Core ((%), (%~), (&), (.~), (^.), (?~))
import System.Random (StdGen, splitGen)
import qualified Miso as M
import Competences.Frontend.Page.EditCompetenceGridPage (editCompetenceGridPage, EditCompetenceGridPage)

type App = EditCompetenceGridPage ROOT

runApp :: App -> JSM ()
runApp = startComponent

mkApp :: SyncDocumentRef -> State -> JSM App
mkApp r initialState = pure $ editCompetenceGridPage r initialState.sessionState.user
  -- pure $ M.component initialState (updateState r) (viewState r)

withTailwindPlay :: App -> App
withTailwindPlay app = app {scripts = Src "https://cdn.tailwindcss.com" : scripts app}

updateState :: SyncDocumentRef -> Action -> Effect p State Action
updateState _ Initialize = do
  subscribe changeUiTopic ChangeUi LogError
  issue $ ChangeUi $ SetMain MainGrid
updateState _ (ChangeUi (PushModal c)) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #modal) %~ ((k, c, g) :)
updateState _ (ChangeUi PopModal) = modify $ \s ->
  s & (#uiState % #modal %~ drop 1)
updateState _ (ChangeUi (SetMain c)) = withUiKey $ \(s, k, g) ->
  s & (#uiState % #main ?~ (k, c, g))
updateState _ (LogError msg) = io_ $ consoleLog msg
updateState r Mounted = io_ $ issueInitialUpdate r

withUiKey :: ((State, MisoString, StdGen) -> State) -> Effect p State Action
withUiKey f = modify $ \s ->
  let key = ms $ "dynamic-component-" <> show (s ^. #uiState % #nextKeyIn)
      (g, g') = splitGen $ s ^. #sessionState % #random
   in f (s & (#uiState % #nextKeyIn %~ (+ 1)) & (#sessionState % #random .~ g), key, g')

viewState :: SyncDocumentRef -> State -> View m Action
viewState r s =
  div_
    []
    [ iconDefs
    , case s ^. #uiState % #main of
        Nothing -> div_ [] [text "Initialize"]
        Just (k, c, g) ->
          div_ [] [text "Initialization done."]
          -- let c' = mkRegisteredComponent r s g c
          --  in div_ [onMounted Mounted] [VComp HTML "div" [key_ k] c']
    ]
