module Competences.Frontend.GridEditor.App (mkApp, grid, runApp) where

import Competences.Command (Command (..))
import Competences.Document (Document (..), fieldATraversal, orderMax)
import Competences.Document.Competence (Competence (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common.Random (random')
import Competences.Frontend.Common.Style (styleSheet)
import Competences.Frontend.Component.CompetenceEditor qualified as CE
import Competences.Frontend.GridEditor.Action (Action (..))
import Competences.Frontend.GridEditor.State (State (..))
import Competences.Frontend.GridEditor.View (viewState)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , Effect
  , ask
  , component
  , consoleLog
  , focus
  , get
  , io_
  , modify
  , put
  , runEffect
  , startComponent
  , tell
  )
import Miso.String (ms)
import Optics.Core (Lens', (%~), (&), (.~), (^.))
import Optics.Core qualified as O
import Competences.Frontend.Common.Effect (liftEffect')

runApp :: Component State Action -> JSM ()
runApp = startComponent

grid :: SyncDocumentRef -> State -> Component State Action
grid = mkApp

mkApp :: SyncDocumentRef -> State -> Component State Action
mkApp docRef initialState =
  (component initialState (updateState docRef) viewState)
    { styles = [Sheet styleSheet]
    , subs = [subscribeDocument docRef UpdateDocument]
    }

updateState :: SyncDocumentRef -> Action -> Effect State Action
updateState _ (EditField f t) = do
  modify $ \state -> state {editFields = Map.insert f t state.editFields}
  io_ $ focus $ ms $ show f
updateState _ (UpdateDocument (DocumentChange m _)) = do
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
updateState docRef (IssueCommand cmd) = do
  io_ $ modifySyncDocument docRef cmd
updateState _ SpawnNewCompetenceEditor = do
  competenceId <- random'
  modify $ \s ->
    let newCompetence =
          Competence
            { id = competenceId
            , order = orderMax
            , competenceGridId = s.model.competenceGrid.id
            , description = ""
            , levelDescriptions = M.empty
            }
     in s & (#newCompetenceEditor .~ Just (CE.Model newCompetence s.translationData))
updateState docRef (NewCompetenceEditorAction CE.CompleteEditing) = do
  s :: State <- get
  case s.newCompetenceEditor of
    (Just e) -> io_ $ modifySyncDocument docRef (AddCompetence e.competence)
    Nothing -> pure ()
  modify $ (#newCompetenceEditor .~ Nothing)
updateState _ (NewCompetenceEditorAction CE.CancelEditing) =
  modify $ (#newCompetenceEditor .~ Nothing)
updateState _ (NewCompetenceEditorAction a) = do
  liftEffect' #newCompetenceEditor NewCompetenceEditorAction $ CE.update a
updateState _ (Log s) = io_ $ consoleLog s

