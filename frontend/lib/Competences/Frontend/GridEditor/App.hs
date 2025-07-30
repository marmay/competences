module Competences.Frontend.GridEditor.App (mkApp, grid, runApp) where

import Competences.Command (Command (..))
import Competences.Document (Document (..), fieldATraversal)
import Competences.Document.Competence (Competence (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common.Random (random')
import Competences.Frontend.Common.Style (styleSheet)
import Competences.Frontend.CompetenceEditor qualified as CE
import Competences.Frontend.GridEditor.Action (Action (..), InMail (..))
import Competences.Frontend.GridEditor.State (State (..))
import Competences.Frontend.GridEditor.View (viewState)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Data.Aeson (Result (..), fromJSON)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , Effect
  , component
  , consoleLog
  , focus
  , io_
  , modify
  , startComponent
  )
import Miso.String (ms)
import Optics.Core ((%~), (&), (.~))
import Optics.Core qualified as O

runApp :: Component State Action -> JSM ()
runApp = startComponent

grid :: SyncDocumentRef -> State -> Component State Action
grid = mkApp

mkApp :: SyncDocumentRef -> State -> Component State Action
mkApp docRef initialState =
  (component initialState (updateState docRef) viewState)
    { styles = [Sheet styleSheet]
    , subs = [subscribeDocument docRef UpdateDocument]
    , mailbox = \m -> case fromJSON @InMail m of
        Success (CompetenceEditor CE.EditingCanceled) -> Just CancelNewCompetence
        Success (CompetenceEditor (CE.EditingDone c)) -> Just $ AddNewCompetence c
        Error s -> Just $ Log $ ms s
    , initialAction = Initialize
    }

updateState :: SyncDocumentRef -> Action -> Effect State Action
updateState _ (EditField f t) = do
  modify $ \state -> state {editFields = Map.insert f t state.editFields}
  io_ $ focus $ ms $ show f
updateState _ (UpdateDocument (DocumentChange m _)) = do
  io_ $ consoleLog "UpdateDocument"
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
updateState _ NewCompetenceEditor = do
  competenceId <- random'
  modify $ \s ->
    let newCompetence = Competence {id = competenceId, competenceGridId = s.model.competenceGrid.id, description = "", levelDescriptions = M.empty}
     in s & (#newCompetence .~ Just newCompetence)
updateState _ (NewCompetenceEditorMounted cId) =
  io_ $ consoleLog $ "NewCompetenceEditorMounted" <> (ms $ show cId)
updateState _ CancelNewCompetence = do
  io_ $ consoleLog "CancelNewCompetence"
  modify $ (#newCompetence .~ Nothing)
updateState docRef (AddNewCompetence c) = do
  io_ $ consoleLog "AddNewCompetence"
  io_ $ modifySyncDocument docRef (AddCompetence c)
  modify $ (#newCompetence .~ Nothing)
updateState _ (Log s) = io_ $ consoleLog s
