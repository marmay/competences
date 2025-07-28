module Competences.Frontend.Grid.App (mkApp, grid, runApp) where

import Competences.Command (Command (..))
import Competences.Frontend.Common.Random (random')
import Competences.Frontend.Common.Style (styleSheet)
import Competences.Frontend.Document
  ( DocumentRef
  , ModelChange (..)
  , modifyDocumentModel
  , subscribeDocument
  )
import Competences.Frontend.Grid.Action (Action (..))
import Competences.Frontend.Grid.State (NewCompetenceData (..), State (..), emptyNewCompetenceData)
import Competences.Frontend.Grid.View (viewState)
import Competences.Model (Model (..), fieldATraversal)
import Competences.Model.Competence (Competence (..), CompetenceId, Level (..))
import Competences.Model.CompetenceGrid (CompetenceGrid (..), CompetenceGridId)
import Competences.Model.User (User (..))
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM)
import Miso
  ( CSS (..)
  , Component (..)
  , Effect
  , component
  , consoleLog
  , focus
  , get
  , io_
  , modify
  , startComponent
  )
import Miso.String (MisoString, fromMisoString, ms)
import Optics.Core ((%~), (&), (.~), (%), (%?))
import Optics.Core qualified as O

runApp :: Component State Action -> JSM ()
runApp = startComponent

grid :: DocumentRef -> State -> Component State Action
grid = mkApp

mkApp :: DocumentRef -> State -> Component State Action
mkApp docRef initialState =
  (component initialState (updateState docRef) viewState)
    { styles = [Sheet styleSheet]
    , subs = [subscribeDocument docRef UpdateModel]
    }

updateState :: DocumentRef -> Action -> Effect State Action
updateState _ (EditField f t) = do
  modify $ \state -> state {editFields = Map.insert f t state.editFields}
  io_ $ focus $ ms $ show f
updateState _ (UpdateModel (ModelChange m _)) = do
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
updateState docRef (IssueCommand cmd) = do
  io_ $ modifyDocumentModel docRef cmd
updateState _ NewCompetence = modify $ (#newCompetenceData .~ Just emptyNewCompetenceData)
updateState _ CancelNewCompetence = modify $ (#newCompetenceData .~ Nothing)
updateState docRef AddNewCompetence = do
  competenceId <- random'
  s <- get
  case s.newCompetenceData of
    Just n ->
      io_ $
        modifyDocumentModel docRef (AddCompetence $ makeCompetence competenceId s.model.competenceGrid.id n)
    Nothing -> pure ()
  modify (#newCompetenceData .~ Nothing)
updateState _ (SetNewCompetenceDescription d) = modify (#newCompetenceData %? #description .~ d)

makeCompetence :: CompetenceId -> CompetenceGridId -> NewCompetenceData -> Competence
makeCompetence competenceId competenceGridId n = do
  Competence
    { id = competenceId
    , competenceGridId = competenceGridId
    , description = fromMisoString n.description
    , levelDescriptions =
        M.fromList $
          catMaybes $
            [ (BasicLevel,) <$> toMaybe (n.basicLevelDescription)
            , (IntermediateLevel,) <$> toMaybe (n.intermediateLevelDescription)
            , (AdvancedLevel,) <$> toMaybe (n.advancedLevelDescription)
            ]
    }
  where
    toMaybe :: MisoString -> Maybe Text
    toMaybe "" = Nothing
    toMaybe s = Just $ fromMisoString s
