module Competences.Frontend.Page.EditCompetenceGridPage
  ( editCompetenceGridPage
  )
where

import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , User
  , emptyDocument
  , orderMax
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceEditor qualified as CE
import Competences.Frontend.Component.CompetenceGridEditor qualified as CGE
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))
import System.Random (StdGen)

editCompetenceGridPage
  :: SyncDocumentRef -> StdGen -> User -> C.TranslationData -> M.Component Model Action
editCompetenceGridPage r g u td =
  (M.component model (update r) view)
    { M.subs =
        map (C.liftSub CompetenceGridEditorAction) (CGE.subscriptions r)
          <> [subscribeDocument r UpdateDocument]
    , M.events = M.defaultEvents <> M.dragEvents
    }
  where
    model =
      Model
        { competenceGridEditor = CGE.mkModel u td
        , newCompetenceEditor = Nothing
        , document = emptyDocument
        , translationData = td
        , random = g
        }

data Model = Model
  { competenceGridEditor :: !CGE.Model
  , newCompetenceEditor :: !(Maybe CE.Model)
  , document :: !Document
  , translationData :: !C.TranslationData
  , random :: !StdGen
  }
  deriving (Eq, Generic, Show)

data Action
  = CompetenceGridEditorAction !CGE.Action
  | NewCompetenceEditorAction !CE.Action
  | SpawnNewCompetenceEditor
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

view :: Model -> M.View Action
view m =
  M.div_
    []
    [ CompetenceGridEditorAction <$> CGE.view m.competenceGridEditor
    , C.iconLabelButton
        [M.onClick SpawnNewCompetenceEditor]
        C.IcnAdd
        (C.translate m C.LblAddNewCompetence)
    , NewCompetenceEditorAction <$> C.maybeModal m.newCompetenceEditor CE.view
    ]

update :: SyncDocumentRef -> Action -> M.Effect Model Action
update r (CompetenceGridEditorAction a) = C.liftEffect #competenceGridEditor CompetenceGridEditorAction (CGE.update r a)
update r (NewCompetenceEditorAction e@CE.CompleteEditing) = do
  C.liftEffect' #newCompetenceEditor NewCompetenceEditorAction (CE.update r e)
  M.modify $ #newCompetenceEditor.~ Nothing
update r (NewCompetenceEditorAction e@CE.CancelEditing) = do
  C.liftEffect' #newCompetenceEditor NewCompetenceEditorAction (CE.update r e)
  M.modify $ #newCompetenceEditor.~ Nothing
update r (NewCompetenceEditorAction a) = C.liftEffect' #newCompetenceEditor NewCompetenceEditorAction (CE.update r a)
update _ SpawnNewCompetenceEditor = do
  competenceId <- C.random'
  M.modify $ \m ->
    let newCompetence =
          Competence
            { id = competenceId
            , order = orderMax
            , competenceGridId = m.document.competenceGrid.id
            , description = ""
            , levelDescriptions = Map.empty
            }
     in m & (#newCompetenceEditor .~ Just (CE.Model newCompetence m.translationData))
update _ (UpdateDocument (DocumentChange d _)) = M.modify $ #document .~ d
