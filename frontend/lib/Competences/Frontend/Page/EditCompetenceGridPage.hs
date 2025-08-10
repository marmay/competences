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
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), (?~))
import System.Random (StdGen)

editCompetenceGridPage
  :: SyncDocumentRef -> StdGen -> User -> C.TranslationData -> M.Component p Model Action
editCompetenceGridPage r g u td =
  (M.component model (update r) view)
    { M.subs =
        map (C.liftSub CompetenceGridEditorAction) (CGE.subscriptions r)
          <> [subscribeDocument r UpdateDocument]
    , M.events = M.defaultEvents <> M.keyboardEvents
    }
  where
    model =
      Model
        { competenceGridEditor = CGE.mkModel u
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

view :: forall m. Model -> M.View m Action
view m =
  V.vBox_
    V.NoExpand
    (V.Expand V.Start)
    V.LargeGap
    [ CompetenceGridEditorAction <$> CGE.view m.competenceGridEditor
    , V.hBox_
        (V.Expand V.End)
        V.NoExpand
        V.NoGap
        [ V.iconLabelButton
            [M.onClick SpawnNewCompetenceEditor]
            V.RegularButton
            V.IcnAdd
            (C.translate' C.LblAddNewCompetence)
        ]
    , NewCompetenceEditorAction <$> V.maybeModalHost m.newCompetenceEditor CE.view
    ]

update :: SyncDocumentRef -> Action -> M.Effect p Model Action
update r (CompetenceGridEditorAction a) = C.liftEffect #competenceGridEditor CompetenceGridEditorAction (CGE.update r a)
update r (NewCompetenceEditorAction e@CE.CompleteEditing) = do
  C.liftEffect' #newCompetenceEditor NewCompetenceEditorAction (CE.update r e)
  M.modify $ #newCompetenceEditor .~ Nothing
update r (NewCompetenceEditorAction e@CE.CancelEditing) = do
  C.liftEffect' #newCompetenceEditor NewCompetenceEditorAction (CE.update r e)
  M.modify $ #newCompetenceEditor .~ Nothing
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
     in m & (#newCompetenceEditor ?~ CE.Model newCompetence)
update _ (UpdateDocument (DocumentChange d _)) = M.modify $ #document .~ d
