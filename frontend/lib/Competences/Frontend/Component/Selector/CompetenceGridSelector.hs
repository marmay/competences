module Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( competenceGridSelectorComponent
  , CompetenceGridSelectorStyle (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , CompetenceGridIxs
  , Document (..)
  , orderMax
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data Model = Model
  { allCompetenceGrids :: Ix.IxSet CompetenceGridIxs CompetenceGrid
  , selectedCompetenceGrid :: !(Maybe CompetenceGrid)
  , newCompetenceGrid :: !(Maybe CompetenceGrid)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectCompetenceGrid !CompetenceGrid
  | CreateNewCompetenceGrid
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

competenceGridSelectorComponent
  :: SyncDocumentRef
  -> CompetenceGridSelectorStyle
  -> Lens' p (Maybe CompetenceGrid)
  -> M.Component p Model Action
competenceGridSelectorComponent r style parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedCompetenceGrid]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing
    update (SelectCompetenceGrid c) =
      M.modify $ \m -> case Ix.getOne (m.allCompetenceGrids Ix.@= c.id) of
        Just c' -> m & (#selectedCompetenceGrid ?~ c') & (#newCompetenceGrid .~ Nothing)
        Nothing -> m & (#newCompetenceGrid ?~ c)
    update CreateNewCompetenceGrid = M.withSink $ \s -> do
      competenceGridId <- nextId r
      let competenceGrid = CompetenceGrid competenceGridId orderMax "" ""
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create competenceGrid)
      s (SelectCompetenceGrid competenceGrid)
    update (UpdateDocument (DocumentChange d _)) = M.modify $ updateModel d

    updateModel :: Document -> Model -> Model
    updateModel d m =
      let allCompetenceGrids = d.competenceGrids
          validateCompetenceGrid c = do
            c' <- c
            Ix.getOne $ allCompetenceGrids Ix.@= c'.id
          (selected', new') = case (validateCompetenceGrid m.selectedCompetenceGrid, validateCompetenceGrid m.newCompetenceGrid) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { allCompetenceGrids = allCompetenceGrids
            , selectedCompetenceGrid = selected'
            , newCompetenceGrid = new'
            }

    view (m :: Model) =
      V.viewFlow
        (V.vFlow & (#gap .~ V.SmallSpace))
        ( [ V.title_ (C.translate' C.LblSelectCompetenceGrids)
          , viewCompetenceGrids m
          ]
            <> [ Button.buttonPrimary (C.translate' C.LblAddCompetenceGrid)
                   & Button.withIcon IcnAdd
                   & Button.withClick CreateNewCompetenceGrid
                   & Button.renderButton
               | style == CompetenceGridSelectorViewAndCreateStyle
               ]
        )
    viewCompetenceGrids (m :: Model) =
      V.viewFlow
        (V.vFlow & (#extraAttrs .~ [T.tailwind [T.Shrink, T.OverflowYAuto]]))
        (map viewCompetenceGrid (Ix.toList m.allCompetenceGrids))
      where
        viewCompetenceGrid c =
          M.a_
            [M.onClickWithOptions M.stopPropagation (SelectCompetenceGrid c)]
            [ V.viewFlow
                (V.vFlow & #expandOrthogonal .~ V.Expand V.Start)
                [ M.a_
                    [M.onClickWithOptions M.stopPropagation (SelectCompetenceGrid c)]
                    [V.text_ (M.ms $ if c.title == "" then "Ohne Titel" else c.title)]
                ]
            ]
