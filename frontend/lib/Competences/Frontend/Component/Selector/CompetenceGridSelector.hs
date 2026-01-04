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
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import GHC.Generics (Generic)
import Miso qualified as M
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
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader
            (C.translate' C.LblSelectCompetenceGrids)
            ( if style == CompetenceGridSelectorViewAndCreateStyle
                then Just CreateNewCompetenceGrid
                else Nothing
            )
        , SL.selectorList (map (viewCompetenceGrid m) (Ix.toList m.allCompetenceGrids))
        ]

    viewCompetenceGrid m c =
      let isSelected = m.selectedCompetenceGrid == Just c || m.newCompetenceGrid == Just c
          label = M.ms $ if c.title == "" then "Ohne Titel" else c.title
       in SL.selectorItem isSelected IcnCompetenceGrid label (SelectCompetenceGrid c)
