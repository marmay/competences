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
  , Order
  , orderMax
  , User (..)
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeIxs)
import Competences.Query.CompetenceGridGrade qualified as QGridGrade
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.ImportModal qualified as ImportModal
import Competences.Frontend.Component.FramedModal (FramedModalConfig (..), ModalHeight (..), ModalWidth (..), openFramedModal)
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext (..)
  , closeModal
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | Projection type: extracts only the data needed for this component.
-- Grid grades are filtered to only the focused user's grades.
data GridSelectorProjection = GridSelectorProjection
  { allGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , userGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Compute the projection from document and focused user.
-- Filters grid grades to only those for the focused user.
gridSelectorProjection :: Document -> Maybe User -> GridSelectorProjection
gridSelectorProjection doc mUser = GridSelectorProjection
  { allGrids = doc.competenceGrids
  , userGridGrades = case mUser of
      Nothing -> Ix.empty
      Just u -> doc.competenceGridGrades Ix.@= u.id
  , focusedUser = mUser
  }

data Model = Model
  { projection :: !GridSelectorProjection
  , selectedCompetenceGrid :: !(Maybe CompetenceGrid)
  , newCompetenceGrid :: !(Maybe CompetenceGrid)
  , isDropdownOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectCompetenceGrid !CompetenceGrid
  | CreateNewCompetenceGrid
  | ProjectionChanged !(ProjectedChange GridSelectorProjection)
  | ToggleDropdown
  | OpenImportModal
  deriving (Eq, Show)

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

competenceGridSelectorComponent
  :: SyncContext
  -> CompetenceGridSelectorStyle
  -> Maybe (Ix.IxSet CompetenceGridIxs CompetenceGrid -> Maybe CompetenceGrid)
  -> Lens' p (Maybe CompetenceGrid)
  -> M.Component p Model Action
competenceGridSelectorComponent r style initialSelection parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedCompetenceGrid]
    , M.subs = [subscribeWithProjection r gridSelectorProjection ProjectionChanged]
    }
  where
    model = Model
      { projection = GridSelectorProjection Ix.empty Ix.empty Nothing
      , selectedCompetenceGrid = Nothing
      , newCompetenceGrid = Nothing
      , isDropdownOpen = False
      }

    update (SelectCompetenceGrid c) =
      M.modify $ \m -> case Ix.getOne (m.projection.allGrids Ix.@= c.id) of
        Just c' -> m & (#selectedCompetenceGrid ?~ c') & (#newCompetenceGrid .~ Nothing)
        Nothing -> m & (#newCompetenceGrid ?~ c)

    update CreateNewCompetenceGrid = M.withSink $ \s -> do
      competenceGridId <- nextId r
      let competenceGrid = CompetenceGrid competenceGridId orderMax "" ""
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create competenceGrid)
      s ToggleDropdown
      s (SelectCompetenceGrid competenceGrid)

    update (ProjectionChanged change) =
      M.modify $ \m ->
        let m' = updateFromProjection change.projection m
         in case (change.changeInfo, m'.selectedCompetenceGrid, initialSelection) of
              (InitialSnapshot, Nothing, Just f) ->
                m' & #selectedCompetenceGrid .~ f change.projection.allGrids
              _ -> m'

    update ToggleDropdown =
      M.modify $ \m -> m & #isDropdownOpen .~ not m.isDropdownOpen

    update OpenImportModal = do
      M.modify $ #isDropdownOpen .~ False
      let cfg = FramedModalConfig (C.translate' C.LblImportCompetenceGrids) ModalWide ModalFull
      M.io_ $ openFramedModal r.windowManager cfg (ImportModal.competenceGridImportModalComponent r (Just $ closeModal r.windowManager))

    updateFromProjection :: GridSelectorProjection -> Model -> Model
    updateFromProjection proj m =
      let grids = proj.allGrids
          validateCompetenceGrid c = do
            c' <- c
            Ix.getOne $ grids Ix.@= c'.id
          (selected', new') = case (validateCompetenceGrid m.selectedCompetenceGrid, validateCompetenceGrid m.newCompetenceGrid) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { projection = proj
            , selectedCompetenceGrid = selected'
            , newCompetenceGrid = new'
            }

    view (m :: Model) =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ case style of
                CompetenceGridSelectorViewOnlyStyle ->
                  SL.selectorHeader (C.translate' C.LblSelectCompetenceGrids) Nothing
                CompetenceGridSelectorViewAndCreateStyle ->
                  SL.selectorHeaderWithDropdown
                    (C.translate' C.LblSelectCompetenceGrids)
                    m.isDropdownOpen
                    ToggleDropdown
                    [ SL.dropdownItem Icon.IcnAdd (C.translate' C.LblCreate) CreateNewCompetenceGrid
                    , SL.dropdownItem Icon.IcnImport (C.translate' C.LblImportCompetenceGrids) OpenImportModal
                    ]
            , SL.selectorList (map (viewCompetenceGrid m) (Ix.toAscList (Proxy @Order) m.projection.allGrids))
            ]
        ]

    viewCompetenceGrid m c =
      let isSelected = m.selectedCompetenceGrid == Just c || m.newCompetenceGrid == Just c
          label = M.ms $ if c.title == "" then "Ohne Titel" else c.title
          -- Get active grade for this grid and focused user
          -- userGridGrades is already filtered to the focused user
          mGrade = do
            user <- m.projection.focusedUser
            gridGrade <- QGridGrade.activeGridGrade (m.projection.userGridGrades Ix.@= user.id) c.id
            pure gridGrade.grade
          gradeBadge = gradeBadgeView <$> mGrade
       in SL.selectorItemWithBadge isSelected Icon.IcnCompetenceGrid label gradeBadge (SelectCompetenceGrid c)
