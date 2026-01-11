module Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( competenceGridSelectorComponent
  , CompetenceGridSelectorStyle (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceGridIxs
  , Document (..)
  , Order
  , orderMax
  , User (..)
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeIxs)
import Competences.Document.User (UserId)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Time (Day)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import GHC.Generics (Generic)
import Miso qualified as M
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
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectCompetenceGrid !CompetenceGrid
  | CreateNewCompetenceGrid
  | ProjectionChanged !(ProjectedChange GridSelectorProjection)
  deriving (Eq, Show)

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

competenceGridSelectorComponent
  :: SyncContext
  -> CompetenceGridSelectorStyle
  -> Lens' p (Maybe CompetenceGrid)
  -> M.Component p Model Action
competenceGridSelectorComponent r style parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedCompetenceGrid]
    , M.subs = [subscribeWithProjection r gridSelectorProjection ProjectionChanged]
    }
  where
    model = Model (GridSelectorProjection Ix.empty Ix.empty Nothing) Nothing Nothing

    update (SelectCompetenceGrid c) =
      M.modify $ \m -> case Ix.getOne (m.projection.allGrids Ix.@= c.id) of
        Just c' -> m & (#selectedCompetenceGrid ?~ c') & (#newCompetenceGrid .~ Nothing)
        Nothing -> m & (#newCompetenceGrid ?~ c)

    update CreateNewCompetenceGrid = M.withSink $ \s -> do
      competenceGridId <- nextId r
      let competenceGrid = CompetenceGrid competenceGridId orderMax "" ""
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create competenceGrid)
      s (SelectCompetenceGrid competenceGrid)

    update (ProjectionChanged change) = M.modify $ updateFromProjection change.projection

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
        , SL.selectorList (map (viewCompetenceGrid m) (Ix.toAscList (Proxy @Order) m.projection.allGrids))
        ]

    viewCompetenceGrid m c =
      let isSelected = m.selectedCompetenceGrid == Just c || m.newCompetenceGrid == Just c
          label = M.ms $ if c.title == "" then "Ohne Titel" else c.title
          -- Get active grade for this grid and focused user
          -- userGridGrades is already filtered to the focused user
          mGrade = do
            user <- m.projection.focusedUser
            gridGrade <- getActiveGridGrade' m.projection.userGridGrades user.id c.id
            pure gridGrade.grade
          gradeBadge = gradeBadgeView <$> mGrade
       in SL.selectorItemWithBadge isSelected IcnCompetenceGrid label gradeBadge (SelectCompetenceGrid c)

-- | Get the most recent (active) grid grade for a user and competence grid.
-- Uses IxSet indexing for efficient lookup.
getActiveGridGrade'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> UserId
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
getActiveGridGrade' grades userId gridId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $
    grades Ix.@= userId Ix.@= gridId
