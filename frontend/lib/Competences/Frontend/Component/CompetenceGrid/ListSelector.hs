-- | Competence-grid list selector — config builder for
-- 'listSelectorComponent'. Projection carries the focused user's
-- grid grades so the per-row grade badge can be derived in
-- 'renderItem'. Style flag toggles the teacher-only create
-- dropdown.
module Competences.Frontend.Component.CompetenceGrid.ListSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridListSelectorComponent
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
import Competences.Document.Id (Id)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.List
  ( Action (..)
  , CreateAction (..)
  , ItemRenderer (..)
  , ListSelectorConfig (..)
  , Model
  , listSelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.GradeBadge (gradeBadgeView)
import Competences.Frontend.Fragment.SelectorFilter (noopFilter)
import Competences.Frontend.Page qualified as Page
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument, nextId)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Query.CompetenceGridGrade qualified as QGridGrade
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens')

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

type Selected = CompetenceGrid

-- | Projection: grids plus the focused user's grades for badging.
data Projection = Projection
  { allGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , userGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  }
  deriving (Eq, Generic, Show)

emptyProjection :: Projection
emptyProjection = Projection Ix.empty Ix.empty

projectGrids :: Document -> Maybe User -> Projection
projectGrids doc mUser =
  Projection
    { allGrids = doc.competenceGrids
    , userGridGrades = case mUser of
        Nothing -> Ix.empty
        Just u -> doc.competenceGridGrades Ix.@= u.id
    }

competenceGridListSelectorComponent
  :: SyncContext
  -> CompetenceGridSelectorStyle
  -> Maybe (Ix.IxSet CompetenceGridIxs CompetenceGrid -> Maybe CompetenceGrid)
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection ()) (Action Selected Projection ())
competenceGridListSelectorComponent r style initialPickFn parentLens =
  listSelectorComponent r (config style initialPickFn parentLens)

config
  :: CompetenceGridSelectorStyle
  -> Maybe (Ix.IxSet CompetenceGridIxs CompetenceGrid -> Maybe CompetenceGrid)
  -> Lens' p (Maybe Selected)
  -> ListSelectorConfig p Selected Projection CompetenceGridIxs (Id CompetenceGrid) () ()
config style initialPickFn parentLens =
  ListSelectorConfig
    { title = C.translate' C.LblSelectCompetenceGrids
    , project = projectGrids
    , emptyProjection = emptyProjection
    , entitiesOf = (.allGrids)
    , itemsInOrder = Ix.toAscList (Proxy @Order)
    , idOf = (.id)
    , itemView = ItemRenderer renderItem
    , createActions = case style of
        CompetenceGridSelectorViewOnlyStyle -> []
        CompetenceGridSelectorViewAndCreateStyle ->
          [ CreateAction
              { icon = Icon.IcnAdd
              , label = C.translate' C.LblCreate
              , run = \r -> do
                  gid <- nextId r
                  let g = CompetenceGrid gid orderMax "" ""
                  modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create g)
                  pure (Just g)
              }
          ]
    , uriBinding =
        Just $ pageBinding (Page.CompetenceGrid . Just) $ \case
          Page.CompetenceGrid (Just gid) -> Just gid
          _ -> Nothing
    , initialPick = initialPickFn
    , filter = noopFilter
    , parentLens = parentLens
    }

renderItem
  :: Selected
  -> Projection
  -> Bool
  -> M.View m (Action Selected Projection ())
renderItem g proj isSel =
  let label = M.ms (if g.title == "" then "Ohne Titel" else g.title)
      mGrade = do
        gridGrade <- QGridGrade.activeGridGrade proj.userGridGrades g.id
        pure gridGrade.grade
      gradeBadge = gradeBadgeView <$> mGrade
   in SL.selectorItemWithBadge isSel Icon.IcnCompetenceGrid label gradeBadge (Pick g)
