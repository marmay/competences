-- | Competence-grid list selector — config wrapper around
-- 'listSelectorComponent'.
--
-- Rich projection: alongside the grids themselves, it carries the
-- focused user's grid grades so the per-row grade badge can be
-- derived without re-walking the document. The badge is rendered by
-- the 'ItemRenderer' (which receives the projection in addition to
-- the grid).
--
-- Style flag controls whether the create dropdown is shown
-- (view-only vs. teacher's view-and-create). The "import grids"
-- dropdown entry is intentionally absent; see docs/TODO.md
-- immediate follow-ups for the universal-import plan.
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
import Competences.Frontend.Fragment.GradeBadge (gradeBadgeView)
import Competences.Frontend.Fragment.SelectorFilter (FilterFragment (..))
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
    , lookupBy = \xs gid -> Ix.getOne (xs Ix.@= gid)
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
    , uriBinding = Nothing
    , initialPick = initialPickFn
    , filter = noopFilter
    , parentLens = parentLens
    }

-- The original selector has no search field; replicate that with a
-- pass-through filter rather than imposing search on every selector.
noopFilter :: FilterFragment Projection () () Selected
noopFilter =
  FilterFragment
    { initialState = ()
    , update = \_act s -> s
    , view = \_st _proj -> M.text ""
    , apply = \_st _proj items -> items
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
