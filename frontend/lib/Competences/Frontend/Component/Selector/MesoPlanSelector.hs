module Competences.Frontend.Component.Selector.MesoPlanSelector
  ( mesoPlanSelectorComponent
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), MesoPlan (..), MesoPlanIxs)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.DateDisplay qualified as DateDisplay
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SelectorList
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | Projection from document - all meso plans sorted by title
data SelectorProjection = SelectorProjection
  { mesoPlans :: !(Ix.IxSet MesoPlanIxs MesoPlan)
  }
  deriving (Eq, Generic, Show)

emptyProjection :: SelectorProjection
emptyProjection = SelectorProjection Ix.empty

-- | Projection function - gets all meso plans
selectorProjection :: Document -> Maybe user -> SelectorProjection
selectorProjection doc _ =
  SelectorProjection
    { mesoPlans = doc.mesoPlans
    }

data Model = Model
  { projection :: !SelectorProjection
  , selectedPlan :: !(Maybe MesoPlan) -- bound to parent
  , searchQuery :: !Text
  , isDropdownOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectPlan !MesoPlan
  | CreateNewPlan
  | SetSearchQuery !Text
  | ProjectionChanged !(ProjectedChange SelectorProjection)
  | ToggleDropdown
  deriving (Eq, Show)

mesoPlanSelectorComponent
  :: SyncContext -> Lens' p (Maybe MesoPlan) -> M.Component p Model Action
mesoPlanSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedPlan]
    , M.subs = [subscribeWithProjection r selectorProjection ProjectionChanged]
    }
  where
    model =
      Model
        { projection = emptyProjection
        , selectedPlan = Nothing
        , searchQuery = ""
        , isDropdownOpen = False
        }

    update (SelectPlan p) =
      M.modify $ \m -> case Ix.getOne (m.projection.mesoPlans Ix.@= p.id) of
        Just p' -> m & (#selectedPlan ?~ p')
        Nothing -> m & (#selectedPlan ?~ p) -- newly created, not yet in projection

    update CreateNewPlan = M.withSink $ \s -> do
      planId <- nextId r
      let newPlan =
            MesoPlan
              { id = planId
              , title = ""
              , dateFrom = Nothing
              , dateTo = Nothing
              }
      modifySyncDocument r $ MesoPlans (OnMesoPlans (CreateAndLock newPlan))
      s (SelectPlan newPlan)
      s ToggleDropdown

    update (SetSearchQuery q) = M.modify $ \m -> m & #searchQuery .~ q

    update (ProjectionChanged change) =
      M.modify $ \m ->
        let proj' = change.projection
            -- Update selected plan from new projection
            selectedPlan' = m.selectedPlan >>= \sel ->
              Ix.getOne (proj'.mesoPlans Ix.@= sel.id)
         in m
              & (#projection .~ proj')
              & (#selectedPlan .~ selectedPlan')

    update ToggleDropdown = M.modify $ \m -> m & #isDropdownOpen .~ not m.isDropdownOpen

    view' m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SelectorList.selectorHeaderWithDropdown
            (C.translate' C.LblMesoPlans)
            m.isDropdownOpen
            ToggleDropdown
            [ SelectorList.dropdownItem Icon.IcnAdd (C.translate' C.LblCreate) CreateNewPlan
            ]
        , SelectorList.selectorSearchField
            (ms m.searchQuery)
            (C.translate' C.LblFilterMesoPlans)
            (SetSearchQuery . M.fromMisoString)
        , SelectorList.selectorList (map (viewPlan m) (filteredPlans m))
        ]

    filteredPlans m =
      let proj = m.projection
          query = T.toLower m.searchQuery
          sorted = sortOn (.title) $ Ix.toList proj.mesoPlans
       in if T.null query
            then sorted
            else filter (\p -> query `T.isInfixOf` T.toLower p.title) sorted

    viewPlan m p =
      let isSelected = m.selectedPlan == Just p
       in SelectorList.selectorItemMultiLine
            isSelected
            [ -- Line 1: Icon + Title
              MH.div_
                [class_ "flex items-center gap-2"]
                [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] Icon.IcnMesoPlan
                , MH.span_
                    [class_ "text-sm truncate font-medium"]
                    [M.text $ ms $ if T.null p.title then "(Untitled)" else p.title]
                ]
            , -- Line 2: Date range
              MH.div_
                [class_ "flex items-center gap-2 text-xs text-muted-foreground"]
                [M.text $ DateDisplay.formatDateRange p.dateFrom p.dateTo]
            ]
            (SelectPlan p)
