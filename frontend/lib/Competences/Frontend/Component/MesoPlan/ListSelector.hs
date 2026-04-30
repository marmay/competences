-- | Meso-plan list selector — a thin config wrapper around
-- 'listSelectorComponent'.
--
-- Meso plans have no draft variant and no URL binding (the meso-plan
-- page isn't a route by itself; it's part of the Planning page).
-- One create button; sorted by '(isNothing dateFrom, dateFrom,
-- title)' so undated plans sink to the bottom.
module Competences.Frontend.Component.MesoPlan.ListSelector
  ( mesoPlanListSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), MesoPlansCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), MesoPlan (..), MesoPlanIxs)
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
import Competences.Frontend.Fragment.SelectorFilter (searchOnlyFilter)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument, nextId)
import Competences.Frontend.View.DateDisplay qualified as DateDisplay
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Competences.Document.Id (Id)
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core (Lens')

type Selected = MesoPlan
type Projection = Ix.IxSet MesoPlanIxs MesoPlan

mesoPlanListSelectorComponent
  :: SyncContext
  -> Maybe (Projection -> Maybe Selected)
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection Text) (Action Selected Projection Text)
mesoPlanListSelectorComponent r initialPickFn parentLens =
  listSelectorComponent r (config initialPickFn parentLens)

config
  :: Maybe (Projection -> Maybe Selected)
  -> Lens' p (Maybe Selected)
  -> ListSelectorConfig p Selected Projection MesoPlanIxs (Id MesoPlan) Text Text
config initialPickFn parentLens =
  ListSelectorConfig
    { title = C.translate' C.LblMesoPlans
    , project = \doc _user -> doc.mesoPlans
    , emptyProjection = Ix.empty
    , entitiesOf = id
    , itemsInOrder = sortOn (\p -> (isNothing p.dateFrom, p.dateFrom, p.title)) . Ix.toList
    , idOf = (.id)
    , itemView = ItemRenderer renderItem
    , createActions =
        [ CreateAction
            { icon = Icon.IcnAdd
            , label = C.translate' C.LblCreate
            , run = \r -> do
                planId <- nextId r
                let p =
                      MesoPlan
                        { id = planId
                        , title = ""
                        , dateFrom = Nothing
                        , dateTo = Nothing
                        }
                modifySyncDocument r $ MesoPlans (OnMesoPlans (CreateAndLock p))
                pure (Just p)
            }
        ]
    , uriBinding =
        Just $ pageBinding Planning $ \case
          Planning mPid -> Just mPid
          _ -> Nothing
    , initialPick = initialPickFn
    , filter = searchOnlyFilter (C.translate' C.LblFilterMesoPlans) (.title)
    , parentLens = parentLens
    }

renderItem
  :: Selected
  -> Projection
  -> Bool
  -> M.View m (Action Selected Projection Text)
renderItem p _proj isSel =
  SL.selectorItemMultiLine
    isSel
    [ MH.div_
        [class_ "flex items-center gap-2"]
        [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] Icon.IcnMesoPlan
        , MH.span_
            [class_ "text-sm truncate font-medium"]
            [M.text $ ms $ if T.null p.title then "(Untitled)" else p.title]
        ]
    , MH.div_
        [class_ "flex items-center gap-2 text-xs text-muted-foreground"]
        [M.text $ DateDisplay.formatDateRange p.dateFrom p.dateTo]
    ]
    (Pick p)
