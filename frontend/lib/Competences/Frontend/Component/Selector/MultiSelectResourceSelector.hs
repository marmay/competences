module Competences.Frontend.Component.Selector.MultiSelectResourceSelector
  ( multiSelectResourceSelectorComponent
  , multiSelectResourceViewerComponent
  )
where

import Competences.Document (Document (..), Resource (..))
import Competences.Document.Resource (ResourceId, ResourceIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.ListReorder
  ( ListReorderAction (..)
  , ListReorderButtons (..)
  , ListReorderState (..)
  , initialListReorderState
  , listReorderButtons
  , moveElement
  )
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , mkSelectorBinding
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.Resource qualified as QResource
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

-- | Projection from document - all resources
data SelectorProjection = SelectorProjection
  { allResources :: ![Resource]
  }
  deriving (Eq, Generic, Show)

-- | Projection function - gets all resources
selectorProjection :: Document -> Maybe user -> SelectorProjection
selectorProjection doc _ =
  SelectorProjection
    { allResources = QResource.allResources doc
    }

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { projection :: !SelectorProjection
  , selectedResults :: ![ResourceId]
  , searchQuery :: !Text
  , isOpen :: !Bool
  , reorderState :: !ListReorderState
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = ProjectionChanged !(ProjectedChange SelectorProjection)
  | SetSearchQuery !Text
  | ToggleResource !ResourceId
  | SetOpen !Bool
  | ResourceReorder !ListReorderAction
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Multi-select resource selector component
-- Binds selected resource IDs to parent model via lens
multiSelectResourceSelectorComponent
  :: SyncContext
  -> [ResourceId] -- ^ Initial selection
  -> SelectorTransformedLens p [] ResourceId f' a'
  -> M.Component p Model Action
multiSelectResourceSelectorComponent r initResults lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedResults]
    , M.subs = [subscribeWithProjection r selectorProjection ProjectionChanged]
    }
  where
    model =
      Model
        { projection = SelectorProjection []
        , selectedResults = initResults
        , searchQuery = ""
        , isOpen = False
        , reorderState = initialListReorderState
        }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        m & #projection .~ change.projection

    update (SetSearchQuery q) =
      M.modify $ #searchQuery .~ q

    update (ToggleResource resId) =
      M.modify $ \m ->
        let current = m.selectedResults
            new =
              if resId `elem` current
                then filter (/= resId) current
                else current <> [resId]
         in m & #selectedResults .~ new
              & #reorderState .~ initialListReorderState

    update (SetOpen open) =
      M.modify $ #isOpen .~ open

    update (ResourceReorder (StartListReorder idx)) =
      M.modify $ \m -> m & #reorderState .~ ListReorderState (Just idx)

    update (ResourceReorder CancelListReorder) =
      M.modify $ \m -> m & #reorderState .~ initialListReorderState

    update (ResourceReorder (ListReorderTo src tgt)) =
      M.modify $ \m ->
        m & #selectedResults .~ moveElement src tgt m.selectedResults
          & #reorderState .~ initialListReorderState

    view m =
      MH.div_
        [class_ "space-y-2"]
        [ -- Multi-select combobox
          let filteredResources = filterResources m.searchQuery m.projection.allResources
              options =
                [ Combobox.ComboboxOption res.id (unResourceIdentifier res.identifier)
                | res <- filteredResources
                ]
              selectedSet = Set.fromList m.selectedResults
           in Combobox.multiSelectCombobox SetSearchQuery ToggleResource SetOpen
                & Combobox.withPlaceholder (M.fromMisoString $ C.translate' C.LblSelectResources)
                & Combobox.withOptions options
                & Combobox.withSelected selectedSet
                & Combobox.withSearchQuery m.searchQuery
                & Combobox.withIsOpen m.isOpen
                & Combobox.renderCombobox
        , -- Display selected resources as vertical list with reorder/remove buttons
          if null m.selectedResults
            then M.text ""
            else
              Layout.vFlow
                Layout.gapT
                [ viewResourceItem m idx res
                | (idx, resId) <- zip [0 ..] m.selectedResults
                , Just res <- [lookupResource resId m.projection.allResources]
                ]
        ]

    filterResources query resources =
      let q = T.toLower query
       in if T.null q
            then resources
            else filter (\res -> q `T.isInfixOf` T.toLower (unResourceIdentifier res.identifier)) resources

    unResourceIdentifier (ResourceIdentifier t) = t

    lookupResource resId resources =
      case filter (\res -> res.id == resId) resources of
        (res : _) -> Just res
        [] -> Nothing

    viewResourceItem :: Model -> Int -> Resource -> M.View Model Action
    viewResourceItem m idx res =
      MH.div_
        [class_ "flex items-center gap-2 px-2 py-1 rounded border border-border bg-background"]
        [ Icon.iconS Icon.Small Icon.IcnResources
        , MH.span_ [class_ "flex-1 text-sm truncate"] [M.text $ M.ms $ unResourceIdentifier res.identifier]
        , reorderButtons m.reorderState idx res.id
        ]

    reorderButtons :: ListReorderState -> Int -> ResourceId -> M.View Model Action
    reorderButtons st idx resId =
      MH.div_
        [class_ "flex items-center gap-0.5"]
        (case listReorderButtons st idx of
          ShowReorderStart ->
            [ Button.ghostSm (Button.button Icon.IcnReorder (ResourceReorder (StartListReorder idx)))
            , Button.ghostSm (Button.button Icon.IcnCancel (ToggleResource resId))
            ]
          ShowReorderCancel ->
            [ Button.ghostSm (Button.button Icon.IcnCancel (ResourceReorder CancelListReorder))
            ]
          ShowReorderTargets fromIdx thisIdx ->
            [ Button.ghostSm (Button.button Icon.IcnArrowUp (ResourceReorder (ListReorderTo fromIdx thisIdx)))
            , Button.ghostSm (Button.button Icon.IcnArrowDown (ResourceReorder (ListReorderTo fromIdx (thisIdx + 1))))
            ])

-- ============================================================================
-- Viewer Component (read-only)
-- ============================================================================

-- | Read-only viewer for selected resources.
-- Shows a simple list of resource names without combobox or action buttons.
data ViewerModel = ViewerModel
  { projection :: !SelectorProjection
  , selectedResults :: ![ResourceId]
  }
  deriving (Eq, Generic, Show)

newtype ViewerAction = ViewerProjectionChanged (ProjectedChange SelectorProjection)
  deriving (Eq, Show)

multiSelectResourceViewerComponent
  :: SyncContext
  -> [ResourceId]
  -> SelectorTransformedLens p [] ResourceId f' a'
  -> M.Component p ViewerModel ViewerAction
multiSelectResourceViewerComponent r initResults lensBinding =
  (M.component viewerModel viewerUpdate viewerView)
    { M.bindings = [mkSelectorBinding lensBinding #selectedResults]
    , M.subs = [subscribeWithProjection r selectorProjection ViewerProjectionChanged]
    }
  where
    viewerModel =
      ViewerModel
        { projection = SelectorProjection []
        , selectedResults = initResults
        }

    viewerUpdate (ViewerProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    viewerView m =
      if null m.selectedResults
        then M.text ""
        else
          Layout.vFlow
            Layout.gapT
            [ MH.div_
                [class_ "flex items-center gap-2 px-2 py-1"]
                [ Icon.iconS Icon.Small Icon.IcnResources
                , MH.span_ [class_ "text-sm"] [M.text $ M.ms $ unResourceIdentifier res.identifier]
                ]
            | resId <- m.selectedResults
            , Just res <- [lookupResource resId m.projection.allResources]
            ]

    unResourceIdentifier (ResourceIdentifier t) = t

    lookupResource resId resources =
      case filter (\res -> res.id == resId) resources of
        (res : _) -> Just res
        [] -> Nothing
