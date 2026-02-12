module Competences.Frontend.Component.Selector.MultiSelectResourceSelector
  ( multiSelectResourceSelectorComponent
  )
where

import Competences.Document (Document (..), Resource (..))
import Competences.Query.Resource qualified as QResource
import Competences.Document.Resource (ResourceId, ResourceIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , mkSelectorBinding
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
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

    update (SetOpen open) =
      M.modify $ #isOpen .~ open

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
        , -- Display selected resources as tags
          if null m.selectedResults
            then M.text ""
            else
              Layout.viewFlow
                Layout.hFlow{Layout.gap = Layout.SmallSpace, Layout.extraAttrs = [class_ "flex-wrap"]}
                [ viewResourceTag res
                | resId <- m.selectedResults
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

    viewResourceTag :: Resource -> M.View Model Action
    viewResourceTag res =
      Badge.interactive
        Badge.Secondary
        (Just (Icon.IcnCancel, ToggleResource res.id))
        (Badge.badgeIconText Icon.IcnResources (M.ms $ unResourceIdentifier res.identifier))
