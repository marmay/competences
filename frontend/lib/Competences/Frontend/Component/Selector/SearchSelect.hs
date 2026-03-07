module Competences.Frontend.Component.Selector.SearchSelect
  ( -- * Config
    SearchSelectConfig (..)

    -- * Component
  , searchSelectComponent

    -- * Types (for parent modules)
  , Model
  , Action
  )
where

import Competences.Document (Document (..))
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
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.TagInput (TagInputConfig (..), tagInput)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Search (Query, matchItem, parseQuery)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (fromMisoString, ms)
import Optics.Core qualified as O
import Optics.Core ((.~))

-- ============================================================================
-- Config
-- ============================================================================

-- | Configuration for a generic search-select component.
data SearchSelectConfig a id = SearchSelectConfig
  { projectItems :: !(Document -> [a])
  -- ^ Extract all selectable items from the document
  , itemId :: !(a -> id)
  -- ^ Stable key for tracking selections across document updates
  , itemLabel :: !(a -> Text)
  -- ^ Primary identifier text (for display in dropdown AND text search)
  , itemMetadata :: !(a -> [Text])
  -- ^ Metadata values for @ filters (entity-specific, defined per use-case)
  , viewTag :: !(a -> (Icon.Icon, M.MisoString))
  -- ^ Icon + label for the inline badge. Component handles x-button and remove action.
  , placeholder :: !Text
  }
  deriving (Generic)

-- ============================================================================
-- Projection
-- ============================================================================

-- | Items projected from the document.
newtype Projection a = Projection
  { allItems :: [a]
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Model
-- ============================================================================

data Model a id = Model
  { allItems :: ![a]
  , selectedIds :: ![id]
  , searchQuery :: !Text
  , highlightIdx :: !(Maybe Int)
  , hasFocus :: !Bool
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action a id
  = ProjectionChanged !(ProjectedChange (Projection a))
  | SetQuery !Text
  | AddAll
  | AddHighlighted
  | MoveHighlight !Int
  | RemoveItem !id
  | RemoveLast
  | ToggleItem !id
  | SetFocus !Bool
  | NoOp
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | A generic search-select component with tag input and query language.
--
-- The binding exposes @[a]@ (full selected entities in selection order) to the
-- parent via 'SelectorTransformedLens'. The parent can transform further
-- (e.g., @(.id)@ to get @[SomeId]@).
searchSelectComponent
  :: forall a id p f' a'
   . (Eq a, Eq id, Ord id, Show a, Show id)
  => SyncContext
  -> SearchSelectConfig a id
  -> [id]
  -> SelectorTransformedLens p [] a f' a'
  -> M.Component p (Model a id) (Action a id)
searchSelectComponent r cfg initIds lensBinding =
  (M.component model update (view cfg))
    { M.bindings = [mkSelectorBinding lensBinding selectedEntitiesLens]
    , M.subs = [subscribeWithProjection r (projection cfg) ProjectionChanged]
    }
  where
    model =
      Model
        { allItems = []
        , selectedIds = initIds
        , searchQuery = ""
        , highlightIdx = Nothing
        , hasFocus = False
        }

    -- | Virtual lens: reads [a] (resolved from selectedIds), writes back [id].
    selectedEntitiesLens :: O.Lens' (Model a id) [a]
    selectedEntitiesLens = O.lens getter setter
      where
        getter m =
          concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
        setter m newSelected =
          m {selectedIds = map cfg.itemId newSelected}

    projection :: SearchSelectConfig a id -> Document -> Maybe user -> Projection a
    projection c doc _ = Projection {allItems = c.projectItems doc}

    update (ProjectionChanged change) =
      M.modify $ #allItems .~ change.projection.allItems
    update (SetQuery q) =
      M.modify $ \m -> m {searchQuery = q, highlightIdx = Nothing}
    update AddAll = M.modify $ \m ->
      let query = parseQuery m.searchQuery
          selectedSet = Set.fromList m.selectedIds
          matches = getMatches cfg query selectedSet m.allItems
          newIds = map (cfg.itemId) matches
       in m
            { selectedIds = m.selectedIds <> newIds
            , searchQuery = ""
            , highlightIdx = Nothing
            }
    update AddHighlighted = M.modify $ \m ->
      case m.highlightIdx of
        Nothing -> m
        Just idx ->
          let query = parseQuery m.searchQuery
              selectedSet = Set.fromList m.selectedIds
              matches = getMatches cfg query selectedSet m.allItems
           in case drop idx matches of
                (item : _) ->
                  m
                    { selectedIds = m.selectedIds <> [cfg.itemId item]
                    , searchQuery = ""
                    , highlightIdx = Nothing
                    }
                [] -> m
    update (MoveHighlight delta) = M.modify $ \m ->
      let query = parseQuery m.searchQuery
          selectedSet = Set.fromList m.selectedIds
          matchCount = length (getMatches cfg query selectedSet m.allItems)
          newIdx = case m.highlightIdx of
            Nothing
              | delta > 0 -> Just 0
              | delta < 0 -> Just (matchCount - 1)
              | otherwise -> Nothing
            Just cur ->
              let next = cur + delta
               in if next < 0 || next >= matchCount
                    then Nothing
                    else Just next
       in m {highlightIdx = newIdx}
    update (RemoveItem i) =
      M.modify $ \m -> m {selectedIds = filter (/= i) m.selectedIds}
    update RemoveLast = M.modify $ \m ->
      case reverse m.selectedIds of
        [] -> m
        (_ : rest) -> m {selectedIds = reverse rest}
    update (ToggleItem i) = M.modify $ \m ->
      if i `elem` m.selectedIds
        then m {selectedIds = filter (/= i) m.selectedIds}
        else m {selectedIds = m.selectedIds <> [i]}
    update (SetFocus focused) =
      M.modify $ \m -> m {hasFocus = focused, highlightIdx = Nothing}
    update NoOp = pure ()

-- | Get matching items that are not already selected.
getMatches
  :: (Ord id) => SearchSelectConfig a id -> Query -> Set.Set id -> [a] -> [a]
getMatches cfg query selectedSet items =
  filter (matchItem cfg.itemLabel cfg.itemMetadata query)
    $ filter (\a -> not $ Set.member (cfg.itemId a) selectedSet) items

-- ============================================================================
-- View
-- ============================================================================

view
  :: (Eq id, Ord id)
  => SearchSelectConfig a id
  -> Model a id
  -> M.View (Model a id) (Action a id)
view cfg m =
  let query = parseQuery m.searchQuery
      selectedSet = Set.fromList m.selectedIds
      matches = getMatches cfg query selectedSet m.allItems
      selectedItems = resolveSelected cfg m
      tags = map (viewSelectedTag cfg) selectedItems
      inputArea =
        MH.input_
          [ class_ "flex-1 min-w-20 bg-transparent text-sm outline-none placeholder:text-muted-foreground"
          , MP.type_ "text"
          , MP.value_ (ms m.searchQuery)
          , MH.onInput (SetQuery . fromMisoString)
          , MP.placeholder_ (ms cfg.placeholder)
          ]
      popoverContent =
        if null matches
          then Nothing
          else Just $ viewSuggestions cfg m.highlightIdx matches
   in tagInput
        TagInputConfig
          { badges = tags
          , inputArea = inputArea
          , popover = popoverContent
          , hasFocus = m.hasFocus
          , onKeyDown = Just (handleKeyDown m)
          , onFocus = Just (SetFocus True)
          , onBlur = Just (SetFocus False)
          }

-- | Resolve selected IDs back to items, preserving selection order.
resolveSelected :: (Eq id) => SearchSelectConfig a id -> Model a id -> [a]
resolveSelected cfg m =
  concatMap (\i -> filter (\a -> cfg.itemId a == i) m.allItems) m.selectedIds

-- | Render a selected item as an interactive badge with a remove button.
viewSelectedTag
  :: SearchSelectConfig a id
  -> a
  -> M.View (Model a id) (Action a id)
viewSelectedTag cfg a =
  let (icn, label) = cfg.viewTag a
   in Badge.interactive
        Badge.Secondary
        (Just (Icon.IcnCancel, RemoveItem (cfg.itemId a)))
        (Badge.badgeIconText icn label)

-- | Render the suggestion dropdown.
viewSuggestions
  :: (Eq id, Ord id)
  => SearchSelectConfig a id
  -> Maybe Int
  -> [a]
  -> M.View (Model a id) (Action a id)
viewSuggestions cfg highlightIdx matches =
  MH.div_
    [class_ "space-y-0.5"]
    [ MH.div_
      [ class_ $
          "px-2 py-1.5 rounded-sm text-sm cursor-pointer "
            <> if highlightIdx == Just i then "bg-accent text-accent-foreground" else "hover:bg-accent"
      , MH.onClick (ToggleItem (cfg.itemId item))
      ]
      [M.text $ ms $ cfg.itemLabel item]
    | (i, item) <- zip [0 ..] matches
    ]

-- | Map keyboard events to actions.
-- Uses numeric key codes since Miso's KeyInfo provides keyCode, not key strings.
handleKeyDown :: Model a id -> M.KeyInfo -> Action a id
handleKeyDown m keyInfo =
  case keyInfo.keyCode of
    13 -> -- Enter
      case m.highlightIdx of
        Just _ -> AddHighlighted
        Nothing -> AddAll
    40 -> MoveHighlight 1 -- ArrowDown
    38 -> MoveHighlight (-1) -- ArrowUp
    8 -- Backspace
      | T.null m.searchQuery -> RemoveLast
    27 -> SetFocus False -- Escape
    _ -> NoOp
