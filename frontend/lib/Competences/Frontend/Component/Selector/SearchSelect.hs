module Competences.Frontend.Component.Selector.SearchSelect
  ( -- * Config
    SearchSelectConfig (..)

    -- * Meta filters
  , MetaFilter (..)
  , keywordsFilter

    -- * Component
  , searchSelectComponent

    -- * Viewer (read-only display for EditorField)
  , ViewerModel
  , ViewerAction
  , searchSelectViewerComponent

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
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Search (Query, QuerySegment (..), matchItemWithFilters, parseQuery, segmentQuery)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (fromMisoString, ms)
import Optics.Core qualified as O
import Optics.Core ((&), (.~))

-- ============================================================================
-- Config
-- ============================================================================

-- | A named meta filter with a hint string for tooltip display.
data MetaFilter a = MetaFilter
  { hint :: !Text
  , parser :: !(Text -> Maybe (a -> Bool))
  }

-- | Build a MetaFilter from keywords and a predicate.
-- The hint is auto-derived: @["hü", "hausübung"]@ → @"\@hü | \@hausübung"@
keywordsFilter :: [Text] -> (a -> Bool) -> MetaFilter a
keywordsFilter keywords predicate =
  MetaFilter
    { hint = T.intercalate " | " (map ("@" <>) keywords)
    , parser = \input ->
        if any (T.toLower input `T.isPrefixOf`) keywords
          then Just predicate
          else Nothing
    }

-- | Configuration for a generic search-select component.
data SearchSelectConfig a id = SearchSelectConfig
  { projectItems :: !(Document -> [a])
  -- ^ Extract all selectable items from the document
  , itemId :: !(a -> id)
  -- ^ Stable key for tracking selections across document updates
  , itemLabel :: !(a -> Text)
  -- ^ Primary identifier text (for display in dropdown AND text search)
  , metaFilters :: ![MetaFilter a]
  -- ^ Typed @-filters with hints for tooltip display.
  , viewTag :: !(a -> (Icon.Icon, M.MisoString))
  -- ^ Icon + label for the inline badge. Component handles x-button and remove action.
  , placeholder :: !Text
  }

-- | Extract parser functions from MetaFilter list (for Search module compatibility).
metaParsers :: [MetaFilter a] -> [Text -> Maybe (a -> Bool)]
metaParsers = map (.parser)

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
          m & #selectedIds .~ map cfg.itemId newSelected

    projection :: SearchSelectConfig a id -> Document -> Maybe user -> Projection a
    projection c doc _ = Projection {allItems = c.projectItems doc}

    update (ProjectionChanged change) =
      M.modify $ #allItems .~ change.projection.allItems
    update (SetQuery q) =
      M.modify $ \m -> m & #searchQuery .~ q & #highlightIdx .~ Nothing
    update AddAll = M.modify $ \m ->
      let query = parseQuery m.searchQuery
          selectedSet = Set.fromList m.selectedIds
          matches = getMatches cfg query selectedSet m.allItems
          newIds = map (cfg.itemId) matches
       in m & #selectedIds .~ (m.selectedIds <> newIds)
            & #searchQuery .~ ""
            & #highlightIdx .~ Nothing
    update AddHighlighted = M.modify $ \m ->
      case m.highlightIdx of
        Nothing -> m
        Just idx ->
          let query = parseQuery m.searchQuery
              selectedSet = Set.fromList m.selectedIds
              matches = getMatches cfg query selectedSet m.allItems
           in case drop idx matches of
                (item : _) ->
                  m & #selectedIds .~ (m.selectedIds <> [cfg.itemId item])
                    & #searchQuery .~ ""
                    & #highlightIdx .~ Nothing
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
       in m & #highlightIdx .~ newIdx
    update (RemoveItem i) =
      M.modify $ \m -> m & #selectedIds .~ filter (/= i) m.selectedIds
    update RemoveLast = M.modify $ \m ->
      case reverse m.selectedIds of
        [] -> m
        (_ : rest) -> m & #selectedIds .~ reverse rest
    update (ToggleItem i) = M.modify $ \m ->
      if i `elem` m.selectedIds
        then m & #selectedIds .~ filter (/= i) m.selectedIds
        else m & #selectedIds .~ (m.selectedIds <> [i])
    update (SetFocus focused) =
      M.modify $ \m -> m & #hasFocus .~ focused & #highlightIdx .~ Nothing
    update NoOp = pure ()

-- | Get matching items that are not already selected.
getMatches
  :: (Ord id) => SearchSelectConfig a id -> Query -> Set.Set id -> [a] -> [a]
getMatches cfg query selectedSet items =
  filter (matchItemWithFilters cfg.itemLabel (metaParsers cfg.metaFilters) query)
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
        MH.div_
          [class_ "relative flex-1 min-w-20"]
          [ -- Highlight layer: visible styled text behind the input
            MH.div_
              [class_ "text-sm whitespace-pre pointer-events-none overflow-hidden h-5"]
              (viewHighlightedQuery cfg m.searchQuery)
          , -- Actual input: transparent text, visible caret, on top
            MH.input_
              [ class_ "absolute inset-0 w-full bg-transparent text-sm text-transparent caret-foreground outline-none placeholder:text-muted-foreground"
              , MP.type_ "text"
              , MP.value_ (ms m.searchQuery)
              , MH.onInput (SetQuery . fromMisoString)
              , MP.placeholder_ (ms cfg.placeholder)
              ]
          ]
      popoverContent =
        if null matches
          then Nothing
          else Just $ viewSuggestions cfg m.highlightIdx matches
   in MH.div_
        [class_ "relative"]
        [ tagInput
            TagInputConfig
              { badges = tags
              , inputArea = inputArea
              , popover = popoverContent
              , hasFocus = m.hasFocus
              , onKeyDown = Just (handleKeyDown m)
              , onFocus = Just (SetFocus True)
              , onBlur = Just (SetFocus False)
              }
        , viewFilterHints cfg m.hasFocus
        ]

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

-- | Render highlighted query segments for the overlay layer.
viewHighlightedQuery
  :: SearchSelectConfig a id
  -> Text
  -> [M.View (Model a id) (Action a id)]
viewHighlightedQuery cfg queryText =
  map viewSegment (segmentQuery (metaParsers cfg.metaFilters) queryText)
  where
    viewSegment (PlainText t) = MH.span_ [] [M.text (ms t)]
    viewSegment (ResolvedFilter t) = MH.span_ [] [M.text (ms t)]
    viewSegment (UnresolvedFilter t) =
      MH.span_
        [class_ "text-destructive underline decoration-destructive/60"]
        [M.text (ms t)]

-- | Render filter hints below the input when focused.
-- Shows available @-filters as a tooltip. Hidden when not focused or no filters.
viewFilterHints
  :: SearchSelectConfig a id
  -> Bool
  -> M.View (Model a id) (Action a id)
viewFilterHints cfg focused
  | not focused || null cfg.metaFilters = M.text ""
  | otherwise =
      MH.div_
        [ class_ "absolute left-0 right-0 top-full mt-1 px-2 py-1.5 rounded-md bg-popover border border-border text-xs text-muted-foreground shadow-sm z-10"
        ]
        [ M.text $ ms $ T.intercalate ", " $ map (.hint) cfg.metaFilters
        ]

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

-- ============================================================================
-- Viewer (read-only display for EditorField)
-- ============================================================================

-- | Minimal model for the read-only viewer: just items + selected IDs.
data ViewerModel a id = ViewerModel
  { allItems :: ![a]
  , selectedIds :: ![id]
  }
  deriving (Eq, Generic, Show)

data ViewerAction a
  = ViewerProjectionChanged !(ProjectedChange (Projection a))
  deriving (Eq, Generic, Show)

-- | Read-only viewer component for SearchSelect.
-- Shows comma-separated item labels, or muted placeholder when empty.
-- Used as the viewer half of an EditorField.
searchSelectViewerComponent
  :: forall a id p f' a'
   . (Eq a, Eq id, Ord id, Show a, Show id)
  => SyncContext
  -> SearchSelectConfig a id
  -> [id]
  -> SelectorTransformedLens p [] a f' a'
  -> M.Component p (ViewerModel a id) (ViewerAction a)
searchSelectViewerComponent r cfg initIds lensBinding =
  (M.component model update viewViewer)
    { M.bindings = [mkSelectorBinding lensBinding viewerEntitiesLens]
    , M.subs = [subscribeWithProjection r (projection cfg) ViewerProjectionChanged]
    }
  where
    model =
      ViewerModel
        { allItems = []
        , selectedIds = initIds
        }

    viewerEntitiesLens :: O.Lens' (ViewerModel a id) [a]
    viewerEntitiesLens = O.lens getter setter
      where
        getter m =
          concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
        setter m newSelected =
          m & #selectedIds .~ map cfg.itemId newSelected

    projection :: SearchSelectConfig a id -> Document -> Maybe user -> Projection a
    projection c doc _ = Projection {allItems = c.projectItems doc}

    update (ViewerProjectionChanged change) =
      M.modify $ #allItems .~ change.projection.allItems

    viewViewer m =
      let resolved = concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
       in case resolved of
            [] -> Typography.muted (ms cfg.placeholder)
            items ->
              let labels = T.intercalate ", " (map cfg.itemLabel items)
                  count = T.pack $ " (" <> show (length items) <> ")"
               in MH.span_ [] [M.text $ ms $ labels <> count]
