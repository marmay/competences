module Competences.Frontend.Component.Selector.SearchSelect
  ( -- * Config
    SearchSelectConfig (..)
  , SelectionOrder (..)
  , TagLayout (..)

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

import Control.Monad (when)
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
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.TagInput (TagInputConfig (..), TagLayout (..), tagInput)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Search (Query, QuerySegment (..), matchItemWithFilters, parseQuery, segmentQuery)
import Data.List (elemIndex)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.DSL (Object (..), create, isNull, jsg, setProp, toJSVal, (#))
import Miso.Html qualified as MH
import Miso.Html.Event qualified as ME
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, fromMisoString, ms)
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

-- | Controls how selected items are ordered in the tag list.
data SelectionOrder a
  = -- | Show reorder buttons on tags; user controls order via up/down
    ManualReorder
  | -- | Post-process resolved items (e.g. @sortOn (.name)@).
    -- @AutoOrder id@ = insertion order (current default).
    AutoOrder !([a] -> [a])

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
  , selectionOrder :: !(SelectionOrder a)
  -- ^ How selected items are ordered: manual reorder buttons or automatic sorting.
  , tagLayout :: !TagLayout
  -- ^ How tags are laid out: inline (default) or vertical (one per line).
  , onCreate :: !(Maybe (IO id))
  -- ^ Optional callback to create a new item. Returns the new item's id.
  -- SearchSelect will add the id to selectedIds automatically.
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
  , reorderingItem :: !(Maybe id)
  , draggingItem :: !(Maybe id)
  , dragOverGap :: !(Maybe Int)
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
  | ClearAll
  | -- Button-based reorder
    StartReorder !id
  | CancelReorder
  | InsertBefore !id
  | InsertAfter !id
  | MoveToFront
  | MoveToBack
  | -- Drag-and-drop
    DragStart !id
  | DragOverGap !Int
  | DragDropAt !Int
  | DragEnd
  | SetFocus !Bool
  | CreateNew
  | ItemCreated !id
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
  -> MisoString
  -- ^ Unique key for this instance (used to derive a unique DOM id)
  -> SearchSelectConfig a id
  -> [id]
  -> SelectorTransformedLens p [] a f' a'
  -> M.Component p (Model a id) (Action a id)
searchSelectComponent r instanceKey cfg initIds lensBinding =
  (M.component model update (view inputId cfg))
    { M.bindings = [mkSelectorBinding lensBinding selectedEntitiesLens]
    , M.subs = [subscribeWithProjection r (projection cfg) ProjectionChanged]
    }
  where
    inputId = instanceKey <> "-input"

    model =
      Model
        { allItems = []
        , selectedIds = initIds
        , searchQuery = ""
        , highlightIdx = Nothing
        , hasFocus = False
        , reorderingItem = Nothing
        , draggingItem = Nothing
        , dragOverGap = Nothing
        }

    -- | Virtual lens: reads [a] (resolved from selectedIds), writes back [id].
    selectedEntitiesLens :: O.Lens' (Model a id) [a]
    selectedEntitiesLens = O.lens getter setter
      where
        getter m =
          applyOrder cfg.selectionOrder $
            concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
        setter m newSelected =
          m & #selectedIds .~ map cfg.itemId newSelected

    projection :: SearchSelectConfig a id -> Document -> Maybe user -> Projection a
    projection c doc _ = Projection {allItems = c.projectItems doc}

    update (ProjectionChanged change) =
      M.modify $ #allItems .~ change.projection.allItems
    update (SetQuery q) =
      M.modify $ \m -> m & #searchQuery .~ q & #highlightIdx .~ Nothing
    update AddAll = do
      M.modify $ \m ->
        let query = parseQuery m.searchQuery
            selectedSet = Set.fromList m.selectedIds
            matches = getMatches cfg query selectedSet m.allItems
            newIds = map (cfg.itemId) matches
         in m & #selectedIds .~ (m.selectedIds <> newIds)
              & #searchQuery .~ ""
              & #highlightIdx .~ Nothing
      M.io_ $ M.focus inputId
    update AddHighlighted = do
      M.modify $ \m ->
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
      M.io_ $ M.focus inputId
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
    update RemoveLast = do
      M.modify $ \m ->
        case reverse m.selectedIds of
          [] -> m
          (_ : rest) -> m & #selectedIds .~ reverse rest
      M.io_ $ M.focus inputId
    update (ToggleItem i) = do
      M.modify $ \m ->
        if i `elem` m.selectedIds
          then m & #selectedIds .~ filter (/= i) m.selectedIds
          else m & #selectedIds .~ (m.selectedIds <> [i])
      M.io_ $ M.focus inputId
    update ClearAll =
      M.modify $ #selectedIds .~ []
    update (StartReorder i) =
      M.modify $ #reorderingItem .~ Just i
    update CancelReorder =
      M.modify $ #reorderingItem .~ Nothing
    update (InsertBefore targetId) = M.modify $ \m ->
      case m.reorderingItem of
        Nothing -> m
        Just src -> m & #selectedIds .~ insertBeforeId src targetId m.selectedIds
                      & #reorderingItem .~ Nothing
    update (InsertAfter targetId) = M.modify $ \m ->
      case m.reorderingItem of
        Nothing -> m
        Just src -> m & #selectedIds .~ insertAfterId src targetId m.selectedIds
                      & #reorderingItem .~ Nothing
    update MoveToFront = M.modify $ \m ->
      case m.reorderingItem of
        Nothing -> m
        Just src -> m & #selectedIds .~ moveToFrontId src m.selectedIds
                      & #reorderingItem .~ Nothing
    update MoveToBack = M.modify $ \m ->
      case m.reorderingItem of
        Nothing -> m
        Just src -> m & #selectedIds .~ moveToBackId src m.selectedIds
                      & #reorderingItem .~ Nothing
    update (DragStart i) =
      M.modify $ #draggingItem .~ Just i
    update (DragOverGap i) =
      M.modify $ #dragOverGap .~ Just i
    update (DragDropAt gapIdx) = M.modify $ \m ->
      case m.draggingItem of
        Nothing -> m
        Just src -> m & #selectedIds .~ insertAtGap src gapIdx m.selectedIds
                      & #draggingItem .~ Nothing
                      & #dragOverGap .~ Nothing
    update DragEnd =
      M.modify $ \m -> m & #draggingItem .~ Nothing & #dragOverGap .~ Nothing
    update (SetFocus focused) = do
      M.modify $ \m -> m & #hasFocus .~ focused & #highlightIdx .~ Nothing
      M.io_ $ when focused (scrollInputToCenter inputId)
    update CreateNew = case cfg.onCreate of
      Nothing -> pure ()
      Just createAction -> M.withSink $ \sink -> do
        newId <- createAction
        sink (ItemCreated newId)
    update (ItemCreated newId) =
      M.modify $ \m -> m & #selectedIds .~ (m.selectedIds <> [newId])
    update NoOp = pure ()

-- | Get matching items that are not already selected.
getMatches
  :: (Ord id) => SearchSelectConfig a id -> Query -> Set.Set id -> [a] -> [a]
getMatches cfg query selectedSet items =
  filter (matchItemWithFilters cfg.itemLabel (metaParsers cfg.metaFilters) query)
    $ filter (\a -> not $ Set.member (cfg.itemId a) selectedSet) items

-- | Apply ordering to resolved items.
applyOrder :: SelectionOrder a -> [a] -> [a]
applyOrder ManualReorder xs = xs
applyOrder (AutoOrder f) xs = f xs

-- | Remove item from list, insert before target.
insertBeforeId :: (Eq id) => id -> id -> [id] -> [id]
insertBeforeId src target xs =
  let without = filter (/= src) xs
   in concatMap (\x -> if x == target then [src, x] else [x]) without

-- | Remove item from list, insert after target.
insertAfterId :: (Eq id) => id -> id -> [id] -> [id]
insertAfterId src target xs =
  let without = filter (/= src) xs
   in concatMap (\x -> if x == target then [x, src] else [x]) without

-- | Remove item, prepend.
moveToFrontId :: (Eq id) => id -> [id] -> [id]
moveToFrontId src xs = src : filter (/= src) xs

-- | Remove item, append.
moveToBackId :: (Eq id) => id -> [id] -> [id]
moveToBackId src xs = filter (/= src) xs <> [src]

-- | Remove source, insert at gap position (0 = before first, n = after last).
-- The gap index refers to the original list; adjusted after removing source.
insertAtGap :: (Eq id) => id -> Int -> [id] -> [id]
insertAtGap src gapIdx ids =
  let srcIdx = maybe 0 (\x -> x) (elemIndex src ids)
      without = filter (/= src) ids
      adjusted = if srcIdx < gapIdx then gapIdx - 1 else gapIdx
      (before, after) = splitAt adjusted without
   in before <> [src] <> after

-- ============================================================================
-- View
-- ============================================================================

view
  :: forall a id
   . (Eq id, Ord id)
  => MisoString
  -> SearchSelectConfig a id
  -> Model a id
  -> M.View (Model a id) (Action a id)
view inputId cfg m =
  let query = parseQuery m.searchQuery
      selectedSet = Set.fromList m.selectedIds
      matches = getMatches cfg query selectedSet m.allItems
      selectedItems = resolveSelected cfg m
      tags = case cfg.selectionOrder of
        ManualReorder ->
          [ viewReorderableTag cfg m idx a
          | (idx, a) <- zip [0 ..] selectedItems
          ]
        AutoOrder _ ->
          map (viewSelectedTag cfg) selectedItems
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
              , MP.id_ inputId
              , MP.value_ (ms m.searchQuery)
              , MH.onInput (SetQuery . fromMisoString)
              , MP.placeholder_ (ms cfg.placeholder)
              , MP.autocomplete_ False
              ]
          ]
      hasClearAll = length selectedItems >= 2
      hasFilters = not (null cfg.metaFilters)
      showBox = m.hasFocus && (not (null matches) || hasFilters)
      tagInputView =
        tagInput
          TagInputConfig
            { badges = tags
            , inputArea = inputArea
            , popover = Nothing
            , hasFocus = m.hasFocus
            , onKeyDown = Just (handleKeyDown m)
            , onFocus = Just (SetFocus True)
            , onBlur = Just (SetFocus False)
            , tagLayout = cfg.tagLayout
            }
      mainContent = case cfg.onCreate of
        Nothing -> tagInputView
        Just _ ->
          MH.div_
            [class_ "flex items-start gap-2"]
            [ MH.div_ [class_ "flex-1 min-w-0"] [tagInputView]
            , Button.outlineSm $ Button.button Icon.IcnAdd (CreateNew :: Action a id)
            ]
   in MH.div_
        [class_ "relative"]
        [ mainContent
        , viewBelowInput cfg hasClearAll hasFilters showBox m.highlightIdx matches
        ]

-- | Resolve selected IDs back to items, applying configured ordering.
resolveSelected :: (Eq id) => SearchSelectConfig a id -> Model a id -> [a]
resolveSelected cfg m =
  applyOrder cfg.selectionOrder $
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

-- | Render a selected item in ManualReorder mode.
-- Supports drag-and-drop and button-based reorder depending on model state.
viewReorderableTag
  :: forall a id
   . (Eq id)
  => SearchSelectConfig a id
  -> Model a id
  -> Int
  -> a
  -> M.View (Model a id) (Action a id)
viewReorderableTag cfg m idx a =
  let thisId = cfg.itemId a
      (icn, label) = cfg.viewTag a
      badgeContent = case cfg.tagLayout of
        TagsVertical ->
          MH.span_ [class_ "inline-flex items-center gap-1 flex-1 min-w-0"]
            [Icon.icon [] icn, MH.span_ [class_ "truncate"] [M.text label]]
        TagsInline -> Badge.badgeIconText icn label
   in case m.reorderingItem of
        -- Button reorder mode: this is the source item
        Just srcId | srcId == thisId ->
          viewReorderSourceTag cfg.tagLayout badgeContent
        -- Button reorder mode: this is a target item
        Just _srcId ->
          viewReorderTargetTag cfg.tagLayout thisId badgeContent
        -- Normal state: draggable tag with hover actions
        Nothing ->
          viewDraggableTag cfg.tagLayout m.draggingItem m.dragOverGap idx thisId badgeContent

-- | Source tag in button-reorder mode: highlighted with move-to-front/back/cancel.
viewReorderSourceTag
  :: forall a id
   . TagLayout
  -> M.View (Model a id) (Action a id)
  -> M.View (Model a id) (Action a id)
viewReorderSourceTag layout badgeContent =
  MH.span_ [class_ $ verticalStretchCls layout]
    [ Badge.withActions Badge.Outline
        [ (Icon.IcnDoubleArrowUp, MoveToFront)
        , (Icon.IcnDoubleArrowDown, MoveToBack)
        , (Icon.IcnCancel, CancelReorder)
        ]
        badgeContent
    ]

-- | Target tag in button-reorder mode: insert-before/after buttons inside badge.
viewReorderTargetTag
  :: forall a id
   . TagLayout
  -> id
  -> M.View (Model a id) (Action a id)
  -> M.View (Model a id) (Action a id)
viewReorderTargetTag layout thisId badgeContent =
  MH.span_ [class_ $ verticalStretchCls layout]
    [ Badge.withActions Badge.Secondary
        [ (Icon.IcnArrowDown, InsertBefore thisId)
        , (Icon.IcnArrowUp, InsertAfter thisId)
        ]
        badgeContent
    ]

-- | CSS class to make a badge stretch full-width in vertical layout.
verticalStretchCls :: TagLayout -> Text
verticalStretchCls TagsVertical = "[&_.badge-secondary]:w-full [&_.badge-outline]:w-full"
verticalStretchCls TagsInline = ""

-- | Normal state: draggable tag with overlay drop zones for DnD.
-- Each tag is split into two invisible overlay halves (before/after).
-- Overlays are always in the DOM but only interactive during an active drag.
-- Visual indicator: a 2px primary-colored border on the insertion edge.
viewDraggableTag
  :: forall a id
   . (Eq id)
  => TagLayout
  -> Maybe id
  -> Maybe Int
  -> Int
  -> id
  -> M.View (Model a id) (Action a id)
  -> M.View (Model a id) (Action a id)
viewDraggableTag layout mDraggingItem mDragOverGap idx thisId badgeContent =
  let isDragging = mDraggingItem /= Nothing
      isSelf = mDraggingItem == Just thisId
      showBefore = mDragOverGap == Just idx
      showAfter = mDragOverGap == Just (idx + 1)
      indicatorCls = case layout of
        TagsVertical ->
          (if showBefore then " border-t-2 border-t-primary" else "")
            <> (if showAfter then " border-b-2 border-b-primary" else "")
        TagsInline ->
          (if showBefore then " border-l-2 border-l-primary" else "")
            <> (if showAfter then " border-r-2 border-r-primary" else "")
      overlayPE = if isDragging then "pointer-events-auto" else "pointer-events-none"
      -- Overlays extend into the flex gap (gap-1.5 = 6px, so 3px each side)
      -- so there are no dead zones between tags.
      (beforeOverlayCls, afterOverlayCls) = case layout of
        TagsVertical ->
          ( "absolute inset-x-0 -top-[3px] bottom-1/2 z-10 " <> overlayPE
          , "absolute inset-x-0 top-1/2 -bottom-[3px] z-10 " <> overlayPE
          )
        TagsInline ->
          ( "absolute inset-y-0 -left-[3px] right-1/2 z-10 " <> overlayPE
          , "absolute inset-y-0 left-1/2 -right-[3px] z-10 " <> overlayPE
          )
      opacityCls = if isSelf then " opacity-40" else ""
      stretchCls = case layout of
        TagsVertical -> " w-full [&_.badge-secondary]:w-full"
        TagsInline -> ""
   in MH.div_
        [class_ $ "relative rounded-md" <> indicatorCls <> opacityCls <> stretchCls]
        [ MH.span_
            [ class_ $ case layout of TagsVertical -> "w-full"; TagsInline -> ""
            , MP.draggable_ True
            , ME.onDragStart (DragStart thisId)
            , ME.onDragEnd DragEnd
            ]
            [ Badge.interactiveMulti Badge.Secondary
                [ (Icon.IcnReorder, StartReorder thisId)
                , (Icon.IcnCancel, RemoveItem thisId)
                ]
                badgeContent
            ]
        , -- Before-half overlay
          MH.div_
            [ class_ beforeOverlayCls
            , ME.onDragOverWithOptions M.preventDefault (DragOverGap idx)
            , ME.onDrop M.preventDefault (DragDropAt idx)
            ]
            []
        , -- After-half overlay
          MH.div_
            [ class_ afterOverlayCls
            , ME.onDragOverWithOptions M.preventDefault (DragOverGap (idx + 1))
            , ME.onDrop M.preventDefault (DragDropAt (idx + 1))
            ]
            []
        ]

-- | Container below the tag input: holds clear-all button (in flow) and suggestion box (absolute overlay).
viewBelowInput
  :: forall a id
   . (Eq id, Ord id)
  => SearchSelectConfig a id
  -> Bool
  -- ^ Has clear-all button (≥ 2 selected)
  -> Bool
  -- ^ Has meta filters
  -> Bool
  -- ^ Show suggestion box
  -> Maybe Int
  -- ^ Highlight index
  -> [a]
  -- ^ Matches
  -> M.View (Model a id) (Action a id)
viewBelowInput cfg hasClearAll hasFilters showBox highlightIdx matches =
  MH.div_
    [class_ "relative"]
    [ -- Clear-all button: in flow so it reserves space
      if hasClearAll
        then
          MH.div_
            [class_ "flex justify-end"]
            [Button.ghostSm $ Button.button C.LblDeselectAll (ClearAll :: Action a id)]
        else M.text ""
    , -- Suggestion box: absolute overlay below
      if showBox
        then
          MH.div_
            [ class_
                "absolute left-0 right-0 top-0 mt-1 z-50 bg-popover border border-border \
                \rounded-md shadow-lg p-2 max-h-48 overflow-y-auto"
            ]
            ( [viewFilterHintRow cfg | hasFilters]
                <> [viewSuggestions cfg highlightIdx matches | not (null matches)]
            )
        else M.text ""
    , -- Invisible sentinel: absolutely positioned below the suggestion area.
      -- Guarantees the page is tall enough to scroll the input to center.
      -- Always in the DOM, doesn't affect layout.
      MH.div_ [class_ "absolute left-0 top-full mt-52 h-px w-px"] []
    ]

-- | Scroll the search input to the center of the viewport.
scrollInputToCenter :: MisoString -> IO ()
scrollInputToCenter elemId = do
  doc <- jsg ("document" :: MisoString)
  el <- doc # ("getElementById" :: MisoString) $ [toJSVal elemId]
  elIsNull <- isNull el
  when (not elIsNull) $ do
    Object opts <- create
    setProp ("block" :: MisoString) ("center" :: MisoString) (Object opts)
    setProp ("behavior" :: MisoString) ("smooth" :: MisoString) (Object opts)
    _ <- el # ("scrollIntoView" :: MisoString) $ [opts]
    pure ()

-- | Filter hints rendered as a muted italic header inside the suggestion box.
viewFilterHintRow
  :: SearchSelectConfig a id
  -> M.View (Model a id) (Action a id)
viewFilterHintRow cfg =
  MH.div_
    [class_ "px-2 py-1 text-xs text-muted-foreground italic border-b border-border mb-1"]
    [M.text $ ms $ "Filter: " <> T.intercalate ", " (map (.hint) cfg.metaFilters)]

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
          applyOrder cfg.selectionOrder $
            concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
        setter m newSelected =
          m & #selectedIds .~ map cfg.itemId newSelected

    projection :: SearchSelectConfig a id -> Document -> Maybe user -> Projection a
    projection c doc _ = Projection {allItems = c.projectItems doc}

    update (ViewerProjectionChanged change) =
      M.modify $ #allItems .~ change.projection.allItems

    viewViewer m =
      let resolved =
            applyOrder cfg.selectionOrder $
              concatMap (\i -> filter (\x -> cfg.itemId x == i) m.allItems) m.selectedIds
       in case resolved of
            [] -> Typography.muted (ms cfg.placeholder)
            items ->
              let labels = T.intercalate ", " (map cfg.itemLabel items)
                  count = T.pack $ " (" <> show (length items) <> ")"
               in MH.span_ [] [M.text $ ms $ labels <> count]
