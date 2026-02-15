module Competences.Frontend.Component.Selector.MultiSelectItemSelector
  ( multiSelectItemSelectorComponent
  , multiSelectItemViewerComponent
  )
where

import Competences.Document (Document (..), Resource (..), Task (..))
import Competences.Document.LessonNotes (LessonNoteItem (..))
import Competences.Document.Resource (ResourceId, ResourceIdentifier (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
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
import Competences.Query.Task qualified as QTask
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

-- | Projection from document - all resources and tasks
data SelectorProjection = SelectorProjection
  { allResources :: ![Resource]
  , allTasks :: ![Task]
  }
  deriving (Eq, Generic, Show)

-- | Projection function - gets all resources and tasks
selectorProjection :: Document -> Maybe user -> SelectorProjection
selectorProjection doc _ =
  SelectorProjection
    { allResources = QResource.allResources doc
    , allTasks = QTask.allTasksSorted doc
    }

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { projection :: !SelectorProjection
  , selectedItems :: ![LessonNoteItem]
  , resourceSearchQuery :: !Text
  , taskSearchQuery :: !Text
  , isResourceOpen :: !Bool
  , isTaskOpen :: !Bool
  , reorderState :: !ListReorderState
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = ProjectionChanged !(ProjectedChange SelectorProjection)
  | SetResourceSearchQuery !Text
  | SetTaskSearchQuery !Text
  | ToggleResourceItem !ResourceId
  | ToggleTaskItem !TaskId
  | SetResourceOpen !Bool
  | SetTaskOpen !Bool
  | ItemReorder !ListReorderAction
  | RemoveItem !Int
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Multi-select item selector component
-- Binds selected LessonNoteItems to parent model via lens
multiSelectItemSelectorComponent
  :: SyncContext
  -> [LessonNoteItem] -- ^ Initial selection
  -> SelectorTransformedLens p [] LessonNoteItem f' a'
  -> M.Component p Model Action
multiSelectItemSelectorComponent r initItems lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedItems]
    , M.subs = [subscribeWithProjection r selectorProjection ProjectionChanged]
    }
  where
    model =
      Model
        { projection = SelectorProjection [] []
        , selectedItems = initItems
        , resourceSearchQuery = ""
        , taskSearchQuery = ""
        , isResourceOpen = False
        , isTaskOpen = False
        , reorderState = initialListReorderState
        }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        m & #projection .~ change.projection

    update (SetResourceSearchQuery q) =
      M.modify $ #resourceSearchQuery .~ q

    update (SetTaskSearchQuery q) =
      M.modify $ #taskSearchQuery .~ q

    update (ToggleResourceItem resId) =
      M.modify $ \m ->
        let item = LessonResource resId
            current = m.selectedItems
            new =
              if item `elem` current
                then filter (/= item) current
                else current <> [item]
         in m & #selectedItems .~ new
              & #reorderState .~ initialListReorderState

    update (ToggleTaskItem taskId) =
      M.modify $ \m ->
        let item = LessonTask taskId
            current = m.selectedItems
            new =
              if item `elem` current
                then filter (/= item) current
                else current <> [item]
         in m & #selectedItems .~ new
              & #reorderState .~ initialListReorderState

    update (SetResourceOpen open) =
      M.modify $ #isResourceOpen .~ open

    update (SetTaskOpen open) =
      M.modify $ #isTaskOpen .~ open

    update (RemoveItem idx) =
      M.modify $ \m ->
        let current = m.selectedItems
            new = take idx current <> drop (idx + 1) current
         in m & #selectedItems .~ new
              & #reorderState .~ initialListReorderState

    update (ItemReorder (StartListReorder idx)) =
      M.modify $ \m -> m & #reorderState .~ ListReorderState (Just idx)

    update (ItemReorder CancelListReorder) =
      M.modify $ \m -> m & #reorderState .~ initialListReorderState

    update (ItemReorder (ListReorderTo src tgt)) =
      M.modify $ \m ->
        m & #selectedItems .~ moveElement src tgt m.selectedItems
          & #reorderState .~ initialListReorderState

    view m =
      MH.div_
        [class_ "space-y-3"]
        [ -- Resource combobox
          MH.div_
            [class_ "space-y-1"]
            [ let filteredResources = filterResources m.resourceSearchQuery m.projection.allResources
                  selectedResourceIds = Set.fromList [rid | LessonResource rid <- m.selectedItems]
                  options =
                    [ Combobox.ComboboxOption res.id (unResourceIdentifier res.identifier)
                    | res <- filteredResources
                    ]
               in Combobox.multiSelectCombobox SetResourceSearchQuery ToggleResourceItem SetResourceOpen
                    & Combobox.withPlaceholder (M.fromMisoString $ C.translate' C.LblSelectResources)
                    & Combobox.withOptions options
                    & Combobox.withSelected selectedResourceIds
                    & Combobox.withSearchQuery m.resourceSearchQuery
                    & Combobox.withIsOpen m.isResourceOpen
                    & Combobox.renderCombobox
            ]
        , -- Task combobox
          MH.div_
            [class_ "space-y-1"]
            [ let filteredTasks = filterTasks m.taskSearchQuery m.projection.allTasks
                  selectedTaskIds = Set.fromList [tid | LessonTask tid <- m.selectedItems]
                  options =
                    [ Combobox.ComboboxOption t.id (unTaskIdentifier t.identifier)
                    | t <- filteredTasks
                    ]
               in Combobox.multiSelectCombobox SetTaskSearchQuery ToggleTaskItem SetTaskOpen
                    & Combobox.withPlaceholder (M.fromMisoString $ C.translate' C.LblSelectTasks)
                    & Combobox.withOptions options
                    & Combobox.withSelected selectedTaskIds
                    & Combobox.withSearchQuery m.taskSearchQuery
                    & Combobox.withIsOpen m.isTaskOpen
                    & Combobox.renderCombobox
            ]
        , -- Display selected items as vertical list with reorder/remove buttons
          if null m.selectedItems
            then M.text ""
            else
              Layout.vFlow
                Layout.gapT
                [ viewItem m idx item
                | (idx, item) <- zip [0 ..] m.selectedItems
                ]
        ]

    filterResources query resources =
      let q = T.toLower query
       in if T.null q
            then resources
            else filter (\res -> q `T.isInfixOf` T.toLower (unResourceIdentifier res.identifier)) resources

    filterTasks query tasks =
      let q = T.toLower query
       in if T.null q
            then tasks
            else filter (\t -> q `T.isInfixOf` T.toLower (unTaskIdentifier t.identifier)) tasks

    unResourceIdentifier (ResourceIdentifier t) = t
    unTaskIdentifier (TaskIdentifier t) = t

    lookupResource resId resources =
      case filter (\res -> res.id == resId) resources of
        (res : _) -> Just res
        [] -> Nothing

    lookupTask taskId tasks =
      case filter (\t -> t.id == taskId) tasks of
        (t : _) -> Just t
        [] -> Nothing

    viewItem :: Model -> Int -> LessonNoteItem -> M.View Model Action
    viewItem m idx item =
      case item of
        LessonResource resId ->
          case lookupResource resId m.projection.allResources of
            Just res ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 rounded border border-border bg-background"]
                [ Icon.iconS Icon.Small Icon.IcnResources
                , MH.span_ [class_ "flex-1 text-sm truncate"] [M.text $ M.ms $ unResourceIdentifier res.identifier]
                , reorderButtons m.reorderState idx (RemoveItem idx)
                ]
            Nothing ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 rounded border border-border bg-background text-muted-foreground"]
                [ Icon.iconS Icon.Small Icon.IcnResources
                , MH.span_ [class_ "flex-1 text-sm truncate italic"] [M.text "(unknown resource)"]
                , reorderButtons m.reorderState idx (RemoveItem idx)
                ]
        LessonTask taskId ->
          case lookupTask taskId m.projection.allTasks of
            Just t ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 rounded border border-border bg-background"]
                [ Icon.iconS Icon.Small Icon.IcnTask
                , MH.span_ [class_ "flex-1 text-sm truncate"] [M.text $ M.ms $ unTaskIdentifier t.identifier]
                , reorderButtons m.reorderState idx (RemoveItem idx)
                ]
            Nothing ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 rounded border border-border bg-background text-muted-foreground"]
                [ Icon.iconS Icon.Small Icon.IcnTask
                , MH.span_ [class_ "flex-1 text-sm truncate italic"] [M.text "(unknown task)"]
                , reorderButtons m.reorderState idx (RemoveItem idx)
                ]

    reorderButtons :: ListReorderState -> Int -> Action -> M.View Model Action
    reorderButtons st idx removeAction =
      MH.div_
        [class_ "flex items-center gap-0.5"]
        (case listReorderButtons st idx of
          ShowReorderStart ->
            [ Button.ghostSm (Button.button Icon.IcnReorder (ItemReorder (StartListReorder idx)))
            , Button.ghostSm (Button.button Icon.IcnCancel removeAction)
            ]
          ShowReorderCancel ->
            [ Button.ghostSm (Button.button Icon.IcnCancel (ItemReorder CancelListReorder))
            ]
          ShowReorderTargets fromIdx thisIdx ->
            [ Button.ghostSm (Button.button Icon.IcnArrowUp (ItemReorder (ListReorderTo fromIdx thisIdx)))
            , Button.ghostSm (Button.button Icon.IcnArrowDown (ItemReorder (ListReorderTo fromIdx (thisIdx + 1))))
            ])

-- ============================================================================
-- Viewer Component (read-only)
-- ============================================================================

-- | Read-only viewer for selected items.
-- Shows a simple list of resource/task names without combobox or action buttons.
data ViewerModel = ViewerModel
  { projection :: !SelectorProjection
  , selectedItems :: ![LessonNoteItem]
  }
  deriving (Eq, Generic, Show)

newtype ViewerAction = ViewerProjectionChanged (ProjectedChange SelectorProjection)
  deriving (Eq, Show)

multiSelectItemViewerComponent
  :: SyncContext
  -> [LessonNoteItem]
  -> SelectorTransformedLens p [] LessonNoteItem f' a'
  -> M.Component p ViewerModel ViewerAction
multiSelectItemViewerComponent r initItems lensBinding =
  (M.component viewerModel viewerUpdate viewerView)
    { M.bindings = [mkSelectorBinding lensBinding #selectedItems]
    , M.subs = [subscribeWithProjection r selectorProjection ViewerProjectionChanged]
    }
  where
    viewerModel =
      ViewerModel
        { projection = SelectorProjection [] []
        , selectedItems = initItems
        }

    viewerUpdate (ViewerProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    viewerView m =
      if null m.selectedItems
        then M.text ""
        else
          Layout.vFlow
            Layout.gapT
            [ viewItemReadOnly m item
            | item <- m.selectedItems
            ]

    viewItemReadOnly m item =
      case item of
        LessonResource resId ->
          case lookupResource resId m.projection.allResources of
            Just res ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1"]
                [ Icon.iconS Icon.Small Icon.IcnResources
                , MH.span_ [class_ "text-sm"] [M.text $ M.ms $ unResourceIdentifier res.identifier]
                ]
            Nothing ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 text-muted-foreground"]
                [ Icon.iconS Icon.Small Icon.IcnResources
                , MH.span_ [class_ "text-sm italic"] [M.text "(unknown resource)"]
                ]
        LessonTask taskId ->
          case lookupTask taskId m.projection.allTasks of
            Just t ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1"]
                [ Icon.iconS Icon.Small Icon.IcnTask
                , MH.span_ [class_ "text-sm"] [M.text $ M.ms $ unTaskIdentifier t.identifier]
                ]
            Nothing ->
              MH.div_
                [class_ "flex items-center gap-2 px-2 py-1 text-muted-foreground"]
                [ Icon.iconS Icon.Small Icon.IcnTask
                , MH.span_ [class_ "text-sm italic"] [M.text "(unknown task)"]
                ]

    unResourceIdentifier (ResourceIdentifier t) = t
    unTaskIdentifier (TaskIdentifier t) = t

    lookupResource resId resources =
      case filter (\res -> res.id == resId) resources of
        (res : _) -> Just res
        [] -> Nothing

    lookupTask taskId tasks =
      case filter (\t -> t.id == taskId) tasks of
        (t : _) -> Just t
        [] -> Nothing
