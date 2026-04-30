module Competences.Frontend.Component.Selector.TaskSelector
  ( SelectedTask (..)
  , TaskSelectorConfig (..)
  , defaultTaskSelectorConfig
  , taskSelectorComponent
  )
where

import Competences.Command (Command (..), DraftTasksCommand (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskIxs)
import Competences.Document.Task (TaskId, TaskIdentifier (..), defaultTask, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.SyncDocument (isInitialUpdate)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | A selected task with its origin (published or draft).
data SelectedTask = SelectedTask
  { origin :: !EntityOrigin
  , task :: !Task
  }
  deriving (Eq, Show)

-- | Customisations for the task selector. All fields default to 'Nothing';
-- enable the ones you need at the call site via record-update on
-- 'defaultTaskSelectorConfig'.
data TaskSelectorConfig = TaskSelectorConfig
  { initialSelection :: !(Maybe (Ix.IxSet TaskIxs Task -> Set TaskId -> Maybe SelectedTask))
  -- ^ Fallback selection applied on first document load when no task
  -- is selected yet (e.g. smart default, or the deep-linked task).
  , uriExtractor :: !(Maybe (M.URI -> Maybe TaskId))
  -- ^ Pull our entity ID out of a URI. When set, the selector subscribes
  -- to URI changes and updates its selection on back/forward navigation.
  -- Returning 'Nothing' (URI doesn't apply) leaves the current selection
  -- alone.
  , onSelect :: !(Maybe (SelectedTask -> IO ()))
  -- ^ Run on user click. Typically pushes the URL via 'M.pushURI'.
  }

defaultTaskSelectorConfig :: TaskSelectorConfig
defaultTaskSelectorConfig =
  TaskSelectorConfig
    { initialSelection = Nothing
    , uriExtractor = Nothing
    , onSelect = Nothing
    }

data Model = Model
  { allTasks :: !(Ix.IxSet TaskIxs Task)
  , draftTaskIds :: !(Set TaskId)
  , selectedItem :: !(Maybe SelectedTask)
  , newItem :: !(Maybe SelectedTask)
  , dropdownOpen :: !Bool
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !SelectedTask
  | CreateNewTask
  | CreateNewDraftTask
  | ToggleDropdown
  | CloseDropdown
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  | UriChanged !M.URI
  deriving (Eq, Show)

taskSelectorComponent
  :: SyncContext
  -> TaskSelectorConfig
  -> Lens' p (Maybe SelectedTask)
  -> M.Component p Model Action
taskSelectorComponent r cfg parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = subscribeDocument r UpdateDocument : [M.uriSub UriChanged | Just _ <- [cfg.uriExtractor]]
    }
  where
    model = Model Ix.empty Set.empty Nothing Nothing False ""

    update (SelectItem item) = do
      M.modify $ \m ->
        case Ix.getOne (m.allTasks Ix.@= item.task.id) of
          Just t' -> m & (#selectedItem ?~ SelectedTask item.origin t') & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)
      case cfg.onSelect of
        Just f -> M.io_ (f item)
        Nothing -> pure ()

    update (UriChanged uri) = do
      m <- M.get
      case cfg.uriExtractor of
        Just extract
          | Just tid <- extract uri
          , maybe True (\sel -> sel.task.id /= tid) m.selectedItem
          , Just t <- Ix.getOne (m.allTasks Ix.@= tid) ->
              let origin = if Set.member tid m.draftTaskIds then Draft else Published
               in M.modify $ \mm ->
                    mm & (#selectedItem ?~ SelectedTask origin t) & (#newItem .~ Nothing)
        _ -> pure ()

    update CreateNewTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = defaultTask taskId
      modifySyncDocument r $ Tasks (OnTasks (CreateAndLock newTask))
      s CloseDropdown
      s (SelectItem $ SelectedTask Published newTask)

    update CreateNewDraftTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = defaultTask taskId
      modifySyncDocument r $ DraftTasks (OnDraftTasks (CreateAndLock newTask))
      s CloseDropdown
      s (SelectItem $ SelectedTask Draft newTask)

    update ToggleDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ not m.dropdownOpen

    update CloseDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ False

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          realTasks = Ix.toList doc.tasks
          draftTasks = Ix.toList doc.draftTasks
          mergedTasks = Ix.fromList (realTasks <> draftTasks)
          draftTaskIds' = Set.fromList $ map (.id) draftTasks
          taskOrigin tid = if Set.member tid draftTaskIds' then Draft else Published
          validatedSelected = case m.selectedItem of
            Just st ->
              (\t' -> SelectedTask (taskOrigin t'.id) t') <$> Ix.getOne (mergedTasks Ix.@= st.task.id)
            Nothing -> Nothing
          validatedNew = case m.newItem of
            Just st ->
              case Ix.getOne (mergedTasks Ix.@= st.task.id) of
                Just t' -> Just (SelectedTask (taskOrigin t'.id) t')
                Nothing -> m.newItem
            Nothing -> Nothing
          -- Apply initial selection on first document load when nothing is selected
          selected' = case (isInitialUpdate dc.change, validatedSelected, cfg.initialSelection) of
            (True, Nothing, Just f) -> f mergedTasks draftTaskIds'
            _ -> validatedSelected
       in m
            { allTasks = mergedTasks
            , draftTaskIds = draftTaskIds'
            , selectedItem = selected'
            , newItem = validatedNew
            }

    view' m =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ SL.selectorHeaderWithDropdown
                (C.translate' C.LblTasks)
                m.dropdownOpen
                ToggleDropdown
                [ SL.dropdownItem Icon.IcnTask (C.translate' C.LblNewTask) CreateNewTask
                , SL.dropdownItem Icon.IcnTask (C.translate' C.LblNewDraftTask) CreateNewDraftTask
                ]
            , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterTasks) (SetSearchQuery . M.fromMisoString)
            , viewItems m
            ]
        ]

    viewItems m =
      let items =
            [ SelectedTask (if Set.member t.id m.draftTaskIds then Draft else Published) t
            | t <- Ix.toAscList (Proxy @TaskIdentifier) m.allTasks
            ]
          query = T.toLower m.searchQuery
          filteredItems =
            if T.null query
              then items
              else filter (\st -> query `T.isInfixOf` T.toLower (taskDisplayName st.task)) items
       in SL.selectorList (map (viewItem m) filteredItems)

    viewItem m st =
      let isSelected = m.selectedItem == Just st || m.newItem == Just st
          draftBadge = case st.origin of
            Draft -> Just $ Badge.secondary (Badge.badgeText (C.translate' C.LblDraft))
            Published -> Nothing
       in SL.selectorItemWithBadge isSelected Icon.IcnTask (ms $ taskDisplayName st.task) draftBadge (SelectItem st)
