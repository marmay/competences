module Competences.Frontend.Component.Selector.TaskOrGroupSelector
  ( TaskOrGroup (..)
  , taskOrGroupSelectorComponent
  , taskOrGroupOrigin
  , EntityOrigin (..)
  )
where

import Competences.Command (Command (..), DraftTasksCommand (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskGroup (..), TaskGroupIxs, TaskIxs, TaskType (..))
import Competences.Document.Task (TaskGroupId, TaskGroupIdentifier (..), TaskId, TaskIdentifier (..), defaultTaskAttributes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Data.List (sortOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | Sum type for items that can be selected in the task/group selector
data TaskOrGroup
  = SelectableTask !EntityOrigin !Task
  | SelectableGroup !EntityOrigin !TaskGroup
  deriving (Eq, Show)

-- | Get the identifier text for sorting
itemIdentifier :: TaskOrGroup -> Text
itemIdentifier (SelectableTask _ t) =
  let TaskIdentifier ident = t.identifier in ident
itemIdentifier (SelectableGroup _ g) =
  let TaskGroupIdentifier ident = g.identifier in ident

-- | Get the origin of a TaskOrGroup item
taskOrGroupOrigin :: TaskOrGroup -> EntityOrigin
taskOrGroupOrigin (SelectableTask origin _) = origin
taskOrGroupOrigin (SelectableGroup origin _) = origin

data Model = Model
  { allTasks :: !(Ix.IxSet TaskIxs Task)
  , allGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , draftTaskIds :: !(Set TaskId)
  , draftGroupIds :: !(Set TaskGroupId)
  , selectedItem :: !(Maybe TaskOrGroup)
  , newItem :: !(Maybe TaskOrGroup)
  , dropdownOpen :: !Bool
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !TaskOrGroup
  | CreateNewTask
  | CreateNewDraftTask
  | CreateNewGroup
  | CreateNewDraftGroup
  | ToggleDropdown
  | CloseDropdown
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

taskOrGroupSelectorComponent
  :: SyncContext -> Lens' p (Maybe TaskOrGroup) -> M.Component p Model Action
taskOrGroupSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Ix.empty Set.empty Set.empty Nothing Nothing False ""

    update (SelectItem item) = M.modify $ \m ->
      case item of
        SelectableTask origin t -> case Ix.getOne (m.allTasks Ix.@= t.id) of
          Just t' -> m & (#selectedItem ?~ SelectableTask origin t') & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)
        SelectableGroup origin g -> case Ix.getOne (m.allGroups Ix.@= g.id) of
          Just g' -> m & (#selectedItem ?~ SelectableGroup origin g') & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)

    update CreateNewTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SelfContained defaultTaskAttributes
            , attachments = []
            }
      modifySyncDocument r $ Tasks (OnTasks (CreateAndLock newTask))
      s CloseDropdown
      s (SelectItem $ SelectableTask Published newTask)

    update CreateNewDraftTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SelfContained defaultTaskAttributes
            , attachments = []
            }
      modifySyncDocument r $ DraftTasks (OnDraftTasks (CreateAndLock newTask))
      s CloseDropdown
      s (SelectItem $ SelectableTask Draft newTask)

    update CreateNewGroup = M.withSink $ \s -> do
      groupId <- nextId r
      let newGroup = TaskGroup
            { id = groupId
            , identifier = TaskGroupIdentifier ""
            , defaultTaskAttributes = defaultTaskAttributes
            , contentBefore = Nothing
            , contentAfter = Nothing
            }
      modifySyncDocument r $ Tasks (OnTaskGroups (CreateAndLock newGroup))
      s CloseDropdown
      s (SelectItem $ SelectableGroup Published newGroup)

    update CreateNewDraftGroup = M.withSink $ \s -> do
      groupId <- nextId r
      let newGroup = TaskGroup
            { id = groupId
            , identifier = TaskGroupIdentifier ""
            , defaultTaskAttributes = defaultTaskAttributes
            , contentBefore = Nothing
            , contentAfter = Nothing
            }
      modifySyncDocument r $ DraftTasks (OnDraftTaskGroups (CreateAndLock newGroup))
      s CloseDropdown
      s (SelectItem $ SelectableGroup Draft newGroup)

    update ToggleDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ not m.dropdownOpen

    update CloseDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ False

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          -- Merge real + draft tasks (both filtered to self-contained only)
          realTasks = filter isSelfContained $ Ix.toList doc.tasks
          draftTasks = filter isSelfContained $ Ix.toList doc.draftTasks
          mergedTasks = Ix.fromList (realTasks <> draftTasks)
          -- Merge real + draft groups
          mergedGroups = Ix.fromList (Ix.toList doc.taskGroups <> Ix.toList doc.draftTaskGroups)
          -- Track draft IDs
          draftTaskIds' = Set.fromList $ map (.id) draftTasks
          draftGroupIds' = Set.fromList $ map (.id) $ Ix.toList doc.draftTaskGroups
          -- Determine origin for an entity
          taskOrigin tid = if Set.member tid draftTaskIds' then Draft else Published
          groupOrigin gid = if Set.member gid draftGroupIds' then Draft else Published
          -- Validate selected item still exists
          validatedSelected = case m.selectedItem of
            Just (SelectableTask _ t) ->
              (\t' -> SelectableTask (taskOrigin t'.id) t') <$> Ix.getOne (mergedTasks Ix.@= t.id)
            Just (SelectableGroup _ g) ->
              (\g' -> SelectableGroup (groupOrigin g'.id) g') <$> Ix.getOne (mergedGroups Ix.@= g.id)
            Nothing -> Nothing
          -- Check if new item now exists in document
          validatedNew = case m.newItem of
            Just (SelectableTask _ t) ->
              case Ix.getOne (mergedTasks Ix.@= t.id) of
                Just t' -> Just (SelectableTask (taskOrigin t'.id) t')
                Nothing -> m.newItem
            Just (SelectableGroup _ g) ->
              case Ix.getOne (mergedGroups Ix.@= g.id) of
                Just g' -> Just (SelectableGroup (groupOrigin g'.id) g')
                Nothing -> m.newItem
            Nothing -> Nothing
       in m
            { allTasks = mergedTasks
            , allGroups = mergedGroups
            , draftTaskIds = draftTaskIds'
            , draftGroupIds = draftGroupIds'
            , selectedItem = validatedSelected
            , newItem = validatedNew
            }

    isSelfContained :: Task -> Bool
    isSelfContained task = case task.taskType of
      SelfContained _ -> True
      SubTask _ _ -> False

    view' m =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ SL.selectorHeaderWithDropdown
                (C.translate' C.LblTasksAndGroups)
                m.dropdownOpen
                ToggleDropdown
                [ SL.dropdownItem Icon.IcnTask (C.translate' C.LblNewTask) CreateNewTask
                , SL.dropdownItem Icon.IcnTaskGroup (C.translate' C.LblNewTaskGroup) CreateNewGroup
                , SL.dropdownItem Icon.IcnTask (C.translate' C.LblNewDraftTask) CreateNewDraftTask
                , SL.dropdownItem Icon.IcnTaskGroup (C.translate' C.LblNewDraftTaskGroup) CreateNewDraftGroup
                ]
            , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterTasks) (SetSearchQuery . M.fromMisoString)
            , viewItems m
            ]
        ]

    viewItems m =
      let taskItems = map (\t -> SelectableTask (if Set.member t.id m.draftTaskIds then Draft else Published) t) $ Ix.toList m.allTasks
          groupItems = map (\g -> SelectableGroup (if Set.member g.id m.draftGroupIds then Draft else Published) g) $ Ix.toList m.allGroups
          allItems = sortOn itemIdentifier (taskItems <> groupItems)
          query = T.toLower m.searchQuery
          filteredItems =
            if T.null query
              then allItems
              else filter (\item -> query `T.isInfixOf` T.toLower (itemIdentifier item)) allItems
       in SL.selectorList (map (viewItem m) filteredItems)

    viewItem m item =
      let isSelected = m.selectedItem == Just item || m.newItem == Just item
          origin = taskOrGroupOrigin item
          (icn, label) = case item of
            SelectableTask _ _ -> (Icon.IcnTask, ms $ itemIdentifier item)
            SelectableGroup _ _ -> (Icon.IcnTaskGroup, ms $ itemIdentifier item)
          draftBadge = case origin of
            Draft -> Just $ Badge.secondary (Badge.badgeText (C.translate' C.LblDraft))
            Published -> Nothing
       in SL.selectorItemWithBadge isSelected icn label draftBadge (SelectItem item)
