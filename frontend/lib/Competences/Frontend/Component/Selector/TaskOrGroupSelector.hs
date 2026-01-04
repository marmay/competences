module Competences.Frontend.Component.Selector.TaskOrGroupSelector
  ( TaskOrGroup (..)
  , taskOrGroupSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskGroup (..), TaskGroupIxs, TaskIxs, TaskType (..))
import Competences.Document.Task (TaskGroupIdentifier (..), TaskIdentifier (..), defaultTaskAttributes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | Sum type for items that can be selected in the task/group selector
data TaskOrGroup
  = SelectableTask !Task
  | SelectableGroup !TaskGroup
  deriving (Eq, Show)

-- | Get the identifier text for sorting
itemIdentifier :: TaskOrGroup -> Text
itemIdentifier (SelectableTask t) =
  let TaskIdentifier ident = t.identifier in ident
itemIdentifier (SelectableGroup g) =
  let TaskGroupIdentifier ident = g.identifier in ident

data Model = Model
  { allTasks :: !(Ix.IxSet TaskIxs Task)
  , allGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , selectedItem :: !(Maybe TaskOrGroup)
  , newItem :: !(Maybe TaskOrGroup)
  , dropdownOpen :: !Bool
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !TaskOrGroup
  | CreateNewTask
  | CreateNewGroup
  | ToggleDropdown
  | CloseDropdown
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

taskOrGroupSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe TaskOrGroup) -> M.Component p Model Action
taskOrGroupSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Ix.empty Nothing Nothing False ""

    update (SelectItem item) = M.modify $ \m ->
      case item of
        SelectableTask t -> case Ix.getOne (m.allTasks Ix.@= t.id) of
          Just t' -> m & (#selectedItem ?~ SelectableTask t') & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)
        SelectableGroup g -> case Ix.getOne (m.allGroups Ix.@= g.id) of
          Just g' -> m & (#selectedItem ?~ SelectableGroup g') & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)

    update CreateNewTask = M.withSink $ \s -> do
      taskId <- nextId r
      let newTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SelfContained defaultTaskAttributes
            }
      modifySyncDocument r $ Tasks (OnTasks (Create newTask))
      s CloseDropdown
      s (SelectItem $ SelectableTask newTask)

    update CreateNewGroup = M.withSink $ \s -> do
      groupId <- nextId r
      let newGroup = TaskGroup
            { id = groupId
            , identifier = TaskGroupIdentifier ""
            , defaultTaskAttributes = defaultTaskAttributes
            , contentBefore = Nothing
            , contentAfter = Nothing
            }
      modifySyncDocument r $ Tasks (OnTaskGroups (Create newGroup))
      s CloseDropdown
      s (SelectItem $ SelectableGroup newGroup)

    update ToggleDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ not m.dropdownOpen

    update CloseDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ False

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      let selfContainedTasks = Ix.fromList $ filter isSelfContained $ Ix.toList dc.document.tasks
          allGroups' = dc.document.taskGroups
          -- Validate selected item still exists
          validatedSelected = case m.selectedItem of
            Just (SelectableTask t) ->
              SelectableTask <$> Ix.getOne (selfContainedTasks Ix.@= t.id)
            Just (SelectableGroup g) ->
              SelectableGroup <$> Ix.getOne (allGroups' Ix.@= g.id)
            Nothing -> Nothing
          -- Check if new item now exists in document
          validatedNew = case m.newItem of
            Just (SelectableTask t) ->
              case Ix.getOne (selfContainedTasks Ix.@= t.id) of
                Just t' -> Just (SelectableTask t')
                Nothing -> m.newItem -- Keep as new
            Just (SelectableGroup g) ->
              case Ix.getOne (allGroups' Ix.@= g.id) of
                Just g' -> Just (SelectableGroup g')
                Nothing -> m.newItem -- Keep as new
            Nothing -> Nothing
       in m
            { allTasks = selfContainedTasks
            , allGroups = allGroups'
            , selectedItem = validatedSelected
            , newItem = validatedNew
            }

    isSelfContained :: Task -> Bool
    isSelfContained task = case task.taskType of
      SelfContained _ -> True
      SubTask _ _ -> False

    view' m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeaderWithDropdown
            (C.translate' C.LblTasksAndGroups)
            m.dropdownOpen
            ToggleDropdown
            [ SL.dropdownItem IcnTask (C.translate' C.LblNewTask) CreateNewTask
            , SL.dropdownItem IcnTaskGroup (C.translate' C.LblNewTaskGroup) CreateNewGroup
            ]
        , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterTasks) (SetSearchQuery . M.fromMisoString)
        , viewItems m
        ]

    viewItems m =
      let taskItems = map SelectableTask $ Ix.toList m.allTasks
          groupItems = map SelectableGroup $ Ix.toList m.allGroups
          allItems = sortOn itemIdentifier (taskItems <> groupItems)
          query = T.toLower m.searchQuery
          filteredItems =
            if T.null query
              then allItems
              else filter (\item -> query `T.isInfixOf` T.toLower (itemIdentifier item)) allItems
       in SL.selectorList (map (viewItem m) filteredItems)

    viewItem m item =
      let isSelected = m.selectedItem == Just item || m.newItem == Just item
          (icn, label) = case item of
            SelectableTask _ -> (IcnTask, ms $ itemIdentifier item)
            SelectableGroup _ -> (IcnTaskGroup, ms $ itemIdentifier item)
       in SL.selectorItem isSelected icn label (SelectItem item)
