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
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
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
        [ viewHeader m
        , viewSearchField m
        , viewItems m
        ]

    viewSearchField m' =
      Input.defaultInput
        & Input.withValue (ms m'.searchQuery)
        & Input.withPlaceholder (C.translate' C.LblFilterTasks)
        & Input.withOnInput (\v -> SetSearchQuery (M.fromMisoString v))
        & Input.renderInput

    viewHeader m' =
      M.div_
        [class_ "flex items-center justify-between"]
        [ Typography.h3 (C.translate' C.LblTasksAndGroups)
        , viewCreateDropdown m'
        ]

    viewCreateDropdown m' =
      M.div_
        [class_ "relative"]
        [ M.button_
            [ class_ "btn btn-secondary h-8 px-2"
            , M.onClick ToggleDropdown
            ]
            [ icon [class_ "w-4 h-4"] IcnAdd ]
        , if m'.dropdownOpen
            then viewDropdownMenu
            else M.text ""
        ]

    viewDropdownMenu =
      M.div_
        [ class_ "absolute right-0 top-full mt-1 z-50 min-w-48 rounded-md border bg-popover p-1 shadow-md"
        ]
        [ M.button_
            [ class_ "flex w-full items-center gap-2 rounded-sm px-2 py-1.5 text-sm hover:bg-accent hover:text-accent-foreground cursor-pointer"
            , M.onClick CreateNewTask
            ]
            [ icon [class_ "w-4 h-4"] IcnTask
            , M.text (C.translate' C.LblNewTask)
            ]
        , M.button_
            [ class_ "flex w-full items-center gap-2 rounded-sm px-2 py-1.5 text-sm hover:bg-accent hover:text-accent-foreground cursor-pointer"
            , M.onClick CreateNewGroup
            ]
            [ icon [class_ "w-4 h-4"] IcnTaskGroup
            , M.text (C.translate' C.LblNewTaskGroup)
            ]
        ]

    viewItems m =
      let taskItems = map SelectableTask $ Ix.toList m.allTasks
          groupItems = map SelectableGroup $ Ix.toList m.allGroups
          allItems = sortOn itemIdentifier (taskItems <> groupItems)
          query = T.toLower m.searchQuery
          filteredItems = if T.null query
            then allItems
            else filter (\item -> query `T.isInfixOf` T.toLower (itemIdentifier item)) allItems
       in V.viewFlow
            (V.vFlow & (#gap .~ V.SmallSpace) & (#extraAttrs .~ [V.overflowYScroll, V.minH0]))
            (map (viewItem m) filteredItems)

    viewItem m item =
      let isSelected = m.selectedItem == Just item || m.newItem == Just item
          baseClasses = "flex items-center gap-2 px-3 py-2 rounded cursor-pointer transition-colors"
          selectedClass = if isSelected
                          then "bg-primary/10 text-primary"
                          else "hover:bg-muted"
       in M.div_
            [ class_ $ baseClasses <> " " <> selectedClass
            , M.onClick (SelectItem item)
            ]
            [ viewItemIcon item
            , M.span_ [class_ "text-sm truncate"] [M.text $ ms $ itemIdentifier item]
            ]

    viewItemIcon (SelectableTask _) =
      M.span_ [class_ "text-muted-foreground shrink-0"] [icon [class_ "w-4 h-4"] IcnTask]
    viewItemIcon (SelectableGroup _) =
      M.span_ [class_ "text-muted-foreground shrink-0"] [icon [class_ "w-4 h-4"] IcnTaskGroup]
