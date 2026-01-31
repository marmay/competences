{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.Resource.Modal
-- Description : Modal component for displaying tasks and learning resources
--
-- Shows tasks and learning resources for a specific competence level.
-- Used via the central ModalManager.
module Competences.Frontend.Component.Resource.Modal
  ( resourceModalComponent
  , ResourceModalConfig (..)
  )
where

import Competences.Document
  ( Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  )
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.TaskResource
  ( DisplayMode (..)
  , TaskResourceList
  , TaskWithSolutions (..)
  , initialState
  , taskResourceListView
  , updateTaskResourceList
  )
import Competences.Frontend.Component.TaskResource qualified as TRL
import Optics.Core ((&))
import Competences.Document.Task (TaskId)
import Competences.Query.TaskStatus (TaskCompletionStatus)
import Competences.Frontend.View.TaskStatus (viewTaskCompletionStatusFromMap)
import Competences.Frontend.SyncContext.ModalManager (ModalManagerRef, closeModal)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon (Icon (IcnLink, IcnResources, IcnVideo), icon)
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Competences.TaskContent.RichContent (toRawText)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration passed when opening the modal
data ResourceModalConfig = ResourceModalConfig
  { tasks :: ![TaskWithSolutions]
  , resources :: ![Resource]
  , showPurposeBadge :: !Bool
  , taskStatuses :: !(Map.Map TaskId TaskCompletionStatus)
  }
  deriving (Eq)

-- ============================================================================
-- Model
-- ============================================================================

-- | View mode for the resource modal
data ResourceViewMode
  = ViewTasks
  | ViewLearningResources
  deriving (Eq, Generic, Show)

-- | Internal model for the component
data Model = Model
  { config :: !ResourceModalConfig
  , taskListState :: !TaskResourceList
  , viewMode :: !ResourceViewMode
  , expandedResources :: !(Set.Set ResourceId)
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = TaskListAction !TRL.Action
  | SwitchViewMode !ResourceViewMode
  | ToggleResourceExpanded !ResourceId
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

resourceModalComponent :: ModalManagerRef -> ResourceModalConfig -> M.Component p Model Action
resourceModalComponent modalMgr cfg =
  M.component model update view
  where
    -- Determine default view mode based on available content
    defaultMode
      | not (null cfg.tasks) = ViewTasks
      | otherwise = ViewLearningResources

    model =
      Model
        { config = cfg
        , taskListState = initialState TasksCollapsed cfg.tasks
        , viewMode = defaultMode
        , expandedResources = Set.empty
        }

    update (TaskListAction action) =
      M.modify $ \m ->
        m {taskListState = updateTaskResourceList action m.taskListState}

    update (SwitchViewMode newMode) =
      M.modify $ \m -> m {viewMode = newMode}

    update (ToggleResourceExpanded resId) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member resId m.expandedResources
                then Set.delete resId m.expandedResources
                else Set.insert resId m.expandedResources
         in m {expandedResources = newExpanded}

    update CloseModal =
      M.io_ $ closeModal modalMgr

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg"
        , class_ "w-[66vw] min-w-[66vw] max-w-none h-[90vh] flex flex-col"
        ]
        [ Modal.modalHeaderWith
            (C.translate' C.LblMaterials)
            [modeSwitcher m.viewMode (not $ null m.config.tasks) (not $ null m.config.resources)]
            CloseModal
        , -- Scrollable content area
          MH.div_
            [class_ "flex-1 overflow-y-auto px-8 py-6"]
            [ case m.viewMode of
                ViewTasks ->
                  taskResourceListView m.config.showPurposeBadge (viewTaskCompletionStatusFromMap m.config.taskStatuses) m.config.tasks m.taskListState TaskListAction
                ViewLearningResources ->
                  resourcesListView m.config.resources m.expandedResources
            ]
        ]

-- ============================================================================
-- View Helpers
-- ============================================================================

-- | Mode switcher using button group
modeSwitcher :: ResourceViewMode -> Bool -> Bool -> M.View Model Action
modeSwitcher currentMode hasTasks hasResources =
  Button.buttonGroup
    [ modeButton ViewTasks (C.translate' C.LblTasks) hasTasks
    , modeButton ViewLearningResources (C.translate' C.LblLearningResources) hasResources
    ]
  where
    modeButton mode label hasContent =
      let variant = if mode == currentMode then Button.Primary else Button.Outline
       in Button.button variant label
            & Button.withSize Button.Small
            & Button.withDisabled (not hasContent)
            & Button.withClick (SwitchViewMode mode)
            & Button.renderButton

-- | View for displaying learning resources
resourcesListView :: [Resource] -> Set.Set ResourceId -> M.View Model Action
resourcesListView resources expandedSet =
  if null resources
    then Typography.muted $ C.translate' C.LblNoResources
    else MH.div_ [class_ "space-y-2"] (map resourceCard resources)
  where
    resourceCard res =
      let ResourceIdentifier ident = res.identifier
          displayName = if T.null ident then "(Unbenannt)" else ident
          nameView =
            MH.div_
              [class_ "flex items-center gap-2"]
              [ icon [class_ "text-sky-600"] IcnResources
              , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
              ]
       in case res.content of
            -- Inline content: expandable card
            InlineContent rc ->
              let isExpanded = Set.member res.id expandedSet
                  hasContent = rc /= mempty
               in if hasContent
                    then
                      Disclosure.collapsible isExpanded (ToggleResourceExpanded res.id) nameView $
                        MH.div_
                          [class_ "prose prose-stone prose-sm max-w-none whitespace-pre-wrap"]
                          [M.text (M.ms (toRawText rc))]
                    else
                      MH.div_
                        [class_ "border rounded-lg overflow-hidden"]
                        [MH.div_ [class_ "flex items-center gap-2 px-3 py-2"] [nameView]]
            -- Web link: direct link card
            WebLink url title ->
              MH.a_
                [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                , MP.href_ (M.ms url)
                , MP.target_ "_blank"
                , MP.rel_ "noopener noreferrer"
                ]
                [ icon [class_ "text-sky-600"] IcnLink
                , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                , if T.null title || title == ident
                    then V.empty
                    else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                ]
            -- Video link: direct link card
            VideoLink url title ->
              MH.a_
                [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                , MP.href_ (M.ms url)
                , MP.target_ "_blank"
                , MP.rel_ "noopener noreferrer"
                ]
                [ icon [class_ "text-sky-600"] IcnVideo
                , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                , if T.null title || title == ident
                    then V.empty
                    else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                ]
