-- | Per-task resources component for the Assignment Viewer.
--
-- Given a task (by 'TaskId'), discovers related learning materials
-- (resources and other tasks with Complete solutions) based on
-- competence-level overlap, groups them by lesson notes, and renders
-- them inside the task's disclosure body.
module Competences.Frontend.Component.Assignment.TaskResources
  ( taskResourcesComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , LessonNoteItem (..)
  , LessonNotes (..)
  , Resource (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (idToText)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Solution (SolutionType (..))
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskId
  , TaskIdentifier (..)
  , getTaskAttributes
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.LessonNotes.ViewerDetail qualified as LNViewer
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalHeight (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , openFramedModal
  )
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.ResourceList qualified as ResourceList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((&), (.~))

-- ============================================================================
-- Types
-- ============================================================================

-- | A task discovered via competence overlap that has Complete solutions.
data DiscoveredTask = DiscoveredTask
  { taskId :: !TaskId
  , identifier :: !T.Text
  , solutions :: ![Solution]
  }
  deriving (Eq, Generic, Show)

-- | Model for the task resources component.
data TaskResourcesModel = TaskResourcesModel
  { lessonNoteGroups :: ![(LessonNotes, [Resource], [DiscoveredTask])]
  , ungroupedResources :: ![Resource]
  , ungroupedTasks :: ![DiscoveredTask]
  , expandedResources :: !(Set ResourceId)
  , expandedDiscoveredTasks :: !(Set TaskId)
  }
  deriving (Eq, Generic, Show)

-- | Actions for the task resources component.
data Action
  = UpdateResources !DocumentChange
  | ToggleResourceExpanded !ResourceId
  | ToggleDiscoveredTaskExpanded !TaskId
  | OpenLessonNotes !LessonNotes
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create a task resources component that discovers and displays
-- related materials for a given task.
taskResourcesComponent :: SyncContext -> TaskId -> M.Component p TaskResourcesModel Action
taskResourcesComponent r taskId =
  (M.component initModel update view')
    { M.subs = [subscribeDocument r UpdateResources]
    }
  where
    initModel :: TaskResourcesModel
    initModel = TaskResourcesModel
      { lessonNoteGroups = []
      , ungroupedResources = []
      , ungroupedTasks = []
      , expandedResources = Set.empty
      , expandedDiscoveredTasks = Set.empty
      }

    update (UpdateResources docChange) =
      M.modify $ \m ->
        let computed = computeResources taskId docChange.document
         in m & #lessonNoteGroups .~ computed.lessonNoteGroups
              & #ungroupedResources .~ computed.ungroupedResources
              & #ungroupedTasks .~ computed.ungroupedTasks

    update (ToggleResourceExpanded resId) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member resId m.expandedResources
                then Set.delete resId m.expandedResources
                else Set.insert resId m.expandedResources
         in m & #expandedResources .~ newExpanded

    update (ToggleDiscoveredTaskExpanded tid) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member tid m.expandedDiscoveredTasks
                then Set.delete tid m.expandedDiscoveredTasks
                else Set.insert tid m.expandedDiscoveredTasks
         in m & #expandedDiscoveredTasks .~ newExpanded

    update (OpenLessonNotes ln) = M.io_ $
      openFramedModal r.windowManager
        (ModalConfig
          { chrome = WindowChrome (ms ln.title) Icon.IcnLessonNotes
          , width = ModalWide
          , height = ModalFull
          , pinnable = Just ()
          })
        (LNViewer.viewerComponent r ln)

    view' :: TaskResourcesModel -> M.View TaskResourcesModel Action
    view' m
      | null m.lessonNoteGroups && null m.ungroupedResources && null m.ungroupedTasks =
          Typography.muted $ C.translate' C.LblNoResources
      | otherwise =
          MH.div_ [class_ "space-y-3"]
            ( map (viewLessonNoteGroup m) m.lessonNoteGroups
              <> viewUngrouped m
            )

-- ============================================================================
-- Computation
-- ============================================================================

-- | Intermediate result from computeResources.
data ComputedResources = ComputedResources
  { lessonNoteGroups :: ![(LessonNotes, [Resource], [DiscoveredTask])]
  , ungroupedResources :: ![Resource]
  , ungroupedTasks :: ![DiscoveredTask]
  }

-- | Compute related resources and discovered tasks for a given task.
computeResources :: TaskId -> Document -> ComputedResources
computeResources taskId doc =
  case Ix.getOne (doc.tasks Ix.@= taskId) of
    Nothing -> ComputedResources [] [] []
    Just task ->
      let attrs = getTaskAttributes doc.taskGroups task
          compLevels = attrs.primary <> attrs.secondary

          -- Find resources by competence levels
          resources = Ix.toList $ doc.resources Ix.@+ compLevels

          -- Find discovered tasks: other tasks sharing competences with Complete solutions
          discoveredTasks = findDiscoveredTasks taskId compLevels doc

          -- Build reverse index: which lesson notes contain which resources/tasks
          resourceIdSet = Set.fromList (map (.id) resources)
          discoveredTaskIdSet = Set.fromList (map (.taskId) discoveredTasks)
          resourceMap = Map.fromList [(r.id, r) | r <- resources]
          discoveredTaskMap = Map.fromList [(dt.taskId, dt) | dt <- discoveredTasks]

          -- Group by lesson notes (sorted by date descending)
          allLessonNotes = sortOn (Down . (.date)) $ Ix.toList doc.lessonNotes
          (groups, usedResourceIds, usedTaskIds) =
            groupByLessonNotes allLessonNotes resourceIdSet discoveredTaskIdSet resourceMap discoveredTaskMap

          -- Ungrouped items
          ungroupedRes = [r | r <- resources, not (Set.member r.id usedResourceIds)]
          ungroupedTsks = [dt | dt <- discoveredTasks, not (Set.member dt.taskId usedTaskIds)]

       in ComputedResources groups ungroupedRes ungroupedTsks

-- | Find tasks sharing competence levels with Complete solutions.
findDiscoveredTasks :: TaskId -> [CompetenceLevelId] -> Document -> [DiscoveredTask]
findDiscoveredTasks currentTaskId compLevels doc =
  let compLevelSet = Set.fromList compLevels
      allTasks = Ix.toList doc.tasks
   in [ DiscoveredTask
          { taskId = t.id
          , identifier = let TaskIdentifier ident = t.identifier in ident
          , solutions = completeSols
          }
      | t <- allTasks
      , t.id /= currentTaskId
      , let tAttrs = getTaskAttributes doc.taskGroups t
      , let tCompLevels = Set.fromList (tAttrs.primary <> tAttrs.secondary)
      , not (Set.disjoint compLevelSet tCompLevels)
      , let completeSols = Ix.toList $ doc.solutions Ix.@= t.id Ix.@= Complete
      , not (null completeSols)
      ]

-- | Group resources and discovered tasks by lesson notes.
-- Returns (groups, used resource IDs, used task IDs).
groupByLessonNotes
  :: [LessonNotes]
  -> Set ResourceId
  -> Set TaskId
  -> Map.Map ResourceId Resource
  -> Map.Map TaskId DiscoveredTask
  -> ([(LessonNotes, [Resource], [DiscoveredTask])], Set ResourceId, Set TaskId)
groupByLessonNotes lns resourceIdSet discoveredTaskIdSet resourceMap discoveredTaskMap =
  foldr addGroup ([], Set.empty, Set.empty) lns
  where
    addGroup ln (groups, usedRes, usedTasks) =
      let -- Find matching resources and tasks in this lesson note's items
          matchingResources =
            [ r
            | LessonResource rid <- ln.items
            , Set.member rid resourceIdSet
            , Just r <- [Map.lookup rid resourceMap]
            ]
          matchingTasks =
            [ dt
            | LessonTask tid <- ln.items
            , Set.member tid discoveredTaskIdSet
            , Just dt <- [Map.lookup tid discoveredTaskMap]
            ]
       in if null matchingResources && null matchingTasks
            then (groups, usedRes, usedTasks)
            else
              let newUsedRes = foldr (Set.insert . (.id)) usedRes matchingResources
                  newUsedTasks = foldr (Set.insert . (.taskId)) usedTasks matchingTasks
               in ((ln, matchingResources, matchingTasks) : groups, newUsedRes, newUsedTasks)

-- ============================================================================
-- Views
-- ============================================================================

-- | Render a lesson note group with clickable header.
viewLessonNoteGroup
  :: TaskResourcesModel
  -> (LessonNotes, [Resource], [DiscoveredTask])
  -> M.View TaskResourcesModel Action
viewLessonNoteGroup m (ln, resources, discoveredTasks) =
  MH.div_
    [class_ "space-y-2"]
    [ -- Clickable lesson note header
      MH.button_
        [ class_ "flex items-center gap-2 text-sm font-medium text-sky-700 hover:text-sky-900 transition-colors"
        , MH.onClick (OpenLessonNotes ln)
        ]
        [ Icon.icon [] Icon.IcnLessonNotes
        , MH.span_ [] [M.text $ ms ln.title]
        , MH.span_
            [class_ "text-muted-foreground font-normal"]
            [M.text $ C.formatDay ln.date]
        ]
    , -- Resources
      if null resources
        then Layout.empty
        else ResourceList.resourcesListView resources m.expandedResources ToggleResourceExpanded
    , -- Discovered tasks
      if null discoveredTasks
        then Layout.empty
        else MH.div_ [class_ "space-y-1"] (map (viewDiscoveredTask m) discoveredTasks)
    ]

-- | Render ungrouped resources and tasks under a heading.
viewUngrouped :: TaskResourcesModel -> [M.View TaskResourcesModel Action]
viewUngrouped m
  | null m.ungroupedResources && null m.ungroupedTasks = []
  | otherwise =
      [ MH.div_
          [class_ "space-y-2"]
          ( [ Typography.small $ C.translate' C.LblOtherResources
            ]
              <> [ ResourceList.resourcesListView m.ungroupedResources m.expandedResources ToggleResourceExpanded
                 | not (null m.ungroupedResources)
                 ]
              <> [ MH.div_ [class_ "space-y-1"] (map (viewDiscoveredTask m) m.ungroupedTasks)
                 | not (null m.ungroupedTasks)
                 ]
          )
      ]

-- | Render a discovered task as a disclosure with its Complete solutions.
viewDiscoveredTask :: TaskResourcesModel -> DiscoveredTask -> M.View TaskResourcesModel Action
viewDiscoveredTask m dt =
  let isExpanded = Set.member dt.taskId m.expandedDiscoveredTasks
      displayName = if T.null dt.identifier then "(Unbenannt)" else dt.identifier
      titleView = Disclosure.titleIconText Icon.IcnTask (ms displayName)
      bodyView = component ("discovered-task-" <> ms (idToText dt.taskId))
        (discoveredTaskBody dt.solutions)
   in Disclosure.innerDisclosure (ToggleDiscoveredTaskExpanded dt.taskId) $
        Disclosure.contents titleView isExpanded bodyView []

-- | Simple component that renders solution content (avoids rendering when collapsed).
discoveredTaskBody :: [Solution] -> M.Component p [Solution] ()
discoveredTaskBody sols =
  M.component sols (\() -> pure ()) $ \ss ->
    MH.div_
      [class_ "space-y-2"]
      (map viewSolutionContent ss)

-- | Render solution content inline.
viewSolutionContent :: Solution -> M.View model action
viewSolutionContent sol =
  MH.div_
    [class_ "space-y-1"]
    [ Typography.small $ C.translate' (C.LblSolutionType sol.solutionType)
    , if sol.content == mempty
        then Layout.empty
        else
          MH.div_
            [class_ "prose prose-stone prose-sm max-w-none"]
            [renderRichText sol.content]
    ]
