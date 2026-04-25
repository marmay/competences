-- | Shared Miso component for rendering 'GroupedResources'.
--
-- Renders lesson groups as collapsible disclosures, with items
-- annotated by relevance (non-relevant items are dimmed).
-- "Other resources" is also a collapsible disclosure.
module Competences.Frontend.Component.ResourceLookup.View
  ( -- * Component
    groupedResourcesComponent
    -- * Types (re-exported for convenience)
  , GroupedResourcesModel (..)
  , GroupedResourcesAction (..)
  )
where

import Competences.Document
  ( Document
  , Lesson (..)
  , Resource (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Id (Id, idToText)
import Competences.Document.Task (taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.EntityMenu qualified as EM
import Competences.Frontend.Component.Resource.Detailed qualified as ResComp
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText)
import Competences.Frontend.Component.ResourceLookup
  ( AnnotatedLessonGroup (..)
  , GroupedResources (..)
  , ItemRelevance (..)
  , ResolvedItem (..)
  )
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.Component.Task.Detailed qualified as VT
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (DocumentChange (..), PinViewerRequest (..), SyncContext (..), isTeacher, subscribeDocument)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Common.Set qualified as SetUtil
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((&), (.~), (%~))

-- ============================================================================
-- Model & Actions
-- ============================================================================

-- | UI state for the grouped resources component.
data GroupedResourcesModel = GroupedResourcesModel
  { groupedResources :: !GroupedResources
  -- ^ Computed grouped resources (updated via document subscription)
  , resourceState :: !ResComp.ResourceDetailedState
  , expandedLessons :: !(Set (Id Lesson))
  , expandedTasks :: !(Set T.Text)
  , otherCollapsed :: !Bool
  -- ^ Whether the "Other resources" section is collapsed
  }
  deriving (Eq, Generic, Show)

-- | Actions for the grouped resources component.
data GroupedResourcesAction
  = DocChanged !DocumentChange
  | ResourceAction !ResComp.ResourceDetailedAction
  | ToggleLessonExpanded !(Id Lesson)
  | ToggleTaskExpanded !T.Text
  | ToggleOtherSection
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

groupedResourcesComponent
  :: SyncContext
  -> (Document -> GroupedResources)
  -> M.Component p GroupedResourcesModel GroupedResourcesAction
groupedResourcesComponent r project =
  (M.component initModel update view')
    { M.subs = [subscribeDocument r DocChanged]
    }
  where
    initModel :: GroupedResourcesModel
    initModel =
      GroupedResourcesModel
        { groupedResources = GroupedResources [] [] []
        , resourceState = ResComp.initialResourceDetailedState []
        , expandedLessons = Set.empty
        , expandedTasks = Set.empty
        , otherCollapsed = True
        }

    update (DocChanged change) =
      M.modify $ \m -> m & #groupedResources .~ project change.document

    update (ResourceAction a) =
      ResComp.updateResourceDetailed #resourceState a

    update (ToggleLessonExpanded lid) =
      M.modify $ #expandedLessons %~ SetUtil.toggle lid

    update (ToggleTaskExpanded key) =
      M.modify $ #expandedTasks %~ SetUtil.toggle key

    update ToggleOtherSection =
      M.modify $ \m -> m & #otherCollapsed .~ not m.otherCollapsed

    view' :: GroupedResourcesModel -> M.View GroupedResourcesModel GroupedResourcesAction
    view' m
      | null gr.lessonGroups && null gr.ungroupedResources && null gr.ungroupedTasks =
          Typography.muted $ C.translate' C.LblNoResources
      | otherwise =
          MH.div_
            [class_ "space-y-3"]
            ( map (viewLessonGroup r m) gr.lessonGroups
                <> viewOtherSection r m gr
            )
      where
        gr = m.groupedResources

-- ============================================================================
-- Lesson Group
-- ============================================================================

viewLessonGroup
  :: SyncContext
  -> GroupedResourcesModel
  -> AnnotatedLessonGroup
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewLessonGroup r m group =
  let l = group.lesson
      title = if T.null l.title then "(Untitled)" else l.title
      dateLabel = case l.date of
        Just d -> " · " <> C.formatDay d
        Nothing -> ""
      titleView =
        Disclosure.titleIconText Icon.IcnLessonRecord (ms (title <> dateLabel))
      isExpanded = Set.member l.id m.expandedLessons
      bodyView =
        MH.div_
          [class_ "space-y-2"]
          (map (viewAnnotatedItem r m) group.items)
   in Disclosure.innerDisclosure (ToggleLessonExpanded l.id) $
        Disclosure.contents titleView isExpanded bodyView []

-- ============================================================================
-- Other (ungrouped) Section
-- ============================================================================

viewOtherSection
  :: SyncContext
  -> GroupedResourcesModel
  -> GroupedResources
  -> [M.View GroupedResourcesModel GroupedResourcesAction]
viewOtherSection r m gr'
  | null gr'.ungroupedResources && null gr'.ungroupedTasks = []
  | otherwise =
      let isExpanded = not m.otherCollapsed
          titleView = Disclosure.titleText (C.translate' C.LblOtherResources)
          bodyView =
            MH.div_
              [class_ "space-y-2"]
              ( map (viewResourceItem r m Relevant) gr'.ungroupedResources
                  <> map (viewTaskItem r.formulaCache m Relevant) gr'.ungroupedTasks
              )
       in [ Disclosure.innerDisclosure ToggleOtherSection $
              Disclosure.contents titleView isExpanded bodyView []
          ]

-- ============================================================================
-- Annotated Item Rendering
-- ============================================================================

viewAnnotatedItem
  :: SyncContext
  -> GroupedResourcesModel
  -> (ResolvedItem, ItemRelevance)
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewAnnotatedItem r m (item, relevance) =
  case item of
    ResolvedResource res -> viewResourceItem r m relevance res
    ResolvedTask tws -> viewTaskItem r.formulaCache m relevance tws

relevanceBadge :: ItemRelevance -> Maybe (M.View m a)
relevanceBadge Relevant = Just (Badge.primary $ Badge.badgeText $ C.translate' C.LblRelevant)
relevanceBadge ContextOnly = Nothing

-- ============================================================================
-- Resource Item
-- ============================================================================

viewResourceItem
  :: SyncContext
  -> GroupedResourcesModel
  -> ItemRelevance
  -> Resource
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewResourceItem r m relevance res =
  ResComp.renderResource r m.resourceState annotations ResourceAction res
  where
    annotations res' =
      maybe [] (: []) (relevanceBadge relevance)
        <> [ inlineComponent ("entity-menu-" <> ms (show res'.id))
                (EM.entityMenuComponent r EM.EntityMenuConfig
                  { edit = Just (EM.resourceEdit res'.id)
                  , pin = Just (PinResourceViewer res')
                  , goTo = Just (ManageResources (Just res'.id))
                  , delete = Nothing
                  , extraEntries = []
                  })
           | isTeacher r
           ]

-- ============================================================================
-- Task Item
-- ============================================================================

viewTaskItem
  :: FormulaCache
  -> GroupedResourcesModel
  -> ItemRelevance
  -> TaskWithSolutions
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewTaskItem fc m relevance tws =
  let displayName = taskDisplayName tws.task
      key = "task-" <> idToText tws.task.id
      isExpanded = Set.member key m.expandedTasks
      titleBase = Disclosure.titleIconText Icon.IcnTask (ms displayName)
      titleView = case relevanceBadge relevance of
        Nothing -> titleBase
        Just b -> Disclosure.titleWithAnnotation titleBase b
      bodyView = taskBodyView fc tws
      picker = if relevance == Relevant then Disclosure.innerPopDisclosure else Disclosure.innerDisclosure
   in picker (ToggleTaskExpanded key) $
        Disclosure.contents titleView isExpanded bodyView []

taskBodyView :: FormulaCache -> TaskWithSolutions -> M.View model action
taskBodyView fc t =
  MH.div_
    [class_ "space-y-2"]
    ( [ case t.taskContent of
          Nothing -> Layout.empty
          Just rc
            | rc == mempty -> Layout.empty
            | otherwise -> VT.taskContentView (renderRichText fc rc)
      ]
        <> map (viewSolutionContent fc) t.solutions
    )

viewSolutionContent :: FormulaCache -> Solution -> M.View model action
viewSolutionContent fc sol =
  VT.solutionInlineView
    (VT.solutionTypeLabel sol.solutionType)
    ( if sol.content == mempty
        then Layout.empty
        else VT.taskContentView (renderRichText fc sol.content)
    )
