-- | Shared Miso component for rendering 'GroupedResources'.
--
-- Renders lesson-note groups as collapsible disclosures, with items
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
  , LessonNotes (..)
  , Resource (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Id (idToText)
import Competences.Document.Task (taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.LessonNotes.Detailed.Embed qualified as LNEmbed
import Competences.Frontend.Component.LessonNotes.EditButton (lessonNotesEditButton)
import Competences.Frontend.Component.LessonNotes.ViewerDetail qualified as LNViewer
import Competences.Frontend.Component.Resource.Detailed.Embed qualified as ResEmbed
import Competences.Frontend.Component.Resource.EditButton (resourceEditButton)
import Competences.Frontend.Fragment.LessonNotes.Detailed qualified as VLN
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText)
import Competences.Frontend.Component.ResourceLookup
  ( AnnotatedLessonNoteGroup (..)
  , GroupedResources (..)
  , ItemRelevance (..)
  , ResolvedItem (..)
  )
import Competences.Frontend.Fragment.Resource.Detailed qualified as VR
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.Fragment.Task.Detailed qualified as VT
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext (..), isTeacher, subscribeDocument)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
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
  , resourceState :: !VR.ResourceDetailedState
  , lessonNotesState :: !VLN.LessonNotesDetailedState
  , expandedTasks :: !(Set T.Text)
  , otherCollapsed :: !Bool
  -- ^ Whether the "Other resources" section is collapsed
  }
  deriving (Eq, Generic, Show)

-- | Actions for the grouped resources component.
data GroupedResourcesAction
  = DocChanged !DocumentChange
  | ResourceAction !VR.ResourceDetailedAction
  | LessonNotesAction !VLN.LessonNotesDetailedAction
  | ToggleTaskExpanded !T.Text
  | ToggleOtherSection
  | OpenLessonNotes !LessonNotes
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create a grouped resources component.
--
-- Takes a projection function @(Document -> GroupedResources)@ so the
-- component can recompute its data whenever the document changes.
-- Renders lesson-note groups with collapsible disclosures.
-- Non-relevant items are dimmed with reduced opacity.
-- "Other resources" is wrapped in its own disclosure.
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
        , resourceState = VR.initialResourceDetailedState []
        , lessonNotesState = VLN.initialLessonNotesDetailedState []
        , expandedTasks = Set.empty
        , otherCollapsed = True
        }

    update (DocChanged change) =
      M.modify $ \m -> m & #groupedResources .~ project change.document

    update (ResourceAction a) =
      ResEmbed.updateResourceDetailed #resourceState r ResourceAction a

    update (LessonNotesAction a) =
      LNEmbed.updateLessonNotesDetailed #lessonNotesState r LessonNotesAction a

    update (ToggleTaskExpanded key) =
      M.modify $ #expandedTasks %~ SetUtil.toggle key

    update ToggleOtherSection =
      M.modify $ \m -> m & #otherCollapsed .~ not m.otherCollapsed

    update (OpenLessonNotes ln) =
      M.io_ $ LNViewer.openLessonNotesModal r ln

    view' :: GroupedResourcesModel -> M.View GroupedResourcesModel GroupedResourcesAction
    view' m
      | null gr.lessonNoteGroups && null gr.ungroupedResources && null gr.ungroupedTasks =
          Typography.muted $ C.translate' C.LblNoResources
      | otherwise =
          MH.div_
            [class_ "space-y-3"]
            ( map (viewLessonNoteGroup r m) gr.lessonNoteGroups
                <> viewOtherSection r m gr
            )
      where
        gr = m.groupedResources

-- ============================================================================
-- Lesson Note Group
-- ============================================================================

-- | Render a lesson note group as a collapsible disclosure.
viewLessonNoteGroup
  :: SyncContext
  -> GroupedResourcesModel
  -> AnnotatedLessonNoteGroup
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewLessonNoteGroup r m group =
  let ln = group.lessonNotes
      bodyView =
        MH.div_
          [class_ "space-y-2"]
          (map (viewAnnotatedItem r m) group.items)
      annotations =
        [ Button.ghostSm (Button.ButtonConfig (Button.IconOnly Icon.IcnOpenModal) (Just (OpenLessonNotes ln)))
        ]
          <> [lessonNotesEditButton r ln | isTeacher r]
   in LNEmbed.renderLessonNotesGroup m.lessonNotesState annotations bodyView LessonNotesAction ln

-- ============================================================================
-- Other (ungrouped) Section
-- ============================================================================

-- | Render ungrouped resources and tasks in a collapsible "Other" disclosure.
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

-- | Render an annotated item (resource or task).
viewAnnotatedItem
  :: SyncContext
  -> GroupedResourcesModel
  -> (ResolvedItem, ItemRelevance)
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewAnnotatedItem r m (item, relevance) =
  case item of
    ResolvedResource res -> viewResourceItem r m relevance res
    ResolvedTask tws -> viewTaskItem r.formulaCache m relevance tws

-- | Relevance badge shown inline with the entity header.
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
  ResEmbed.renderResource r m.resourceState annotations ResourceAction res
  where
    annotations res' =
      maybe [] (: []) (relevanceBadge relevance)
        <> [resourceEditButton r res' | isTeacher r]

-- ============================================================================
-- Task Item
-- ============================================================================

-- | Render a single task with its solutions as a disclosure (ad-hoc — task
-- items in this view are still keyed by text; full Fragment migration is
-- deferred).
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

-- | Render task body content (task content + solutions).
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

-- | Render solution content inline.
viewSolutionContent :: FormulaCache -> Solution -> M.View model action
viewSolutionContent fc sol =
  VT.solutionInlineView
    (VT.solutionTypeLabel sol.solutionType)
    ( if sol.content == mempty
        then Layout.empty
        else VT.taskContentView (renderRichText fc sol.content)
    )
