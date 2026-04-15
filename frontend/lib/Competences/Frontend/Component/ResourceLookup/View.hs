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
  , FileRef (..)
  , LessonNotes (..)
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Id (idToText)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Document.Task (taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileGallery (fileGalleryComponent)
import Competences.Frontend.Component.LessonNotes.ViewerDetail (viewLinkCard)
import Competences.Frontend.Component.LessonNotes.ViewerDetail qualified as LNViewer
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText, renderRichTextWithFiles)
import Competences.Frontend.Component.ResourceLookup
  ( AnnotatedLessonNoteGroup (..)
  , GroupedResources (..)
  , ItemRelevance (..)
  , ResolvedItem (..)
  )
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.Fragment.Task.Detailed qualified as VT
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext (..), subscribeDocument)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((&), (.~))

-- ============================================================================
-- Model & Actions
-- ============================================================================

-- | UI state for the grouped resources component.
data GroupedResourcesModel = GroupedResourcesModel
  { groupedResources :: !GroupedResources
  -- ^ Computed grouped resources (updated via document subscription)
  , expandedItems :: !(Set T.Text)
  -- ^ Expanded resource/task IDs (using text keys for uniformity)
  , collapsedLessonNotes :: !(Set LessonNotesId)
  -- ^ Lesson note groups that have been collapsed (default: expanded)
  , otherCollapsed :: !Bool
  -- ^ Whether the "Other resources" section is collapsed
  }
  deriving (Eq, Generic, Show)

-- | Actions for the grouped resources component.
data GroupedResourcesAction
  = DocChanged !DocumentChange
  | ToggleItemExpanded !T.Text
  | ToggleLessonNoteGroup !LessonNotesId
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
        , expandedItems = Set.empty
        , collapsedLessonNotes = Set.empty
        , otherCollapsed = True
        }

    update (DocChanged change) =
      M.modify $ \m -> m & #groupedResources .~ project change.document

    update (ToggleItemExpanded key) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member key m.expandedItems
                then Set.delete key m.expandedItems
                else Set.insert key m.expandedItems
         in m & #expandedItems .~ newExpanded

    update (ToggleLessonNoteGroup lnId) =
      M.modify $ \m ->
        let newCollapsed =
              if Set.member lnId m.collapsedLessonNotes
                then Set.delete lnId m.collapsedLessonNotes
                else Set.insert lnId m.collapsedLessonNotes
         in m & #collapsedLessonNotes .~ newCollapsed

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
      isExpanded = not (Set.member ln.id m.collapsedLessonNotes)
      titleView = Disclosure.titleIconText Icon.IcnLessonNotes (ms ln.title)
      bodyView =
        MH.div_
          [class_ "space-y-2"]
          (map (viewAnnotatedItem r m) group.items)
      openAction = Disclosure.action Icon.IcnOpenModal (OpenLessonNotes ln)
   in Disclosure.innerDisclosure (ToggleLessonNoteGroup ln.id) $
        Disclosure.contents titleView isExpanded bodyView [openAction]

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

-- | Render an annotated item (resource or task) with relevance-based styling.
--
-- Relevant items use a primary-accented (pop) disclosure header.
-- Context-only items use the default muted header with a "Kontext" badge.
viewAnnotatedItem
  :: SyncContext
  -> GroupedResourcesModel
  -> (ResolvedItem, ItemRelevance)
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewAnnotatedItem r m (item, relevance) =
  case item of
    ResolvedResource res -> viewResourceItem r m relevance res
    ResolvedTask tws -> viewTaskItem r.formulaCache m relevance tws

-- | Pick the right inner disclosure variant based on relevance.
relevanceDisclosure :: ItemRelevance -> a -> Disclosure.DisclosureContents m a -> M.View m a
relevanceDisclosure Relevant = Disclosure.innerPopDisclosure
relevanceDisclosure ContextOnly = Disclosure.innerDisclosure

-- | Build a disclosure title with an optional "Passend" badge for relevant items.
relevanceTitle :: ItemRelevance -> M.View m a -> M.View m a
relevanceTitle Relevant title =
  Disclosure.titleWithAnnotation title (Badge.primary $ Badge.badgeText $ C.translate' C.LblRelevant)
relevanceTitle ContextOnly title = title

-- ============================================================================
-- Resource Item
-- ============================================================================

-- | Render a single resource as a disclosure or link card.
viewResourceItem
  :: SyncContext
  -> GroupedResourcesModel
  -> ItemRelevance
  -> Resource
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewResourceItem r m relevance res =
  let ResourceIdentifier ident = res.identifier
      displayName = if T.null ident then "(Unbenannt)" else ident
      key = "res-" <> idToText res.id
   in case res.content of
        InlineContent rc ->
          let isExpanded = Set.member key m.expandedItems
              hasContent = rc /= mempty
              titleBase = Disclosure.titleIconText Icon.IcnResources (ms displayName)
              disclosureTitle = relevanceTitle relevance titleBase
              bodyView =
                MH.div_
                  [class_ "prose prose-stone prose-sm max-w-none"]
                  [renderRichTextWithFiles r.formulaCache r res.attachments rc]
           in if hasContent
                then
                  relevanceDisclosure relevance (ToggleItemExpanded key) $
                    Disclosure.contents disclosureTitle isExpanded bodyView []
                else
                  MH.div_
                    [class_ "rounded overflow-hidden"]
                    [ MH.div_
                        [class_ "px-2 py-1.5"]
                        [ Layout.hFlow
                            (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                            [ Icon.icon [class_ "text-sky-600"] Icon.IcnResources
                            , MH.span_ [class_ "font-medium"] [M.text (ms displayName)]
                            ]
                        ]
                    ]
        WebLink url title -> viewLinkCard Icon.IcnLink ident displayName url title
        VideoLink url title -> viewLinkCard Icon.IcnVideo ident displayName url title
        FileContent fileRef ->
          let isExpanded' = Set.member key m.expandedItems
              titleBase' = Disclosure.titleIconText Icon.IcnResources (ms displayName)
              disclosureTitle' = relevanceTitle relevance titleBase'
              bodyView' =
                inlineComponent
                  ("res-gallery-" <> ms (show fileRef.hash))
                  (fileGalleryComponent r [fileRef])
           in relevanceDisclosure relevance (ToggleItemExpanded key) $
                Disclosure.contents disclosureTitle' isExpanded' bodyView' []

-- ============================================================================
-- Task Item
-- ============================================================================

-- | Render a single task with its solutions as a disclosure.
viewTaskItem
  :: FormulaCache
  -> GroupedResourcesModel
  -> ItemRelevance
  -> TaskWithSolutions
  -> M.View GroupedResourcesModel GroupedResourcesAction
viewTaskItem fc m relevance tws =
  let displayName = taskDisplayName tws.task
      key = "task-" <> idToText tws.task.id
      isExpanded = Set.member key m.expandedItems
      titleBase = Disclosure.titleIconText Icon.IcnTask (ms displayName)
      titleView = relevanceTitle relevance titleBase
      bodyView = taskBodyView fc tws
   in relevanceDisclosure relevance (ToggleItemExpanded key) $
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
