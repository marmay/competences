module Competences.Frontend.Component.Assignment.ViewerDetail
  ( viewerDetailView
  , pinAssignmentViewer
  -- Re-export from Query module for backward compatibility
  , AssignmentStatus (..)
  , assignmentStatus
  , statusLabel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Competence (..)
  , Document (..)
  , Solution (..)
  , User (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Id (idToText)
import Competences.Document.Competence (CompetenceIxs, LevelInfo (..))
import Competences.Document.Evidence (Ability (..), Evidence (..), TaskRemark (..))
import Competences.Document.Task
  ( Task (..)
  , TaskAttributes (..)
  , TaskId
  , TaskIdentifier (..)
  , getTaskAttributes
  , getTaskContent
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.CSS (printStyleView)
import Competences.Frontend.Component.PrintEngine.Measure
  ( PageGroup (..)
  , PageGrouping
  , contentHeightPx
  , groupIntoPages
  , measureTaskHeights
  , nameFieldPx
  , firstPageHeaderPx
  )
import Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalAction (..)
  , PrintModalModel (..)
  , initPrintModalModel
  , measurementContainer
  , needsRemeasure
  , printModalView
  , updatePrintModal
  , renderFirstPageHeader
  , renderCompactHeader
  , renderPageFooter
  , renderNameField
  )
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentSettings (..)
  , PrintSettings (..)
  , TaskContentSetting (..)
  , TaskHeaderStyle (..)
  , TaskLayout (..)
  , cellsPerPage
  , chunksOf
  , defaultPrintSettings
  , expandTaskSequence
  , isTaskVisible
  , mkTaskInfos
  , pageMarginMm
  , pageSizeMm
  , taskContentSetting
  )
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.TaskResource
  ( TaskResourceList
  , TaskWithSolutions (..)
  , DisplayMode (..)
  , initialState
  , taskResourceListView
  , updateTaskResourceList
  )
import Competences.Frontend.Component.TaskResource qualified as TRL
import Competences.Frontend.Component.Assignment.TaskResources qualified as TaskResources
import Competences.Frontend.Component.Submission qualified as Submission
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), WindowMode, inlineComponent, inlineComponentWith, isPinned, pinDialogWith)
import Competences.Frontend.View.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Color (PaletteName (..), textClass')
import Competences.Frontend.View.Color.Ability (abilityPalette)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Color.Completion (CompletionStatus (..))
import Competences.Frontend.View.StatusIcon (completionIcon)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.WindowFrame (pinButton)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.User (UserRole (..))
import Competences.Query.Assignment (AssignmentStatus (..), accumulatedObservations, assignmentStatus)
import Competences.Query.Assignment qualified as Q
import Competences.Query.TaskStatus (TaskCompletionStatus, taskCompletionStatuses)
import Competences.Frontend.View.TaskStatus (viewTaskCompletionStatusFromMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Proxy (Proxy (..))
import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.DSL (jsg, (#))
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))

-- | Trigger browser print dialog.
-- Safe to call after DOM has been patched (e.g., from onCreated sentinel).
triggerPrint :: IO ()
triggerPrint = do
  window <- jsg ("window" :: MisoString)
  _ <- window # ("print" :: MisoString) $ ([] :: [MisoString])
  pure ()

-- ============================================================================
-- Status Helpers (delegate to Query module)
-- ============================================================================

-- | Status label for display (wraps Query module's Text version to MisoString)
statusLabel :: AssignmentStatus -> MisoString
statusLabel = ms . Q.statusLabel

-- | Status icon display: growing icon (yellow) for NeedsWork, checkmark (green) for Completed
statusIcon :: AssignmentStatus -> M.View model a
statusIcon NotGraded = M.text ""  -- No icon for not graded
statusIcon NeedsWork = completionIcon InProgress
statusIcon Completed = completionIcon Done

-- ============================================================================
-- Viewer Projection (pre-computed data)
-- ============================================================================

-- | Pre-computed projection for the viewer
-- All expensive queries are done once per document/user change
data ViewerProjection = ViewerProjection
  { -- | Pre-filtered and sorted tasks with solutions for this assignment
    tasksWithSolutions :: ![TaskWithSolutions]
    -- | Pre-computed: accumulated observations (later assessments override earlier)
  , accumulatedObs :: !(Map CompetenceLevelId Ability)
    -- | Competences for looking up level descriptions
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
    -- | Pre-computed: assignment status for the effective user
  , status :: !AssignmentStatus
    -- | The current assignment (may be updated if edited)
  , currentAssignment :: !Assignment
    -- | Focused user (for header display, can be Nothing for students)
  , focusedUser :: !(Maybe User)
    -- | Connected user role (for conditional display)
  , connectedUserRole :: !UserRole
    -- | Pre-computed: per-task completion status for the effective user
  , taskStatuses :: !(Map TaskId TaskCompletionStatus)
    -- | Tasks that have associated competence levels (primary or secondary)
  , tasksWithCompetences :: !(Set.Set TaskId)
    -- | Per-task qualitative remarks (union across all evidences for this assignment/user)
  , taskRemarkMap :: !(Map TaskId (Set.Set TaskRemark))
  }
  deriving (Eq, Generic, Show)

-- | Empty projection for initial state
emptyProjection :: UserRole -> Assignment -> ViewerProjection
emptyProjection role assignment = ViewerProjection
  { tasksWithSolutions = []
  , accumulatedObs = Map.empty
  , competences = Ix.empty
  , status = NotGraded
  , currentAssignment = assignment
  , focusedUser = Nothing
  , connectedUserRole = role
  , taskStatuses = Map.empty
  , tasksWithCompetences = Set.empty
  , taskRemarkMap = Map.empty
  }

-- ============================================================================
-- Viewer Detail Component
-- ============================================================================

-- | Pin the assignment viewer as a persistent dialog.
pinAssignmentViewer :: SyncContext -> User -> Assignment -> IO ()
pinAssignmentViewer r user assignment =
  let AssignmentName nameText = assignment.name
      chrome = WindowChrome (M.ms nameText) (EvidenceIcon.activityTypeIcon assignment.activityType)
      meta = PinMeta
        { key = "assignment-" <> idToText assignment.id
        , category = PinCatAssignment
        , sortKey = SortKey [SortAtom assignment.assignmentDate, SortAtom assignment.activityType, SortAtom nameText, SortAtom assignment.id]
        , context = Just (C.formatDayShort assignment.assignmentDate)
        }
   in pinDialogWith r.windowManager
        meta
        chrome
        (viewerComponent r user assignment)

-- | Detail view for viewing an assignment (read-only)
-- Shows assignment details and renders task content with MathJax
viewerDetailView
  :: SyncContext
  -> User
  -> Assignment
  -> M.View (SD.Model Assignment mode) (SD.Action mode)
viewerDetailView r user assignment =
  inlineComponentWith
    ("assignment-viewer-" <> M.ms (show assignment.id))
    (viewerComponent r user assignment)

-- | Model with projection and task list state
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  , taskListState :: !TaskResourceList
  , expandedTaskResources :: !(Set.Set TaskId)
  , pagePrintModal :: !(Maybe PrintModalModel)
  , pagePrintPending :: !(Maybe PrintSettings)
  , pagePrintPendingContent :: !(Maybe ContentSettings)
  , pagePrintPageGrouping :: !PageGrouping
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | TaskListAction !TRL.Action
  | PinThis
  | ToggleTaskResourcesExpanded !TaskId
  | OpenPagePrintModal
  | PagePrintMsg !PrintModalAction
  | ClearPagePrint
  deriving (Eq, Show)

-- | The viewer component using subscribeWithProjection pattern
viewerComponent :: SyncContext -> User -> Assignment -> WindowMode -> M.Component p ViewerModel ViewerAction
viewerComponent r user assignment wm =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection assignment user.id user.role) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = emptyProjection user.role assignment
      , taskListState = initialState TasksExpanded Map.empty []
      , expandedTaskResources = Set.empty
      , pagePrintModal = Nothing
      , pagePrintPending = Nothing
      , pagePrintPendingContent = Nothing
      , pagePrintPageGrouping = []
      }

    -- Projection function captures assignment, currentUserId, and role from closure
    viewerProjection :: Assignment -> UserId -> UserRole -> Document -> Maybe User -> ViewerProjection
    viewerProjection asmt currentUserId role doc mUser =
      let -- Determine effective user (focused or fallback to connected)
          effectiveUserId = maybe currentUserId (.id) mUser

          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = maybe asmt id $ Ix.getOne (doc.assignments Ix.@= asmt.id)

          -- Filter tasks for this assignment, sorted by identifier
          relevantTasks = Ix.toAscList (Proxy @TaskIdentifier) $
            doc.tasks Ix.@+ updatedAssignment.tasks

          -- Build TaskWithSolutions for each task
          taskGroups = doc.taskGroups
          tasksWithSolutions =
            [ TaskWithSolutions
                { task = task
                , taskContent = getTaskContent taskGroups task
                , taskPurpose = (getTaskAttributes taskGroups task).purpose
                , solutions = Ix.toList $ doc.solutions Ix.@= task.id
                }
            | task <- relevantTasks
            ]

          -- Get accumulated observations (later assessments override earlier)
          accumulated = accumulatedObservations doc effectiveUserId updatedAssignment.id

          -- Get competence IDs referenced by observations (for level description lookup)
          referencedCompetenceIds = map fst $ Map.keys accumulated
          competences = doc.competences Ix.@+ referencedCompetenceIds

          -- Pre-compute status
          status = assignmentStatus doc effectiveUserId updatedAssignment.id

          -- Pre-compute per-task completion status
          taskStatuses = taskCompletionStatuses doc effectiveUserId relevantTasks

          -- Identify tasks with competence levels (primary or secondary)
          tasksWithCompetences = Set.fromList
            [ task.id
            | task <- relevantTasks
            , let attrs = getTaskAttributes taskGroups task
            , not (null attrs.primary) || not (null attrs.secondary)
            ]

          -- Collect task remarks from all evidences for this assignment/user
          userEvidences = Ix.toList $ doc.evidences Ix.@= effectiveUserId Ix.@= updatedAssignment.id
          taskRemarkMap = Map.unionsWith Set.union
            [ ev.taskRemarks | ev <- userEvidences ]

       in ViewerProjection
            { tasksWithSolutions
            , accumulatedObs = accumulated
            , competences
            , status
            , currentAssignment = updatedAssignment
            , focusedUser = mUser
            , connectedUserRole = role
            , taskStatuses
            , tasksWithCompetences
            , taskRemarkMap
            }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        let newTasks = change.projection.tasksWithSolutions
            -- Re-initialize task list state with new tasks, keeping expanded state
            newTaskListState = initialState TasksExpanded change.projection.taskStatuses newTasks
         in m & #projection .~ change.projection
              & #taskListState .~ newTaskListState

    update (TaskListAction action) =
      M.modify $ \m -> m & #taskListState .~ updateTaskResourceList action m.taskListState

    update (ToggleTaskResourcesExpanded taskId) =
      M.modify $ \m ->
        let newSet =
              if Set.member taskId m.expandedTaskResources
                then Set.delete taskId m.expandedTaskResources
                else Set.insert taskId m.expandedTaskResources
         in m & #expandedTaskResources .~ newSet

    update OpenPagePrintModal = do
      M.modify $ \m ->
        let infos = mkTaskInfos
              [ (tws.task, tws.solutions, tws.taskContent)
              | tws <- m.projection.tasksWithSolutions
              ]
         in m & #pagePrintModal .~ Just (initPrintModalModel infos)
      M.io $ do
        threadDelay 100000 -- 100ms for DOM to render measurement container
        heights <- measureTaskHeights
        let s = defaultPrintSettings
            (firstAvail, restAvail) = decorationAdjustedHeights s
            gap = minGapPx s.baseFontSize
        pure (PagePrintMsg (MeasuredPageGrouping (groupIntoPages firstAvail restAvail gap heights)))

    update (PagePrintMsg CancelPrint) =
      M.modify $ \m -> m & #pagePrintModal .~ Nothing

    update (PagePrintMsg ConfirmPrint) = do
      M.modify $ \m ->
        let settings = maybe defaultPrintSettings (.settings) m.pagePrintModal
            cs = fmap (.contentSettings) m.pagePrintModal
            pg = maybe [] (.pageGrouping) m.pagePrintModal
         in m & #pagePrintModal .~ Nothing
              & #pagePrintPending .~ Just settings
              & #pagePrintPendingContent .~ cs
              & #pagePrintPageGrouping .~ pg
      -- Delay to let MathJax finish rendering formulas in the hidden div,
      -- then trigger the browser print dialog and clean up afterwards.
      M.io $ do
        threadDelay 800000 -- 800ms for MathJax
        triggerPrint
        pure ClearPagePrint

    update (PagePrintMsg action) = do
      M.modify $ \m ->
        let expanded = case m.pagePrintModal of
              Nothing -> m.projection.tasksWithSolutions
              Just mm -> expandedTasks mm.settings mm.contentSettings m.projection
            total = length expanded
         in m & #pagePrintModal .~ fmap (updatePrintModal action total) m.pagePrintModal
      if needsRemeasure action
        then do
          -- Read updated settings from model before spawning IO
          m <- M.get
          let settings = maybe defaultPrintSettings (.settings) m.pagePrintModal
              (firstAvail, restAvail) = decorationAdjustedHeights settings
              gap = minGapPx settings.baseFontSize
          M.io $ do
            threadDelay 100000 -- 100ms for DOM to re-render
            heights <- measureTaskHeights
            pure (PagePrintMsg (MeasuredPageGrouping (groupIntoPages firstAvail restAvail gap heights)))
        else pure ()

    update ClearPagePrint =
      M.modify $ \m -> m & #pagePrintPending .~ Nothing
                          & #pagePrintPendingContent .~ Nothing
                          & #pagePrintPageGrouping .~ []

    update PinThis = M.io_ $ pinAssignmentViewer r user assignment

    view' m =
      M.div_
        []
        [ M.div_
            [class_ "space-y-6 print:hidden"]
            [ viewAssignment m
            ]
        , -- Page-print modal (when open)
          case m.pagePrintModal of
            Nothing -> M.text ""
            Just modalModel ->
              let expanded = expandedTasks modalModel.settings modalModel.contentSettings m.projection
                  expandedCount = length expanded
                  taskNumMap = originalTaskNumbers m.projection.tasksWithSolutions
                  renderFn = renderExpandedTaskForPrint modalModel.settings modalModel.contentSettings taskNumMap expanded
               in M.div_
                    []
                    [ printModalView
                        renderFn
                        expandedCount
                        (assignmentNameToText m.projection.currentAssignment.name)
                        (C.formatDay m.projection.currentAssignment.assignmentDate)
                        modalModel
                        PagePrintMsg
                    , -- Off-screen measurement container for continuous layout
                      case modalModel.settings.taskLayout of
                        Continuous ->
                          measurementContainer
                            renderFn
                            expandedCount
                            modalModel
                        _ -> M.text ""
                    ]
        , viewPagePrintContent m.pagePrintPending m.pagePrintPendingContent
            m.pagePrintPageGrouping m.projection
        ]

    viewAssignment m =
      let proj = m.projection
          desc = proj.currentAssignment.description
          showPurposeBadge = proj.connectedUserRole == Teacher
          taskStatusRenderer taskId =
            M.div_ [class_ "flex items-center gap-1"]
              ( viewTaskRemarkBadges proj.taskRemarkMap taskId
                  <> [viewTaskCompletionStatusFromMap proj.taskStatuses taskId]
              )
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Title line with date + status + print on the right
                  Layout.hFlow (Layout.hFull <> Layout.crossCenter)
                    [ Typography.h2 (assignmentNameToText proj.currentAssignment.name)
                    , Layout.flowSpring
                    , M.div_
                        [class_ "text-sm"]
                        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) $
                            [ M.span_
                                [class_ "text-muted-foreground"]
                                [M.text $ C.formatDay proj.currentAssignment.assignmentDate]
                            , statusIcon proj.status
                            ]
                            <> [ pinButton PinThis | not (isPinned wm) ]
                            <> [ viewPagePrintButton ]
                        ]
                    ]
                , -- Description (if present, supports math syntax)
                  if desc == mempty
                    then M.text ""
                    else M.div_
                           [class_ "prose prose-stone prose-sm max-w-none"]
                           [renderRichText r.formulaCache desc]
                , -- Accumulated observations list (one per competence level)
                  viewObservationList proj
                ]
            , M.div_
              [class_ "space-y-4"]
              ( [ Typography.h3 $ C.translate' C.LblAssignmentTasks | desc /= mempty ] <>
                [ taskResourceListView r.formulaCache showPurposeBadge taskStatusRenderer proj.taskStatuses proj.tasksWithSolutions m.taskListState (viewTaskResources m r) TaskListAction ]
              )
            , -- Students see submission form; teachers don't (teacher view comes in Part 2)
              if proj.connectedUserRole == Student
                then inlineComponentWith ("submission-" <> M.ms (show proj.currentAssignment.id))
                       (Submission.submissionComponent r proj.currentAssignment.id user.id)
                else M.text ""
            ]


    viewObservationList proj =
      if Map.null proj.accumulatedObs
        then M.text ""
        else M.div_
               [class_ "mt-2 space-y-1"]
               (map (viewObservationDetail proj.competences) (Map.toList proj.accumulatedObs))

    viewObservationDetail competences (compLevelId, ability) =
      let (competenceId, level) = compLevelId
          abilityClass = textClass' (abilityPalette ability)
          abilityIcn = abilityIcon ability
          abilityLabel = C.translate' (C.LblAbility ability)
          levelDesc = case Ix.getOne (competences Ix.@= competenceId) of
            Nothing -> ""
            Just comp -> maybe "" (.description) (comp.levels Map.!? level)
       in M.div_
            [class_ "text-sm"]
            [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                [ M.span_
                    [class_ abilityClass]
                    [Icon.icon [MSP.stroke_ "currentColor", class_ "w-4 h-4"] abilityIcn]
                , M.span_
                    [class_ $ abilityClass <> " font-medium"]
                    [M.text abilityLabel]
                , if levelDesc == ""
                    then M.text ""
                    else M.span_
                           [class_ "text-muted-foreground"]
                           [M.text $ "– " <> ms levelDesc]
                ]
            ]

    abilityIcon SelfReliant = Icon.IcnAbilitySelfReliant
    abilityIcon SelfReliantWithSillyMistakes = Icon.IcnAbilitySillyMistakes
    abilityIcon WithSupport = Icon.IcnAbilityWithSupport
    abilityIcon NotYet = Icon.IcnAbilityNotYet

    viewTaskResources :: ViewerModel -> SyncContext -> TaskId -> [M.View ViewerModel ViewerAction]
    viewTaskResources m syncCtx taskId
      | not (Set.member taskId m.projection.tasksWithCompetences) = []
      | otherwise =
          let isExpanded = Set.member taskId m.expandedTaskResources
              titleView = Disclosure.titleIconText Icon.IcnResources (C.translate' C.LblMaterials)
              bodyView = inlineComponent ("task-resources-" <> ms (show taskId))
                           (TaskResources.taskResourcesComponent syncCtx taskId)
           in [Disclosure.innerDisclosure (ToggleTaskResourcesExpanded taskId) $
                 Disclosure.contents titleView isExpanded bodyView []]

    viewTaskRemarkBadges :: Map TaskId (Set.Set TaskRemark) -> TaskId -> [M.View ViewerModel ViewerAction]
    viewTaskRemarkBadges remarkMap taskId =
      case Map.lookup taskId remarkMap of
        Nothing -> []
        Just remarks -> map viewRemarkBadge (Set.toList remarks)

    viewRemarkBadge :: TaskRemark -> M.View ViewerModel ViewerAction
    viewRemarkBadge Exceptional = Badge.badge (PaletteName "ability-success") (Badge.badgeLabel (C.LblTaskRemark Exceptional))
    viewRemarkBadge Sloppy = Badge.badge (PaletteName "ability-warning") (Badge.badgeLabel (C.LblTaskRemark Sloppy))
    viewRemarkBadge Lacking = Badge.badge (PaletteName "ability-warning") (Badge.badgeLabel (C.LblTaskRemark Lacking))

    -- ========================================================================
    -- Page Print
    -- ========================================================================

    viewPagePrintButton :: M.View ViewerModel ViewerAction
    viewPagePrintButton =
      Button.ghostSm (Button.button Icon.IcnPrint OpenPagePrintModal)

    -- | Build the expanded task list from settings, filtering by content visibility
    expandedTasks :: PrintSettings -> ContentSettings -> ViewerProjection -> [TaskWithSolutions]
    expandedTasks settings cs proj =
      let visible = filter (\tws -> isTaskVisible cs tws.task.id) proj.tasksWithSolutions
       in expandTaskSequence settings.groupedCopies settings.totalCopies visible

    -- | Minimum gap between tasks in CSS px, corresponding to 1.5em at the given font size.
    -- 1pt = 96/72 CSS px, so 1.5em at Xpt = 1.5 * X * 96/72.
    minGapPx :: Double -> Double
    minGapPx fontSizePt = 1.5 * fontSizePt * 96.0 / 72.0

    -- | Compute first-page and rest-page available heights.
    -- The compact header and footer live in the page margin area.
    -- The first-page title and name field are in the content area,
    -- so they reduce available height on the first page.
    decorationAdjustedHeights :: PrintSettings -> (Double, Double)
    decorationAdjustedHeights s =
      let baseAvail = contentHeightPx s.paperSize s.orientation
          headerH = if s.showTitle then firstPageHeaderPx s.baseFontSize else 0
          nameH = if s.showNameField then nameFieldPx s.baseFontSize else 0
          firstAvail = baseAvail - headerH - nameH
       in (firstAvail, baseAvail)

    -- | Render a single task from the expanded list by index
    renderExpandedTaskForPrint :: PrintSettings -> ContentSettings -> Map TaskId Int -> [TaskWithSolutions] -> Int -> M.View ViewerModel ViewerAction
    renderExpandedTaskForPrint settings cs taskNumMap expanded idx =
      case drop idx expanded of
        [] -> M.text ""
        (tws : _) -> printTaskView settings.taskHeaderStyle cs (taskNumFor taskNumMap tws) [] tws

    -- | Hidden div with all tasks for page-print.
    -- When mSettings is Just, the @page style is injected (actual print).
    -- When Nothing, an empty placeholder keeps child positions stable
    -- so Miso doesn't recreate richContent components (preserving MathJax state).
    viewPagePrintContent :: Maybe PrintSettings -> Maybe ContentSettings -> PageGrouping -> ViewerProjection -> M.View ViewerModel ViewerAction
    viewPagePrintContent mSettings mCS pageGrp proj =
      let settings = maybe defaultPrintSettings id mSettings
          cs = maybe defaultEmptyContentSettings id mCS
          expanded = expandedTasks settings cs proj
          taskNumMap = originalTaskNumbers proj.tasksWithSolutions
       in M.div_
            []
            [ maybe (M.text "") printStyleView mSettings
            , M.div_
                [class_ "hidden page-print-content"]
                (renderExpandedForPrint settings cs taskNumMap pageGrp expanded)
            ]

    defaultEmptyContentSettings :: ContentSettings
    defaultEmptyContentSettings = ContentSettings { perTask = Map.empty }

    -- | Render expanded tasks for print, choosing continuous or grid layout
    renderExpandedForPrint :: PrintSettings -> ContentSettings -> Map TaskId Int -> PageGrouping -> [TaskWithSolutions] -> [M.View ViewerModel ViewerAction]
    renderExpandedForPrint settings cs taskNumMap pageGrp expanded =
      let style = settings.taskHeaderStyle
       in case settings.taskLayout of
            Continuous
              | not (null pageGrp) ->
                  -- Group tasks into .print-page containers using measured page grouping
                  let totalPages = length pageGrp
                      title = assignmentNameToText assignment.name
                      date = C.formatDay assignment.assignmentDate
                   in zipWith (renderContinuousPage settings cs taskNumMap title date totalPages expanded) [0 ..] pageGrp
              | otherwise ->
                  -- Fallback: each task in a .print-task div, no forced page breaks
                  [ printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-task", MC.style_ [("margin-bottom", "1.5em")]] tws
                  | (_i, tws) <- zip [0 :: Int ..] expanded
                  ]
            Grid gc ->
              -- Group into pages, each page in a .print-page grid div
              let cpp = cellsPerPage gc
                  indexed = zip [0 :: Int ..] expanded
                  pages = chunksOf cpp indexed
                  renderPage indexedTasks =
                    let cells =
                          [ printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-cell"] tws
                          | (_i, tws) <- indexedTasks
                          ]
                          <> replicate (cpp - length indexedTasks) emptyGridCell
                     in M.div_ [class_ "print-page"] cells
               in map renderPage pages

    -- | Render a page of continuous tasks grouped by measurement,
    -- with the computed gap between tasks for even spacing.
    -- Uses 3-section layout: margin-top (header), content-area (name + tasks),
    -- margin-bottom (footer). Header/footer sit in the page margin area.
    renderContinuousPage :: PrintSettings -> ContentSettings -> Map TaskId Int -> MisoString -> MisoString -> Int -> [TaskWithSolutions] -> Int -> PageGroup -> M.View ViewerModel ViewerAction
    renderContinuousPage settings cs taskNumMap title date totalPages expanded pageIdx pg =
      let style = settings.taskHeaderStyle
          isFirst = pageIdx == 0
          (_pw, ph) = pageSizeMm settings.paperSize settings.orientation
          margin = pageMarginMm settings.paperSize
          showMm d = ms (show d <> "mm")
          marginStyle = MC.style_ [("height", showMm margin)]
          pageStyle = MC.style_
            [ ("height", showMm ph)
            , ("padding-left", showMm margin)
            , ("padding-right", showMm margin)
            ]
          marginTopContent
            | isFirst && settings.showTitle = []
            | not settings.showHeader = []
            | otherwise = [renderCompactHeader title date]
          firstPageTitleView
            | settings.showTitle && isFirst = [renderFirstPageHeader title date]
            | otherwise = []
          nameView
            | settings.showNameField && isFirst = [renderNameField]
            | otherwise = []
          marginBottomContent
            | settings.showFooter = [renderPageFooter (pageIdx + 1) totalPages]
            | otherwise = []
       in M.div_
            [class_ "print-page", pageStyle]
            [ -- Top margin area: header at bottom edge
              M.div_ [class_ "print-margin-top", marginStyle] marginTopContent
            , -- Content area: title (first page), name field, tasks
              M.div_
                [class_ "print-content-area"]
                ( firstPageTitleView
                    <> nameView
                    <> [ M.div_
                           [ class_ "flex flex-col"
                           , MC.style_ [("gap", ms (showPx pg.gapPx))]
                           ]
                           [ printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-task"] tws
                           | idx <- pg.indices
                           , Just tws <- [safeIndex expanded idx]
                           ]
                       ]
                )
            , -- Bottom margin area: footer at top edge
              M.div_ [class_ "print-margin-bottom", marginStyle] marginBottomContent
            ]

    safeIndex :: [a] -> Int -> Maybe a
    safeIndex xs i
      | i < 0 = Nothing
      | otherwise = case drop i xs of
          [] -> Nothing
          (x : _) -> Just x

    emptyGridCell :: M.View ViewerModel ViewerAction
    emptyGridCell = M.div_ [class_ "print-cell"] []

    -- | Render a task for print: title h2 + optional description + solutions + grid,
    -- wrapped in a div with the given attributes. Visual styling
    -- (font-size, h2 sizing, margins) comes from the shared
    -- .page-print-content CSS rule.
    printTaskView :: TaskHeaderStyle -> ContentSettings -> Int -> [M.Attribute ViewerAction] -> TaskWithSolutions -> M.View ViewerModel ViewerAction
    printTaskView style cs taskNum attrs tws =
      let TaskIdentifier ident = tws.task.identifier
          tcs = taskContentSetting cs tws.task.id
          prefix = C.translate' C.LblTaskWord
          numText = ms (show taskNum) <> "."
          header = case style of
            HeaderNumber ->
              [M.h2_ [] [M.text (prefix <> numText)]]
            HeaderTitle ->
              [M.h2_ [] [M.text $ ms ident]]
            HeaderBoth ->
              [M.h2_ []
                [ M.strong_ [] [M.text (prefix <> numText)]
                , M.text (" " <> ms ident)
                ]]
          descriptionView
            | tcs.showDescription =
                [ M.div_
                    [ MP.class_ $ "prose prose-stone prose-sm max-w-none"
                        <> printColumnsClass tcs.itemsPerRow
                        <> if tcs.inlineAnswer then " print-inline-answer" else ""
                    ]
                    [renderRichText r.formulaCache content]
                | Just content <- [tws.taskContent]
                ]
            | otherwise = []
          solutionViews =
            [ printSolutionView sol
            | sol <- tws.solutions
            , Set.member sol.id tcs.visibleSolutions
            ]
          gridView = case tcs.gridHeightMm of
            Just h -> [answerGrid h]
            Nothing -> []
       in M.div_
            attrs
            (header <> descriptionView <> solutionViews <> gridView)

    -- | Render a solution for print: type label (h2-sized) + rich text content
    printSolutionView :: Solution -> M.View ViewerModel ViewerAction
    printSolutionView sol =
      M.div_
        [class_ "mt-2"]
        [ M.h2_ [] [M.text $ C.translate' (C.LblSolutionType sol.solutionType)]
        , M.div_
            [class_ "prose prose-stone prose-sm max-w-none"]
            [renderRichText r.formulaCache sol.content]
        ]

    -- | Render an answer grid with 5mm squares (no outer border)
    answerGrid :: Double -> M.View model action
    answerGrid heightMm =
      M.div_
        [ MC.style_
            [ ("width", "100%")
            , ("height", ms (show heightMm) <> "mm")
            , ("background-image", "linear-gradient(to right, #ccc 1px, transparent 1px), linear-gradient(to bottom, #ccc 1px, transparent 1px)")
            , ("background-size", "5mm 5mm")
            , ("margin-top", "0.5em")
            , ("print-color-adjust", "exact")
            , ("-webkit-print-color-adjust", "exact")
            ]
        ]
        []

    -- | CSS class for multi-column letter lists
    printColumnsClass :: Int -> MisoString
    printColumnsClass 1 = ""
    printColumnsClass n = " print-columns-" <> ms (show (min 4 n))

    -- | Build a map from TaskId to its 1-based position in the original
    -- (unfiltered) task list, so hidden tasks don't renumber visible ones.
    originalTaskNumbers :: [TaskWithSolutions] -> Map TaskId Int
    originalTaskNumbers twss = Map.fromList
      [(tws.task.id, i) | (i, tws) <- zip [1 ..] twss]

    -- | Look up the original task number from the map
    taskNumFor :: Map TaskId Int -> TaskWithSolutions -> Int
    taskNumFor numMap tws = Map.findWithDefault 0 tws.task.id numMap

    showPx :: Double -> MisoString
    showPx d = ms (show (round d :: Int)) <> "px"

    assignmentNameToText (AssignmentName t) = ms t
