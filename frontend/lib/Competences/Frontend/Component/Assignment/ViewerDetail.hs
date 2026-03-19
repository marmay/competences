module Competences.Frontend.Component.Assignment.ViewerDetail
  ( viewerDetailView
  , pinAssignmentViewer
  -- Re-export from Query module for backward compatibility
  , AssignmentStatus (..)
  , assignmentStatus
  , statusLabel
  )
where

import Control.Monad (when)
import Competences.Query.Task (getTaskOrDraft)
import Data.Default (def)
import Data.Maybe (isJust, mapMaybe)
import Competences.Command (Command (..))
import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..))
import Competences.Command.Common (EntityCommand (..))
import Competences.Command.Common qualified as Cmd (ModifyCommand (..))
import Competences.Command.Layouts (LayoutsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Competence (..)
  , ContentPreset (..)
  , Document (..)
  , Layout (..)
  , Solution (..)
  , User (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Id (Id (..), idToText)
import Competences.Document.Competence (CompetenceIxs, LevelInfo (..))
import Competences.Document.Evidence (Ability (..), Evidence (..), TaskRemark (..)
  )
import Competences.Document.Layout (LayoutId)
import Competences.Document.Task
  ( Task (..)
  , TaskAttributes (..)
  , TaskId
  , getTaskAttributes
  , getTaskContent
  , taskDisplayName
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.PrintEngine.CSS (printStyleView)
import Competences.Frontend.Component.PrintEngine.Footer qualified as Footer
import Competences.Frontend.Component.PrintEngine.Measure
  ( PageGrouping
  , adjustForFooter
  , contentHeightPx
  , groupIntoPages
  , measureFooterHeight
  , measureTaskHeights
  , nameFieldPx
  , firstPageHeaderPx
  )
import Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalAction (..)
  , PrintModalModel (..)
  , footerMeasureContainer
  , initPrintModalModel
  , initFromLayout
  , measurementContainer
  , needsRemeasure
  , printModalView
  , reorderedTaskIds
  , updatePrintModal
  )
import Competences.Frontend.Component.PrintEngine.Page qualified as Page
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentSettings (..)
  , PrintSettings (..)
  , TaskContentSetting (..)
  , TaskHeaderStyle (..)
  , TaskLayout (..)
  , cellsPerPage
  , chunksOf
  , defaultContentSettings
  , defaultPrintSettings
  , expandTaskSequence
  , isTaskVisible
  , TaskInfo (..)
  , mkTaskInfos
  , taskContentSetting
  )
import Competences.Frontend.Component.RenumberModal (RenumberTaskInfo (..), openRenumberModal)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
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
  , modifySyncDocument
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), WindowMode, inlineComponent, inlineComponentWith, isPinned, pinDialogWith)
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
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
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.DSL (jsg, (#))
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~), (%~))
import System.Random (randomIO)


-- | Trigger browser print dialog.
-- Safe to call after DOM has been patched (e.g., from onCreated sentinel).
triggerPrint :: IO ()
triggerPrint = do
  window <- jsg ("window" :: MisoString)
  _ <- window # ("print" :: MisoString) $ ([] :: [MisoString])
  pure ()

-- | Save a layout from the current modal state.
-- Deletes the old layout and creates a new one with updated settings.
saveLayoutFromModal :: SyncContext -> PrintModalModel -> IO ()
saveLayoutFromModal syncCtx mm = do
  modifySyncDocument syncCtx (Layouts (OnLayouts (Delete mm.layoutId)))
  let layout = Layout
        { id = mm.layoutId
        , assignmentId = mm.layoutAssignmentId
        , preset = mm.selectedPreset
        , printSettings = mm.settings
        , contentSettings = mm.contentSettings
        , createdAt = mm.layoutCreatedAt
        }
  modifySyncDocument syncCtx (Layouts (OnLayouts (Create layout)))

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
    -- | Pre-computed submission summary for the effective user (students only)
  , submissionSummary :: !Submission.SubmissionSummary
    -- | Saved print layouts for this assignment
  , assignmentLayouts :: ![Layout]
    -- | Whether this assignment is a draft or published
  , origin :: !EntityOrigin
    -- | Set of task IDs that live in the draft collection (for per-task command wrapping)
  , draftTaskIds :: !(Set.Set TaskId)
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
  , submissionSummary = Submission.NoSubmissions
  , assignmentLayouts = []
  , origin = Published
  , draftTaskIds = Set.empty
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
  , layoutHoldState :: !(HoldButton.HoldState LayoutId)
  , footerDraftGen :: !Int
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | TaskListAction !TRL.Action
  | PinThis
  | ToggleTaskResourcesExpanded !TaskId
  | OpenPagePrintModal !(Maybe LayoutId)
  | OpenNewLayoutModal !Layout
  | PagePrintMsg !PrintModalAction
  | ClearPagePrint
  | OpenSubmissionModal
  | LayoutHoldAction !(HoldButton.HoldAction LayoutId)
  | DebouncedRemeasure !Int
  | NoOp
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
      , layoutHoldState = HoldButton.emptyHoldState
      , footerDraftGen = 0
      }

    -- Projection function captures assignment, currentUserId, and role from closure
    viewerProjection :: Assignment -> UserId -> UserRole -> Document -> Maybe User -> ViewerProjection
    viewerProjection asmt currentUserId role doc mUser =
      let -- Determine effective user (focused or fallback to connected)
          effectiveUserId = maybe currentUserId (.id) mUser

          -- Look up the current assignment from the document (in case it was edited)
          -- Also determine origin from which collection it was found in
          (updatedAssignment, asmtOrigin) =
            case Ix.getOne (doc.assignments Ix.@= asmt.id) of
              Just published -> (published, Published)
              Nothing -> case Ix.getOne (doc.draftAssignments Ix.@= asmt.id) of
                Just draft -> (draft, Draft)
                Nothing -> (asmt, Published)

          -- Look up tasks preserving assignment list order
          relevantTasks = mapMaybe (getTaskOrDraft doc) updatedAssignment.tasks

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

          -- Compute submission summary for this assignment+user
          userSubmissions = Ix.toList $ doc.submissions Ix.@= updatedAssignment.id Ix.@= effectiveUserId
          subSummary = Submission.submissionSummary userSubmissions

          -- Get saved layouts for this assignment
          layouts = Ix.toList $ doc.layouts Ix.@= updatedAssignment.id

          -- Compute which tasks are in draft collection
          draftTids = Set.fromList
            [ t.id | t <- relevantTasks, isJust (Ix.getOne (doc.draftTasks Ix.@= t.id)) ]

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
            , submissionSummary = subSummary
            , assignmentLayouts = layouts
            , origin = asmtOrigin
            , draftTaskIds = draftTids
            }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        let newTasks = change.projection.tasksWithSolutions
            -- Re-initialize task list state with new tasks, keeping expanded state
            newTaskListState = initialState TasksExpanded change.projection.taskStatuses newTasks
            -- Build fresh TaskInfos map for updating modal order
            freshMap = Map.fromList
              [(ti.taskId, ti) | ti <- taskInfosFromTws newTasks]
            -- Update modal taskInfos preserving user's custom order
            updateModal modal = modal
              { taskInfos = mapMaybe (\ti -> Map.lookup ti.taskId freshMap) modal.taskInfos
              }
         in m & #projection .~ change.projection
              & #taskListState .~ newTaskListState
              & #pagePrintModal %~ fmap updateModal

    update (TaskListAction action) =
      M.modify $ \m -> m & #taskListState .~ updateTaskResourceList action m.taskListState

    update (ToggleTaskResourcesExpanded taskId) =
      M.modify $ \m ->
        let newSet =
              if Set.member taskId m.expandedTaskResources
                then Set.delete taskId m.expandedTaskResources
                else Set.insert taskId m.expandedTaskResources
         in m & #expandedTaskResources .~ newSet

    update (OpenPagePrintModal mLayoutId) = do
      m <- M.get
      let infos = taskInfosFromTws m.projection.tasksWithSolutions
      case mLayoutId of
        -- Load existing layout
        Just lid -> case filter (\l -> l.id == lid) m.projection.assignmentLayouts of
          (layout : _) -> do
            M.modify $ \m' ->
              m' & #pagePrintModal .~ Just (initFromLayout layout infos)
            M.io $ do
              threadDelay 100000
              heights <- measureTaskHeights
              footerH <- measureFooterHeight
              let s = layout.printSettings
                  cs' = layout.contentSettings
                  (firstAvail, restAvail) = decorationAdjustedHeights s cs'
                  gap = minGapPx s.baseFontSize
                  baseGrouping = groupIntoPages firstAvail restAvail gap s.distributeLastPage heights
                  finalGrouping = adjustForFooter footerH firstAvail restAvail gap s.distributeLastPage baseGrouping heights
              pure (PagePrintMsg (MeasuredPageGrouping finalGrouping))
          [] -> pure () -- Layout not found, do nothing

        -- Create new layout: generate ID + timestamp in IO, send Create, then open
        Nothing -> M.io $ do
          newId <- Id <$> randomIO
          now <- getCurrentTime
          let layout = Layout
                { id = newId
                , assignmentId = assignment.id
                , preset = Aufgabenblatt
                , printSettings = defaultPrintSettings
                , contentSettings = defaultContentSettings
                , createdAt = now
                }
          modifySyncDocument r (Layouts (OnLayouts (Create layout)))
          pure (OpenNewLayoutModal layout)

    update (OpenNewLayoutModal layout) = do
      M.modify $ \m ->
        let infos = taskInfosFromTws m.projection.tasksWithSolutions
         in m & #pagePrintModal .~ Just (initPrintModalModel layout infos)
      M.io $ do
        threadDelay 100000
        heights <- measureTaskHeights
        footerH <- measureFooterHeight
        let s = defaultPrintSettings
            cs' = defaultContentSettings
            (firstAvail, restAvail) = decorationAdjustedHeights s cs'
            gap = minGapPx s.baseFontSize
            baseGrouping = groupIntoPages firstAvail restAvail gap s.distributeLastPage heights
            finalGrouping = adjustForFooter footerH firstAvail restAvail gap s.distributeLastPage baseGrouping heights
        pure (PagePrintMsg (MeasuredPageGrouping finalGrouping))

    update (PagePrintMsg ToggleReorderMode) = do
      m <- M.get
      case m.pagePrintModal of
        Nothing -> pure ()
        Just mm ->
          if not mm.reorderMode
            then do
              -- Entering reorder mode: lock assignment, store original order
              let assignId = m.projection.currentAssignment.id
                  origOrder = m.projection.currentAssignment.tasks
                  mm' = updatePrintModal ToggleReorderMode 0 mm
              M.io_ $ modifySyncDocument r $ wrapForOrigin m.projection.origin $ Assignments (OnAssignments (Modify assignId Cmd.Lock))
              M.modify $ \m' ->
                m' & #pagePrintModal .~ Just (mm' & #originalTaskOrder .~ origOrder)
            else do
              -- Exiting reorder mode: release assignment with new task order
              releaseReorderedTasks m.projection.origin mm m.projection.currentAssignment.id
              M.modify $ \m' ->
                m' & #pagePrintModal .~ Just (updatePrintModal ToggleReorderMode 0 mm)

    update (PagePrintMsg CancelPrint) = do
      m <- M.get
      case m.pagePrintModal of
        Nothing -> pure ()
        Just mm -> when mm.reorderMode $ cancelReorder m.projection.origin m.projection.currentAssignment.id
      M.modify $ \m' -> m' & #pagePrintModal .~ Nothing

    update (PagePrintMsg SaveLayout) = do
      m <- M.get
      case m.pagePrintModal of
        Nothing -> pure ()
        Just mm -> do
          releaseIfReordering m.projection.origin mm m.projection.currentAssignment.id
          M.io_ $ saveLayoutFromModal r mm
          M.modify $ \m' -> m' & #pagePrintModal .~ Nothing

    update (PagePrintMsg PrintAndSaveLayout) = do
      m <- M.get
      case m.pagePrintModal of
        Nothing -> pure ()
        Just mm -> do
          releaseIfReordering m.projection.origin mm m.projection.currentAssignment.id
          M.io_ $ saveLayoutFromModal r mm
          let settings = mm.settings
              cs = mm.contentSettings
              pg = mm.pageGrouping
          M.modify $ \m' ->
            m' & #pagePrintModal .~ Nothing
               & #pagePrintPending .~ Just settings
               & #pagePrintPendingContent .~ Just cs
               & #pagePrintPageGrouping .~ pg
          M.io $ do
            threadDelay 800000 -- 800ms for MathJax
            triggerPrint
            pure ClearPagePrint

    update (PagePrintMsg OpenRenumberModal) = do
      m <- M.get
      case m.pagePrintModal of
        Nothing -> pure ()
        Just mm -> M.io_ $ do
          let taskOrder = reorderedTaskIds mm
              twsMap = Map.fromList [(tws.task.id, tws) | tws <- m.projection.tasksWithSolutions]
              mkInfo tid tws = RenumberTaskInfo
                { taskId = tid
                , identifier = tws.task.identifier
                , title = tws.task.title
                , isMultiAssignment = False
                , origin = if Set.member tid m.projection.draftTaskIds then Draft else Published
                }
              infos = mapMaybe (\tid -> mkInfo tid <$> Map.lookup tid twsMap) taskOrder
          openRenumberModal r infos

    update (DebouncedRemeasure gen) = do
      m <- M.get
      if m.footerDraftGen == gen
        then update (PagePrintMsg RemeasurePages)
        else pure () -- stale, a newer keystroke superseded this

    update (PagePrintMsg action) = do
      M.modify $ \m ->
        let expanded = case m.pagePrintModal of
              Nothing -> m.projection.tasksWithSolutions
              Just mm -> expandedTasks (Just (reorderedTaskIds mm)) mm.settings mm.contentSettings m.projection
            total = length expanded
         in m & #pagePrintModal .~ fmap (updatePrintModal action total) m.pagePrintModal
      if needsRemeasure action
        then doRemeasure
        else case action of
          SetCustomFooter _ -> scheduleDebouncedRemeasure
          SetPoints _ _ -> scheduleDebouncedRemeasure
          _ -> pure ()
      where
        scheduleDebouncedRemeasure = do
          M.modify $ \m -> m & #footerDraftGen .~ (m.footerDraftGen + 1)
          m <- M.get
          let gen = m.footerDraftGen
          M.io $ do
            threadDelay 500000 -- 500ms debounce
            pure (DebouncedRemeasure gen)

        doRemeasure = do
          m <- M.get
          let settings = maybe defaultPrintSettings (.settings) m.pagePrintModal
              cs' = maybe defaultContentSettings (.contentSettings) m.pagePrintModal
              (firstAvail, restAvail) = decorationAdjustedHeights settings cs'
              gap = minGapPx settings.baseFontSize
          M.io $ do
            threadDelay 100000 -- 100ms for DOM to re-render
            heights <- measureTaskHeights
            footerH <- measureFooterHeight
            let baseGrouping = groupIntoPages firstAvail restAvail gap settings.distributeLastPage heights
                finalGrouping = adjustForFooter footerH firstAvail restAvail gap settings.distributeLastPage baseGrouping heights
            pure (PagePrintMsg (MeasuredPageGrouping finalGrouping))

    update ClearPagePrint =
      M.modify $ \m -> m & #pagePrintPending .~ Nothing
                          & #pagePrintPendingContent .~ Nothing
                          & #pagePrintPageGrouping .~ []

    update PinThis = M.io_ $ pinAssignmentViewer r user assignment

    update (LayoutHoldAction ha) =
      HoldButton.handleHoldAction #layoutHoldState
        (\lid -> modifySyncDocument r (Layouts (OnLayouts (Delete lid))))
        LayoutHoldAction
        ha

    update OpenSubmissionModal = M.io_ $
      Submission.openSubmissionModal r assignment.id user.id

    update NoOp = pure ()

    -- | Release an assignment lock with the given patch, routed by origin
    releaseAssignment origin assignId patch =
      M.io_ $ modifySyncDocument r $ wrapForOrigin origin $ Assignments (OnAssignments (Modify assignId (Cmd.Release patch)))

    -- | Release the assignment lock, including task reorder if changed
    releaseReorderedTasks origin mm assignId = do
      let origOrder = mm.originalTaskOrder
          newOrder = reorderedTaskIds mm
          tasksChange = if origOrder == newOrder then Nothing else Just (origOrder, newOrder)
          patch = def & #tasks .~ tasksChange :: AssignmentPatch
      releaseAssignment origin assignId patch

    -- | Release the lock only if reorder mode was active
    releaseIfReordering origin mm assignId =
      when mm.reorderMode $ releaseReorderedTasks origin mm assignId

    -- | Release the lock discarding any task reorder
    cancelReorder origin assignId =
      releaseAssignment origin assignId (def :: AssignmentPatch)

    view' m =
      M.div_
        []
        [ M.div_
            [class_ "space-y-6 print-hide"]
            [ viewAssignment m
            ]
        , -- Page-print modal (when open)
          case m.pagePrintModal of
            Nothing -> M.text ""
            Just modalModel ->
              let taskOrder = reorderedTaskIds modalModel
                  reordered = reorderTasks taskOrder m.projection.tasksWithSolutions
                  expanded = expandedTasks (Just taskOrder) modalModel.settings modalModel.contentSettings m.projection
                  expandedCount = length expanded
                  taskNumMap = originalTaskNumbers reordered
                  renderFn = renderExpandedTaskForPrint modalModel.settings modalModel.contentSettings taskNumMap expanded
               in M.div_
                    []
                    [ printModalView
                        r.formulaCache
                        renderFn
                        expandedCount
                        (assignmentNameToText m.projection.currentAssignment.name)
                        (C.formatDay m.projection.currentAssignment.assignmentDate)
                        modalModel
                        PagePrintMsg
                    , -- Off-screen measurement container for continuous layout
                      case modalModel.settings.taskLayout of
                        Continuous ->
                          M.div_
                            []
                            [ measurementContainer renderFn expandedCount modalModel
                            , footerMeasureContainer r.formulaCache modalModel
                            ]
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
                [ -- Title line with action buttons on the right
                  Layout.hFlow (Layout.hFull <> Layout.crossCenter)
                    [ Typography.h2 (assignmentNameToText proj.currentAssignment.name)
                    , Layout.flowSpring
                    , M.div_
                        [class_ "text-sm"]
                        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) $
                            -- Students: status button; Teachers: status icon
                            (if proj.connectedUserRole == Student
                              then [viewSubmissionStatusButton proj.submissionSummary]
                              else [statusIcon proj.status])
                            <> [ pinButton PinThis | not (isPinned wm) ]
                            <> [ viewPagePrintButton m | proj.connectedUserRole == Teacher ]
                        ]
                    ]
                , -- Date below title (muted, small)
                  M.span_
                    [class_ "text-sm text-muted-foreground"]
                    [M.text $ C.formatDay proj.currentAssignment.assignmentDate]
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
                [ taskResourceListView r showPurposeBadge taskStatusRenderer proj.taskStatuses proj.tasksWithSolutions m.taskListState (viewTaskResources m r) TaskListAction ]
              )
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
    -- Submission Status Button (students only)
    -- ========================================================================

    viewSubmissionStatusButton :: Submission.SubmissionSummary -> M.View ViewerModel ViewerAction
    viewSubmissionStatusButton Submission.NoSubmissions =
      Button.primarySm (Button.button (C.translate' C.LblAbgabe) OpenSubmissionModal)
    viewSubmissionStatusButton (Submission.DigitalOnly _date) =
      Button.secondarySm (Button.button (C.translate' C.LblAbgegeben) OpenSubmissionModal)
    viewSubmissionStatusButton (Submission.NonDigitalOnly _date) =
      Button.secondarySm (Button.button (C.translate' C.LblGemacht) OpenSubmissionModal)
    viewSubmissionStatusButton Submission.DigitalAndNonDigital =
      Button.secondarySm (Button.button (C.translate' C.LblAbgegebenUndGemacht) OpenSubmissionModal)
    viewSubmissionStatusButton Submission.VoidOnly =
      Button.outlineSm (Button.button (C.translate' C.LblNichtGemacht) OpenSubmissionModal)

    -- ========================================================================
    -- Page Print
    -- ========================================================================

    viewPagePrintButton :: ViewerModel -> M.View ViewerModel ViewerAction
    viewPagePrintButton m =
      case m.projection.assignmentLayouts of
        [] -> Button.ghostSm (Button.button Icon.IcnPrint (OpenPagePrintModal Nothing))
        layouts ->
          HoverMenu.hoverMenuRight
            (Button.ghostSm (Button.button Icon.IcnPrint NoOp))
            ( map (layoutEntry m) layouts
                <> [ HoverMenu.hoverMenuSeparator
                   , HoverMenu.hoverMenuEntry False Icon.IcnPlus (C.translate' C.LblNewLayout) (OpenPagePrintModal Nothing)
                   ]
            )

    layoutEntry :: ViewerModel -> Layout -> M.View ViewerModel ViewerAction
    layoutEntry m layout =
      let presetLabel = presetName layout.preset
       in M.div_
            [class_ "flex items-center gap-1 px-1"]
            [ M.div_
                [ class_ "flex-1 cursor-pointer hover:bg-accent hover:text-accent-foreground px-2 py-1 rounded text-sm"
                , M.onClick (OpenPagePrintModal (Just layout.id))
                ]
                [ M.div_ [] [M.text presetLabel]
                , M.div_ [class_ "text-xs text-muted-foreground"] [M.text $ C.formatDateTime layout.createdAt]
                ]
            , HoldButton.holdDeleteButtonSm LayoutHoldAction m.layoutHoldState layout.id
            ]

    presetName :: ContentPreset -> MisoString
    presetName Aufgabenblatt = C.translate' C.LblPresetAufgabenblatt
    presetName Arbeitsblatt = C.translate' C.LblPresetArbeitsblatt
    presetName Loesungsblatt = C.translate' C.LblPresetLoesungsblatt
    presetName Musteraufgaben = C.translate' C.LblPresetMusteraufgaben

    -- | Build the expanded task list from settings, filtering by content visibility.
    -- When a reorder list is provided, tasks are rearranged accordingly.
    expandedTasks :: Maybe [TaskId] -> PrintSettings -> ContentSettings -> ViewerProjection -> [TaskWithSolutions]
    expandedTasks mReorder settings cs proj =
      let base = maybe id reorderTasks mReorder proj.tasksWithSolutions
          visible = filter (\tws -> isTaskVisible cs tws.task.id) base
       in expandTaskSequence settings.groupedCopies settings.totalCopies visible

    -- | Reorder tasks according to a list of TaskIds
    reorderTasks :: [TaskId] -> [TaskWithSolutions] -> [TaskWithSolutions]
    reorderTasks order twss =
      let twsMap = Map.fromList [(tws.task.id, tws) | tws <- twss]
       in mapMaybe (`Map.lookup` twsMap) order

    -- | Minimum gap between tasks in CSS px, corresponding to 1.5em at the given font size.
    -- 1pt = 96/72 CSS px, so 1.5em at Xpt = 1.5 * X * 96/72.
    minGapPx :: Double -> Double
    minGapPx fontSizePt = 1.5 * fontSizePt * 96.0 / 72.0

    -- | Compute first-page and rest-page available heights.
    -- The compact header and footer live in the page margin area.
    -- The first-page title and name field are in the content area,
    -- so they reduce available height on the first page.
    decorationAdjustedHeights :: PrintSettings -> ContentSettings -> (Double, Double)
    decorationAdjustedHeights s cs =
      let baseAvail = contentHeightPx s.paperSize s.orientation
          headerH = if cs.showTitle then firstPageHeaderPx s.baseFontSize else 0
          nameH = if cs.showNameField then nameFieldPx s.baseFontSize else 0
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
          cs = maybe defaultContentSettings id mCS
          expanded = expandedTasks Nothing settings cs proj
          taskNumMap = originalTaskNumbers proj.tasksWithSolutions
       in M.div_
            []
            [ case mSettings of
                Nothing -> M.text ""
                Just s -> printStyleView s cs
            , M.div_
                [class_ "hidden page-print-content"]
                (renderExpandedForPrint settings cs taskNumMap pageGrp expanded)
            ]

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
                      renderFn idx = case safeIndex expanded idx of
                        Nothing -> M.text ""
                        Just tws -> printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-task"] tws
                      taskIds = map (.task.id) (nubByTaskId expanded)
                      customFooterView = case cs.customFooter of
                        Just footer -> Just (Footer.renderCustomFooter r.formulaCache footer cs taskIds)
                        Nothing -> Nothing
                   in zipWith (Page.renderContinuousPage settings cs title date totalPages renderFn customFooterView) [0 ..] pageGrp
              | otherwise ->
                  -- Fallback: each task in a .print-task div, no forced page breaks
                  [ printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-task", MC.style_ [("margin-bottom", "1.5em")]] tws
                  | (_i, tws) <- zip [0 :: Int ..] expanded
                  ]
                  <> case cs.customFooter of
                       Just footer -> [Footer.renderCustomFooter r.formulaCache footer cs (map (.task.id) (nubByTaskId expanded))]
                       Nothing -> []
            Grid gc ->
              -- Group into pages, each page in a .print-page grid div
              let cpp = cellsPerPage gc
                  allIndices = [0 .. length expanded - 1]
                  pages = chunksOf cpp allIndices
                  renderFn idx = case safeIndex expanded idx of
                    Nothing -> M.div_ [class_ "print-cell"] []
                    Just tws -> printTaskView style cs (taskNumFor taskNumMap tws) [class_ "print-cell"] tws
               in map (\pageIdxs -> Page.renderGridPage settings.paperSize settings.orientation gc renderFn pageIdxs) pages

    -- | Remove duplicate tasks (from expandTaskSequence copies)
    nubByTaskId :: [TaskWithSolutions] -> [TaskWithSolutions]
    nubByTaskId = go Set.empty
      where
        go _ [] = []
        go seen (tws : rest)
          | Set.member tws.task.id seen = go seen rest
          | otherwise = tws : go (Set.insert tws.task.id seen) rest

    safeIndex :: [a] -> Int -> Maybe a
    safeIndex xs i
      | i < 0 = Nothing
      | otherwise = case drop i xs of
          [] -> Nothing
          (x : _) -> Just x

    -- | Render a task for print: title h2 + optional description + solutions + grid,
    -- wrapped in a div with the given attributes. Visual styling
    -- (font-size, h2 sizing, margins) comes from the shared
    -- .page-print-content CSS rule.
    printTaskView :: TaskHeaderStyle -> ContentSettings -> Int -> [M.Attribute ViewerAction] -> TaskWithSolutions -> M.View ViewerModel ViewerAction
    printTaskView style cs taskNum attrs tws =
      let displayName = taskDisplayName tws.task
          tcs = taskContentSetting cs tws.task.id
          prefix = C.translate' C.LblTaskWord
          numText = ms (show taskNum) <> "."
          pointsSpan = case tcs.points of
            Nothing -> []
            Just p ->
              [ M.span_
                  [class_ "print-task-points"]
                  [ M.span_
                      [MC.style_ [("display", "inline-block"), ("border-bottom", "1px solid #999"), ("width", "3em"), ("vertical-align", "bottom")]]
                      [M.text "\xA0"]
                  , M.text $ " / " <> ms (Footer.showPoints p) <> " " <> C.translate' C.LblPoints
                  ]
              ]
          header = case style of
            HeaderNumber ->
              [M.h2_ [] ([M.text (prefix <> numText)] <> pointsSpan)]
            HeaderTitle ->
              [M.h2_ [] ([M.text $ ms displayName] <> pointsSpan)]
            HeaderBoth ->
              [M.h2_ []
                ([ M.strong_ [] [M.text (prefix <> numText)]
                , M.text (" " <> ms displayName)
                ] <> pointsSpan)]
          descriptionView
            | tcs.showDescription =
                [ M.div_
                    [ MP.class_ $ "prose prose-stone prose-sm max-w-none"
                        <> printColumnsClass tcs.itemsPerRow
                        <> if tcs.inlineAnswer then " print-inline-answer" else ""
                    ]
                    [renderRichTextWithFiles r.formulaCache r tws.task.attachments content]
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
            , ("background-image", "url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='100%25' height='100%25'><defs><pattern id='g' patternUnits='userSpaceOnUse' width='5mm' height='5mm' x='2.5mm' y='2.5mm'><line x1='2.5mm' y1='0' x2='2.5mm' y2='5mm' stroke='%23ccc' stroke-width='0.1mm'/><line x1='0' y1='2.5mm' x2='5mm' y2='2.5mm' stroke='%23ccc' stroke-width='0.1mm'/></pattern></defs><rect width='100%25' height='100%25' fill='url(%23g)'/></svg>\")")
            , ("background-size", "100% 100%")
            , ("border", "0.1mm solid #ccc")
            , ("margin-top", "0.5em")
            , ("print-color-adjust", "exact")
            , ("-webkit-print-color-adjust", "exact")
            , ("image-rendering", "crisp-edges")
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

    taskInfosFromTws :: [TaskWithSolutions] -> [TaskInfo]
    taskInfosFromTws = mkTaskInfos . map (\tws -> (tws.task, tws.solutions, tws.taskContent))

    assignmentNameToText (AssignmentName t) = ms t
