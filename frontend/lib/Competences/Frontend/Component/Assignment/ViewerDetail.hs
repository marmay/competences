module Competences.Frontend.Component.Assignment.ViewerDetail
  ( viewerDetailView
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
import Competences.Document.Evidence (Ability (..))
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
import Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalAction (..)
  , PrintModalModel (..)
  , defaultPrintModalModel
  , printModalView
  , updatePrintModal
  )
import Competences.Frontend.Component.PrintEngine.Types (PrintSettings (..), defaultPrintSettings)
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
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (PinId (..), WindowChrome (..), WindowMode, inlineComponent, inlineComponentWith, isPinned, pinDialogWith)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Color (textClass')
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
import Miso.DSL (jsg, (#))
import Miso.Event (onCreated)
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))

-- ============================================================================
-- Print Content Selection
-- ============================================================================

-- | What to include in printed output
data PrintContent = PrintTasks | PrintSolutions | PrintBoth
  deriving (Eq, Show)

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
  }

-- ============================================================================
-- Viewer Detail Component
-- ============================================================================

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
  , printMode :: !PrintContent
  , printDropdownOpen :: !Bool
  , printPending :: !Bool
  , expandedTaskResources :: !(Set.Set TaskId)
  , pagePrintModal :: !(Maybe PrintModalModel)
  , pagePrintPending :: !(Maybe PrintSettings)
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | TaskListAction !TRL.Action
  | TogglePrintDropdown
  | DoPrintWith !PrintContent
  | ExecutePrint
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
      , printMode = PrintBoth
      , printDropdownOpen = False
      , printPending = False
      , expandedTaskResources = Set.empty
      , pagePrintModal = Nothing
      , pagePrintPending = Nothing
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

    update TogglePrintDropdown =
      M.modify $ \m -> m & #printDropdownOpen .~ not m.printDropdownOpen

    update (DoPrintWith mode) =
      M.modify $ \m -> m & #printMode .~ mode
                          & #printDropdownOpen .~ False
                          & #printPending .~ True

    update ExecutePrint = do
      M.modify $ \m -> m & #printPending .~ False
      M.io_ triggerPrint

    update (ToggleTaskResourcesExpanded taskId) =
      M.modify $ \m ->
        let newSet =
              if Set.member taskId m.expandedTaskResources
                then Set.delete taskId m.expandedTaskResources
                else Set.insert taskId m.expandedTaskResources
         in m & #expandedTaskResources .~ newSet

    update OpenPagePrintModal =
      M.modify $ \m -> m & #pagePrintModal .~ Just defaultPrintModalModel

    update (PagePrintMsg CancelPrint) =
      M.modify $ \m -> m & #pagePrintModal .~ Nothing

    update (PagePrintMsg ConfirmPrint) = do
      M.modify $ \m ->
        let settings = maybe defaultPrintSettings (.settings) m.pagePrintModal
         in m & #pagePrintModal .~ Nothing
              & #pagePrintPending .~ Just settings
      -- Delay to let MathJax finish rendering formulas in the hidden div,
      -- then trigger the browser print dialog and clean up afterwards.
      M.io $ do
        threadDelay 800000 -- 800ms for MathJax
        triggerPrint
        pure ClearPagePrint

    update (PagePrintMsg action) =
      M.modify $ \m ->
        let total = length m.projection.tasksWithSolutions
         in m & #pagePrintModal .~ fmap (updatePrintModal action total) m.pagePrintModal

    update ClearPagePrint =
      M.modify $ \m -> m & #pagePrintPending .~ Nothing

    update PinThis = M.io_ $
      let AssignmentName nameText = assignment.name
          chrome = WindowChrome (M.ms nameText) Icon.IcnAssignment
       in pinDialogWith r.windowManager
            (PinId $ "assignment-" <> idToText assignment.id)
            chrome
            (viewerComponent r user assignment)

    view' m =
      let pagePrintActive = pagePrintIsActive m
       in M.div_
            []
            [ M.div_
                [class_ "space-y-6 print:hidden"]
                [ viewAssignment m
                ]
            , -- Existing print content (from dropdown) — hidden when page-print is active
              if pagePrintActive
                then M.text ""
                else M.div_
                       [class_ "hidden print:block"]
                       [viewPrintContent m]
            , printSentinel m
            , -- Page-print modal (when open)
              case m.pagePrintModal of
                Nothing -> M.text ""
                Just modalModel ->
                  printModalView
                    (renderTaskForPrint m.projection)
                    (length m.projection.tasksWithSolutions)
                    modalModel
                    PagePrintMsg
            , -- Page-print tasks: pre-rendered while modal is open (so MathJax
              -- has time to render), kept when printing. The @page style is only
              -- injected at print time so it doesn't affect the old print path.
              if pagePrintActive
                then viewPagePrintContent m.pagePrintPending m.projection
                else M.text ""
            ]

    printSentinel :: ViewerModel -> M.View ViewerModel ViewerAction
    printSentinel m
      | m.printPending = M.div_ [onCreated ExecutePrint, class_ "hidden"] []
      | otherwise = M.text ""

    viewAssignment m =
      let proj = m.projection
          desc = proj.currentAssignment.description
          showPurposeBadge = proj.connectedUserRole == Teacher
          taskStatusRenderer = viewTaskCompletionStatusFromMap proj.taskStatuses
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
                            <> [ viewPrintDropdown m ]
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

    -- ========================================================================
    -- Print Dropdown
    -- ========================================================================

    viewPrintDropdown :: ViewerModel -> M.View ViewerModel ViewerAction
    viewPrintDropdown m =
      M.div_
        [class_ "relative"]
        [ M.button_
            [ class_ "inline-flex items-center gap-1 px-2 py-1 rounded text-xs text-muted-foreground hover:text-foreground hover:bg-muted transition-colors"
            , M.onClick TogglePrintDropdown
            , MP.title_ (C.translate' C.LblPrint)
            ]
            [Icon.iconS Icon.Small Icon.IcnPrint]
        , if m.printDropdownOpen
            then viewPrintDropdownMenu
            else M.text ""
        ]

    viewPrintDropdownMenu :: M.View ViewerModel ViewerAction
    viewPrintDropdownMenu =
      M.div_
        [class_ "absolute right-0 top-full mt-1 z-50 min-w-36 bg-popover text-popover-foreground border border-border rounded-md shadow-lg py-1"]
        [ M.div_
            [class_ "fixed inset-0 z-[-1]", M.onClick TogglePrintDropdown]
            []
        , printDropdownItem PrintTasks C.LblPrintTasks
        , printDropdownItem PrintSolutions C.LblPrintSolutions
        , printDropdownItem PrintBoth C.LblPrintAll
        ]

    printDropdownItem :: PrintContent -> C.Label -> M.View ViewerModel ViewerAction
    printDropdownItem mode lbl =
      M.button_
        [ class_ "flex w-full items-center gap-2 rounded-sm px-2 py-1.5 text-sm hover:bg-accent hover:text-accent-foreground transition-colors"
        , M.onClick (DoPrintWith mode)
        ]
        [ Icon.iconS Icon.Small Icon.IcnPrint
        , M.text (C.translate' lbl)
        ]

    -- ========================================================================
    -- Print Content (hidden on screen, visible in print)
    -- ========================================================================

    viewPrintContent :: ViewerModel -> M.View ViewerModel ViewerAction
    viewPrintContent m =
      let proj = m.projection
          asmtName = assignmentNameToText proj.currentAssignment.name
          asmtDate = C.formatDay proj.currentAssignment.assignmentDate
       in M.div_
            [class_ "space-y-6"]
            ( [ M.h1_ [class_ "text-2xl font-bold"] [M.text asmtName]
              , M.p_ [class_ "text-sm text-muted-foreground"] [M.text asmtDate]
              ]
              <> concatMap (viewPrintTask m.printMode) proj.tasksWithSolutions
            )

    viewPrintTask :: PrintContent -> TaskWithSolutions -> [M.View ViewerModel ViewerAction]
    viewPrintTask mode tws =
      let TaskIdentifier ident = tws.task.identifier
          showTask = mode == PrintTasks || mode == PrintBoth
          showSolutions = mode == PrintSolutions || mode == PrintBoth
       in [ M.div_
              [class_ "space-y-2 mt-4"]
              ( [M.h2_ [class_ "text-lg font-semibold"] [M.text $ ms ident]]
                <> [ M.div_
                       [class_ "prose prose-stone prose-sm max-w-none"]
                       [renderRichText r.formulaCache content]
                   | showTask
                   , Just content <- [tws.taskContent]
                   ]
                <> [ M.div_
                       [class_ "mt-2 space-y-1"]
                       (map viewPrintSolution tws.solutions)
                   | showSolutions
                   , not (null tws.solutions)
                   ]
              )
          ]

    viewPrintSolution :: Solution -> M.View ViewerModel ViewerAction
    viewPrintSolution sol =
      M.div_
        [class_ "pl-4 border-l-2 border-muted space-y-1"]
        [ M.p_
            [class_ "text-xs font-medium text-muted-foreground"]
            [M.text $ C.translate' (C.LblSolutionType sol.solutionType)]
        , M.div_
            [class_ "prose prose-stone prose-sm max-w-none"]
            [renderRichText r.formulaCache sol.content]
        ]

    -- ========================================================================
    -- Page Print (one task per page)
    -- ========================================================================

    -- | Page-print is active when the modal is open or print is pending
    pagePrintIsActive :: ViewerModel -> Bool
    pagePrintIsActive m = case m.pagePrintModal of
      Just _ -> True
      Nothing -> case m.pagePrintPending of
        Just _ -> True
        Nothing -> False

    viewPagePrintButton :: M.View ViewerModel ViewerAction
    viewPagePrintButton =
      Button.ghostSm (Button.button (Icon.IcnPrint, C.LblPrintPreview) OpenPagePrintModal)

    -- | Render a single task for print preview / print output
    renderTaskForPrint :: ViewerProjection -> Int -> M.View ViewerModel ViewerAction
    renderTaskForPrint proj idx =
      case drop idx proj.tasksWithSolutions of
        [] -> M.text ""
        (tws : _) ->
          let TaskIdentifier ident = tws.task.identifier
           in M.div_
                []
                ( [M.h2_ [class_ "text-lg font-semibold mb-2"] [M.text $ ms ident]]
                  <> [ M.div_
                         [class_ "prose prose-stone prose-sm max-w-none"]
                         [renderRichText r.formulaCache content]
                     | Just content <- [tws.taskContent]
                     ]
                )

    -- | Hidden div with all tasks for page-print (one per page).
    -- When mSettings is Just, the @page style is injected (actual print).
    -- When Nothing, an empty placeholder keeps child positions stable
    -- so Miso doesn't recreate richContent components (preserving MathJax state).
    viewPagePrintContent :: Maybe PrintSettings -> ViewerProjection -> M.View ViewerModel ViewerAction
    viewPagePrintContent mSettings proj =
      M.div_
        [class_ "hidden page-print-content"]
        ( [maybe (M.text "") printStyleView mSettings]
          <> zipWith (viewPagePrintTask proj) [0 ..] proj.tasksWithSolutions
        )

    viewPagePrintTask :: ViewerProjection -> Int -> TaskWithSolutions -> M.View ViewerModel ViewerAction
    viewPagePrintTask _proj _idx tws =
      let TaskIdentifier ident = tws.task.identifier
       in M.div_
            [class_ "print-task"]
            ( [M.h2_ [class_ "text-lg font-semibold mb-2"] [M.text $ ms ident]]
              <> [ M.div_
                     [class_ "prose prose-stone prose-sm max-w-none"]
                     [renderRichText r.formulaCache content]
                 | Just content <- [tws.taskContent]
                 ]
            )

    assignmentNameToText (AssignmentName t) = ms t
