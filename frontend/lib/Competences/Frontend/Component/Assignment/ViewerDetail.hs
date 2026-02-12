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
  , User (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
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
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Color (textClass')
import Competences.Frontend.View.Color.Ability (abilityPalette)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Color.Completion (CompletionStatus (..))
import Competences.Frontend.View.StatusIcon (completionIcon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.User (UserRole (..))
import Competences.Query.Assignment (AssignmentStatus (..), accumulatedObservations, assignmentStatus)
import Competences.Query.Assignment qualified as Q
import Competences.Query.TaskStatus (TaskCompletionStatus, taskCompletionStatuses)
import Competences.Frontend.View.TaskStatus (viewTaskCompletionStatusFromMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))

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
  component
    ("assignment-viewer-" <> M.ms (show assignment.id))
    (viewerComponent r user assignment)

-- | Model with projection and task list state
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  , taskListState :: !TaskResourceList
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | TaskListAction !TRL.Action
  deriving (Eq, Show)

-- | The viewer component using subscribeWithProjection pattern
viewerComponent :: SyncContext -> User -> Assignment -> M.Component p ViewerModel ViewerAction
viewerComponent r user assignment =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection assignment user.id user.role) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = emptyProjection user.role assignment
      , taskListState = initialState TasksExpanded Map.empty []
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

       in ViewerProjection
            { tasksWithSolutions
            , accumulatedObs = accumulated
            , competences
            , status
            , currentAssignment = updatedAssignment
            , focusedUser = mUser
            , connectedUserRole = role
            , taskStatuses
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

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewAssignmentHeader m
        , viewTaskList m
        ]

    viewAssignmentHeader m =
      let proj = m.projection
          desc = proj.currentAssignment.description
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Title line with date + status on the right
                  Layout.viewFlow
                    Layout.hFlow{Layout.expandOrthogonal = Layout.Expand Layout.Center}
                    [ Typography.h2 (assignmentNameToText proj.currentAssignment.name)
                    , Layout.flowSpring
                    , Layout.viewFlow
                        Layout.hFlow
                          { Layout.gap = Layout.SmallSpace
                          , Layout.expandOrthogonal = Layout.Expand Layout.Center
                          , Layout.extraAttrs = [class_ "text-sm"]
                          }
                        [ M.span_
                            [class_ "text-muted-foreground"]
                            [M.text $ C.formatDay proj.currentAssignment.assignmentDate]
                        , statusIcon proj.status
                        ]
                    ]
                , -- Description (if present, supports math syntax)
                  if desc == mempty
                    then M.text ""
                    else M.div_
                           [class_ "prose prose-stone prose-sm max-w-none"]
                           [renderRichText desc]
                , -- Accumulated observations list (one per competence level)
                  viewObservationList proj
                ]
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
       in Layout.viewFlow
            Layout.hFlow
              { Layout.gap = Layout.SmallSpace
              , Layout.expandOrthogonal = Layout.Expand Layout.Center
              , Layout.extraAttrs = [class_ "text-sm"]
              }
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

    abilityIcon SelfReliant = Icon.IcnAbilitySelfReliant
    abilityIcon SelfReliantWithSillyMistakes = Icon.IcnAbilitySillyMistakes
    abilityIcon WithSupport = Icon.IcnAbilityWithSupport
    abilityIcon NotYet = Icon.IcnAbilityNotYet

    viewTaskList m =
      let proj = m.projection
          -- Only show purpose badges for teachers
          showPurposeBadge = proj.connectedUserRole == Teacher
          taskStatusRenderer = viewTaskCompletionStatusFromMap proj.taskStatuses
       in M.div_
            [class_ "space-y-4"]
            [ Typography.h3 $ C.translate' C.LblAssignmentTasks
            , taskResourceListView showPurposeBadge taskStatusRenderer proj.taskStatuses proj.tasksWithSolutions m.taskListState TaskListAction
            ]

    assignmentNameToText (AssignmentName t) = ms t
