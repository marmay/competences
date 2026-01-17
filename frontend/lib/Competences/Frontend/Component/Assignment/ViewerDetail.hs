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
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.Task
  ( Task (..)
  , TaskId
  , TaskIdentifier (..)
  , getTaskContent
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.TaskContentView (renderTaskContentText)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Colors (abilityTextClass)
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (AssignmentStatus (..), assignmentStatus)
import Competences.Query.Assignment qualified as Q
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Miso.Svg.Property qualified as MSP
import Optics.Core ((.~))

-- ============================================================================
-- Status Helpers (delegate to Query module)
-- ============================================================================

-- | Status label for display (wraps Query module's Text version to MisoString)
statusLabel :: AssignmentStatus -> MisoString
statusLabel = ms . Q.statusLabel

-- | Status badge variant
statusBadgeVariant :: AssignmentStatus -> Badge.BadgeVariant
statusBadgeVariant NotGraded = Badge.BadgeSecondary
statusBadgeVariant NeedsWork = Badge.BadgeOutline
statusBadgeVariant Completed = Badge.BadgePrimary

-- ============================================================================
-- Viewer Projection (pre-computed data)
-- ============================================================================

-- | Pre-computed projection for the viewer
-- All expensive queries are done once per document/user change
data ViewerProjection = ViewerProjection
  { -- | Pre-filtered and sorted tasks for this assignment
    relevantTasks :: ![Task]
    -- | Pre-computed: task content from task groups
  , taskContents :: !(Map.Map TaskId (Maybe Text))
    -- | Pre-computed: all observations from linked evidences (at assignment level)
  , observations :: ![Observation]
    -- | Competences for looking up level descriptions
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
    -- | Pre-computed: assignment status for the effective user
  , status :: !AssignmentStatus
    -- | The current assignment (may be updated if edited)
  , currentAssignment :: !Assignment
    -- | Focused user (for header display, can be Nothing for students)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Empty projection for initial state
emptyProjection :: Assignment -> ViewerProjection
emptyProjection assignment = ViewerProjection
  { relevantTasks = []
  , taskContents = Map.empty
  , observations = []
  , competences = Ix.empty
  , status = NotGraded
  , currentAssignment = assignment
  , focusedUser = Nothing
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
  V.component
    ("assignment-viewer-" <> M.ms (show assignment.id))
    (viewerComponent r user assignment)

-- | Minimal model - only stores the projection
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  deriving (Eq, Show)

-- | The viewer component using subscribeWithProjection pattern
viewerComponent :: SyncContext -> User -> Assignment -> M.Component p ViewerModel ViewerAction
viewerComponent r user assignment =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection assignment user.id) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = emptyProjection assignment
      }

    -- Projection function captures assignment and currentUserId from closure
    viewerProjection :: Assignment -> UserId -> Document -> Maybe User -> ViewerProjection
    viewerProjection asmt currentUserId doc mUser =
      let -- Determine effective user (focused or fallback to connected)
          effectiveUserId = maybe currentUserId (.id) mUser

          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = maybe asmt id $ Ix.getOne (doc.assignments Ix.@= asmt.id)

          -- Filter tasks for this assignment, sorted by identifier
          relevantTasks = Ix.toAscList (Proxy @TaskIdentifier) $
            doc.tasks Ix.@+ updatedAssignment.tasks

          -- Pre-compute task contents
          taskContents = Map.fromList
            [ (task.id, getTaskContent doc.taskGroups task)
            | task <- relevantTasks
            ]

          -- Find evidences linked to this assignment for effective user
          linkedEvidence = filter (\e -> e.assignmentId == Just updatedAssignment.id) $
            Ix.toList $ doc.evidences Ix.@= effectiveUserId

          -- Extract all observations at assignment level (from linked evidences)
          observations = concatMap (Ix.toList . (.observations)) linkedEvidence

          -- Get competence IDs referenced by observations (for level description lookup)
          referencedCompetenceIds = map (fst . (.competenceLevelId)) observations
          competences = doc.competences Ix.@+ referencedCompetenceIds

          -- Pre-compute status
          status = assignmentStatus doc effectiveUserId updatedAssignment.id

       in ViewerProjection
            { relevantTasks
            , taskContents
            , observations
            , competences
            , status
            , currentAssignment = updatedAssignment
            , focusedUser = mUser
            }

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewAssignmentHeader m
        , viewTaskList m
        ]

    viewAssignmentHeader m =
      let proj = m.projection
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Line 1: Title
                  Typography.h2 (assignmentNameToText proj.currentAssignment.name)
                , -- Line 2: Date + Status
                  M.div_
                    [class_ "flex items-center gap-4 text-sm"]
                    [ M.span_
                        [class_ "text-muted-foreground"]
                        [M.text $ C.formatDay proj.currentAssignment.assignmentDate]
                    , Badge.badge (statusBadgeVariant proj.status) (statusLabel proj.status)
                    ]
                , -- Observations list (one per line)
                  viewObservationList proj
                ]
            ]

    viewObservationList proj =
      if null proj.observations
        then M.text ""
        else M.div_
               [class_ "mt-2 space-y-1"]
               (map (viewObservationDetail proj.competences) proj.observations)

    viewObservationDetail competences obs =
      let (competenceId, level) = obs.competenceLevelId
          abilityClass = abilityTextClass obs.ability
          abilityIcn = abilityIcon obs.ability
          abilityLabel = C.translate' (C.LblAbility obs.ability)
          levelDesc = case Ix.getOne (competences Ix.@= competenceId) of
            Nothing -> ""
            Just comp -> maybe "" (.description) (comp.levels Map.!? level)
       in M.div_
            [class_ "flex items-center gap-2 text-sm"]
            [ M.span_
                [class_ abilityClass]
                [icon [MSP.stroke_ "currentColor", class_ "w-4 h-4"] abilityIcn]
            , M.span_
                [class_ $ abilityClass <> " font-medium"]
                [M.text abilityLabel]
            , if levelDesc == ""
                then M.text ""
                else M.span_
                       [class_ "text-muted-foreground"]
                       [M.text $ "– " <> ms levelDesc]
            ]

    abilityIcon SelfReliant = IcnAbilitySelfReliant
    abilityIcon SelfReliantWithSillyMistakes = IcnAbilitySillyMistakes
    abilityIcon WithSupport = IcnAbilityWithSupport
    abilityIcon NotYet = IcnAbilityNotYet

    viewTaskList m =
      let proj = m.projection
       in M.div_
            [class_ "space-y-4"]
            [ Typography.h3 $ C.translate' C.LblAssignmentTasks
            , if null proj.relevantTasks
                then Typography.muted "Keine Aufgaben"
                else M.div_ [class_ "space-y-4"] (map (viewTask m) proj.relevantTasks)
            ]

    viewTask m task =
      let content = fromMaybe Nothing $ Map.lookup task.id m.projection.taskContents
          TaskIdentifier identifier = task.identifier
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Task identifier
                  M.div_ [class_ "font-semibold text-foreground"] [M.text $ ms identifier]
                , -- Task content rendered with MathJax
                  case content of
                    Nothing -> Typography.muted "Kein Inhalt"
                    Just c ->
                      if c == ""
                        then Typography.muted "Kein Inhalt"
                        else M.div_ [class_ "prose prose-stone max-w-none"] [renderTaskContentText c]
                ]
            ]

    assignmentNameToText (AssignmentName t) = ms t
