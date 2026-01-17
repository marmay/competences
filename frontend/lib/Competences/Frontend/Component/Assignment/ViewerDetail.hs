module Competences.Frontend.Component.Assignment.ViewerDetail
  ( viewerDetailView
  , AssignmentStatus (..)
  , assignmentStatus
  , statusLabel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Document (..)
  , User (..)
  , emptyDocument
  )
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.Task
  ( Task (..)
  , TaskGroup
  , TaskGroupIxs
  , TaskIdentifier (..)
  , TaskIxs
  , getTaskContent
  )
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.TaskContentView (renderTaskContentText)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)

-- ============================================================================
-- Assignment Status Types and Logic
-- ============================================================================

-- | Assignment completion status for a user
data AssignmentStatus
  = NotGraded -- "Nicht korrigiert" - No evidence exists
  | NeedsWork -- "Zu verbessern" - Has WithSupport or NotYet abilities
  | Completed -- "Erledigt" - All SelfReliant or SelfReliantWithSillyMistakes
  deriving (Eq, Show)

-- | Determine assignment status for a user
-- Uses the direct assignmentId link on Evidence
assignmentStatus :: Document -> UserId -> AssignmentId -> AssignmentStatus
assignmentStatus doc userId assignmentId =
  let -- Find evidences for this user linked to this assignment
      userEvidences = Ix.toList $ doc.evidences Ix.@= userId
      linkedEvidences = filter (\e -> e.assignmentId == Just assignmentId) userEvidences
   in case linkedEvidences of
        [] -> NotGraded -- No evidence linked to this assignment
        evidences ->
          let -- Get all abilities from observations across all linked evidences
              allAbilities = concatMap (map (.ability) . Ix.toList . (.observations)) evidences
              hasNeedsWork = any (`elem` [WithSupport, NotYet]) allAbilities
           in if null allAbilities
                then NotGraded -- Has evidence but no observations yet
                else
                  if hasNeedsWork
                    then NeedsWork
                    else Completed -- All SelfReliant or SelfReliantWithSillyMistakes

-- | Status label for display
statusLabel :: AssignmentStatus -> MisoString
statusLabel NotGraded = "Nicht korrigiert"
statusLabel NeedsWork = "Zu verbessern"
statusLabel Completed = "Erledigt"

-- | Status badge variant
statusBadgeVariant :: AssignmentStatus -> Badge.BadgeVariant
statusBadgeVariant NotGraded = Badge.BadgeSecondary
statusBadgeVariant NeedsWork = Badge.BadgeOutline
statusBadgeVariant Completed = Badge.BadgePrimary

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

-- | Internal model for the viewer component
data ViewerModel = ViewerModel
  { assignment :: !Assignment
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , currentUserId :: !UserId
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

-- | The viewer component
viewerComponent :: SyncContext -> User -> Assignment -> M.Component p ViewerModel ViewerAction
viewerComponent r user assignment =
  (M.component model update view')
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      ViewerModel
        { assignment = assignment
        , tasks = Ix.empty
        , taskGroups = Ix.empty
        , currentUserId = user.id
        , document = emptyDocument
        }

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          -- Look up the current assignment from the document (in case it was edited)
          updatedAssignment = maybe m.assignment id $ Ix.getOne (doc.assignments Ix.@= m.assignment.id)
       in m
            { assignment = updatedAssignment
            , tasks = doc.tasks
            , taskGroups = doc.taskGroups
            , document = doc
            }

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewAssignmentHeader m
        , viewTaskList m
        ]

    viewAssignmentHeader m =
      let status = assignmentStatus m.document m.currentUserId m.assignment.id
       in Card.card
            [ M.div_
                [class_ "space-y-3"]
                [ -- Title and status badge
                  M.div_
                    [class_ "flex items-center justify-between"]
                    [ Typography.h2 (assignmentNameToText m.assignment.name)
                    , Badge.badge (statusBadgeVariant status) (statusLabel status)
                    ]
                , -- Metadata
                  M.div_
                    [class_ "flex flex-wrap gap-4 text-sm text-muted-foreground"]
                    [ M.span_
                        []
                        [ M.text "Datum: "
                        , M.text $ C.formatDay m.assignment.assignmentDate
                        ]
                    , M.span_
                        []
                        [ M.text "Art: "
                        , M.text $ C.translate' $ C.LblActivityTypeDescription m.assignment.activityType
                        ]
                    ]
                ]
            ]

    viewTaskList m =
      let -- Sort tasks by identifier for consistent display order
          sortedTasks =
            Ix.toAscList (Proxy @TaskIdentifier) $ m.tasks Ix.@+ m.assignment.tasks
       in M.div_
            [class_ "space-y-4"]
            [ Typography.h3 $ C.translate' C.LblAssignmentTasks
            , if null sortedTasks
                then Typography.muted "Keine Aufgaben"
                else M.div_ [class_ "space-y-4"] (map (viewTask m) sortedTasks)
            ]

    viewTask m task =
      let TaskIdentifier identifier = task.identifier
          content = getTaskContent m.taskGroups task
       in Card.card
            [ M.div_
                [class_ "space-y-2"]
                [ -- Task identifier as header
                  M.div_
                    [class_ "font-semibold text-foreground"]
                    [M.text $ ms identifier]
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
