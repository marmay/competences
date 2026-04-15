-- | Shared view for rendering task completion status.
-- Reusable across assignment viewer, competence resources, and assignment evaluator.
module Competences.Frontend.Fragment.TaskStatus
  ( viewTaskCompletionStatus
  , viewTaskCompletionStatusFromMap
  )
where

import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Color (textClass')
import Competences.Frontend.View.Color.Completion (CompletionStatus (..))
import Competences.Frontend.View.Color.Completion qualified as Completion
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.TaskStatus (EvidenceRef (..), TaskCompletionStatus (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Render completion status for a single task.
--
-- * 'TaskDone' -> green check icon + "Stand:" text
-- * 'TaskNotDone' -> yellow progress icon + "Stand:" text
-- * 'TaskNotEvaluated' -> empty (no indicator)
viewTaskCompletionStatus :: TaskCompletionStatus -> M.View model a
viewTaskCompletionStatus TaskNotEvaluated = Layout.empty
viewTaskCompletionStatus (TaskDone ref) =
  statusView (textClass' $ Completion.completionPalette Done) Icon.IcnApply ref
viewTaskCompletionStatus (TaskNotDone ref) =
  statusView (textClass' $ Completion.completionPalette InProgress) Icon.IcnProgress ref

-- | Convenience: look up task status from a map and render.
-- Returns empty view for tasks not in the map.
viewTaskCompletionStatusFromMap :: Map TaskId TaskCompletionStatus -> TaskId -> M.View model a
viewTaskCompletionStatusFromMap statuses taskId =
  maybe Layout.empty viewTaskCompletionStatus (Map.lookup taskId statuses)

-- | Internal: render status icon + "Stand:" text.
statusView :: Text -> Icon.Icon -> EvidenceRef -> M.View model a
statusView colorClass icn ref =
  Layout.hFlow
    (Layout.gapT <> Layout.hFull <> Layout.crossCenter)
    [ Icon.icon [class_ $ "w-4 h-4 " <> colorClass] icn
    , MH.span_
        [class_ "text-xs text-muted-foreground"]
        [M.text $ C.translate' C.LblTaskCompletionAsOf <> " " <> refLabel ref <> ", " <> formatShortDay ref.date]
    ]

-- | Format evidence reference as label text.
-- Uses assignment name if available, otherwise activity type description.
refLabel :: EvidenceRef -> M.MisoString
refLabel ref = case ref.assignmentName of
  Just (AssignmentName name) -> M.ms name
  Nothing -> C.translate' (C.LblActivityTypeDescription ref.activityType)

-- | Format a day as short "dd.mm." (day and month only, no year).
formatShortDay :: Day -> M.MisoString
formatShortDay = M.ms . formatTime defaultTimeLocale "%d.%m."
