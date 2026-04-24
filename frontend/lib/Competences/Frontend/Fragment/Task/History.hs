-- | Per-task evaluation-history fragment — the "Bearbeitungshistorie"
-- block shown beneath a task's content in both the standalone task
-- detailed view and the assignment viewer.
--
-- Pure per the @Fragment.*@ rule in CLAUDE.md: no 'SyncContext',
-- no 'Command', no 'IO'. Effectful bits (the per-row entity menu,
-- rich-content rendering) are supplied by callers.
module Competences.Frontend.Fragment.Task.History
  ( -- * Projection
    EvidenceRow (..)
  , evaluationHistory
    -- * View
  , historySection
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), User (..))
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Evidence (Ability (..), Evidence (..), TaskRemark)
import Competences.Document.Task (Task (..), TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Color (textClass')
import Competences.Frontend.View.Color.Completion (CompletionStatus (..), completionPalette)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.TaskStatus
  ( TaskCompletionStatus (..)
  , mkEvidenceRef
  )
import Competences.TaskContent.RichContent (RichContent)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- | One row in the per-task evaluation history. Carries the resolved
-- assignment (if any) and the per-evidence completion status so the
-- view layer doesn't need further document lookups.
data EvidenceRow = EvidenceRow
  { evidence :: !Evidence
  , assignment :: !(Maybe Assignment)
  , status :: !TaskCompletionStatus
  }
  deriving (Eq, Generic, Show)

-- | All evidences for the given user that mention this task,
-- newest-first, each paired with its resolved assignment and the
-- per-evidence completion status.
evaluationHistory :: Document -> User -> Task -> [EvidenceRow]
evaluationHistory doc user task =
  let userTaskEvs = doc.evidences Ix.@= user.id Ix.@= task.id
      ordered = Ix.toDescList (Proxy @Day) userTaskEvs
   in map (mkRow doc task.id) ordered

mkRow :: Document -> TaskId -> Evidence -> EvidenceRow
mkRow doc taskId ev =
  EvidenceRow
    { evidence = ev
    , assignment = ev.assignmentId >>= \aid -> Ix.getOne (doc.assignments Ix.@= aid)
    , status = rowStatus doc taskId ev
    }

-- | Per-evidence completion status for a single task. Looks up the
-- per-task evaluation map and returns 'TaskDone' iff every ability is
-- satisfactory. Returns 'TaskNotEvaluated' when no per-task data is
-- recorded (the row may still carry a note or remarks).
rowStatus :: Document -> TaskId -> Evidence -> TaskCompletionStatus
rowStatus doc taskId ev =
  case Map.lookup taskId ev.tasks of
    Nothing -> TaskNotEvaluated
    Just evals
      | Map.null evals -> TaskNotEvaluated
      | all isSatisfactory (Map.elems evals) -> TaskDone (mkEvidenceRef doc ev)
      | otherwise -> TaskNotDone (mkEvidenceRef doc ev)
  where
    isSatisfactory SelfReliant = True
    isSatisfactory SelfReliantWithSillyMistakes = True
    isSatisfactory _ = False

-- ============================================================================
-- View
-- ============================================================================

-- | Collapsible history section. Matches the per-solution disclosure
-- style: 'innerDisclosure' + icon/label header. The caller supplies:
--
-- * a note renderer (@'RichContent' -> 'M.View' m a@) — lets the
--   caller thread its 'FormulaCache' without the fragment touching
--   'SyncContext';
-- * an optional per-row entity-menu renderer — lets the caller mount
--   a 'Component.EntityMenu' with its own 'SyncContext' when needed;
-- * the set of expanded task ids and a toggle-action lifter so the
--   enclosing component owns the expansion state.
historySection
  :: (RichContent -> M.View m a)
  -- ^ Note renderer (caller supplies to avoid 'SyncContext' dep).
  -> (TaskId -> EvidenceRow -> M.View m a)
  -- ^ Per-row entity-menu slot. Return 'mempty'-equivalent (e.g.
  -- 'Layout.empty') to omit.
  -> Set TaskId
  -- ^ Task ids whose history is collapsed. History is shown by
  -- default (rarely more than one entry), so callers pass an
  -- 'Set.empty' at init and add ids as the user hides sections.
  -> (TaskId -> a)
  -- ^ Toggle-history action constructor.
  -> TaskId
  -> [EvidenceRow]
  -> M.View m a
historySection renderNote rowMenu collapsed onToggle taskId rows
  | null rows = Layout.empty
  | otherwise =
      let isExpanded = not (Set.member taskId collapsed)
       in Disclosure.innerDisclosure (onToggle taskId) $
            Disclosure.contents
              (Disclosure.titleIconText Icon.IcnEvidence (C.translate' C.LblTaskEvaluationHistory))
              isExpanded
              ( MH.div_
                  [class_ "border rounded-md divide-y divide-border"]
                  (map (viewHistoryRow renderNote rowMenu taskId) rows)
              )
              []

-- | Single history row. Layout:
--
-- @
-- [status]  vom DATE   [assignment title ─────── flex-grow ──────] [menu]
--                      [remark badges (when any)]
--                      [note (when any)]
-- @
--
-- Status icon falls back to a neutral "missing" dash when the
-- evidence recorded no abilities for this task (the task was in scope
-- for the evaluation but explicitly not done). The dash carries no
-- good/bad judgement — whether a miss is acceptable depends on why.
viewHistoryRow
  :: (RichContent -> M.View m a)
  -> (TaskId -> EvidenceRow -> M.View m a)
  -> TaskId
  -> EvidenceRow
  -> M.View m a
viewHistoryRow renderNote rowMenu taskId row =
  let ev = row.evidence
      mNote = Map.lookup taskId ev.taskNotes
      remarks = Map.findWithDefault Set.empty taskId ev.taskRemarks
   in MH.div_
        [class_ "grid grid-cols-[auto_1fr] gap-x-3 gap-y-0.5 items-baseline px-2 py-1.5"]
        $ [leftCell row, titleRow rowMenu taskId row]
            <> (if Set.null remarks then [] else [gridFiller, remarksRow remarks])
            <> (case mNote of
                  Just note | note /= mempty -> [gridFiller, noteBlock renderNote note]
                  _ -> [])

-- | Empty first-column cell that preserves alignment for secondary
-- rows (remarks, notes) so they indent under the assignment title.
gridFiller :: M.View m a
gridFiller = MH.span_ [] []

-- | An inline-level span so its baseline is the text baseline, which
-- the outer grid uses for row alignment. The status icon is rendered
-- 'inline-block' with 'align-middle' so it visually centers with the
-- text without breaking baseline alignment of the outer cell.
leftCell :: EvidenceRow -> M.View m a
leftCell row =
  MH.span_
    [class_ "text-sm text-muted-foreground whitespace-nowrap"]
    [ statusIcon row.status
    , M.text (" " <> C.translate' C.LblEvaluatedOn <> " " <> C.formatDay row.evidence.date)
    ]

-- | Checkbox-style status icon: a circle that is either empty
-- (missing / not yet decided), has a check (done), or has a cross
-- (not done). The empty-circle case carries no good/bad connotation —
-- whether an absence in an evaluation is acceptable depends on why.
statusIcon :: TaskCompletionStatus -> M.View m a
statusIcon status =
  let (icn, cs) = case status of
        TaskNotEvaluated -> (Icon.IcnCircle, Open)
        TaskDone _ -> (Icon.IcnApply, Done)
        TaskNotDone _ -> (Icon.IcnProgress, InProgress)
   in Icon.icon
        [class_ ("w-4 h-4 inline-block align-middle " <> textClass' (completionPalette cs))]
        icn

titleRow
  :: (TaskId -> EvidenceRow -> M.View m a)
  -> TaskId
  -> EvidenceRow
  -> M.View m a
titleRow rowMenu taskId row =
  MH.div_
    [class_ "flex items-center gap-2"]
    [ MH.div_ [class_ "flex-1 min-w-0"] [assignmentLabel row]
    , MH.div_ [class_ "shrink-0"] [rowMenu taskId row]
    ]

assignmentLabel :: EvidenceRow -> M.View m a
assignmentLabel row = case row.assignment of
  Just a ->
    let AssignmentName name = a.name
     in MH.span_ [class_ "font-medium truncate"] [M.text (ms name)]
  Nothing ->
    MH.span_
      [class_ "italic text-muted-foreground truncate"]
      [M.text (C.translate' (C.LblActivityTypeDescription row.evidence.activityType))]

remarksRow :: Set TaskRemark -> M.View m a
remarksRow remarks =
  Layout.hFlow (Layout.gapMicro <> Layout.crossCenter) $
    map (Badge.outline . Badge.badgeLabel . C.LblTaskRemark) (Set.toAscList remarks)

noteBlock :: (RichContent -> M.View m a) -> RichContent -> M.View m a
noteBlock renderNote note =
  MH.div_
    [class_ "prose prose-stone prose-sm max-w-none"]
    [renderNote note]

