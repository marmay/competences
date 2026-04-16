# TODO

## Frontend

- [ ] **Migrate PrintEngine modal to WindowManager** — `PrintEngine/Modal.hs` calls `modalFrame` directly, bypassing the window manager stack. `ViewerDetail.hs` manages the modal state/lifecycle manually via `Maybe PrintModalModel`. To migrate: extract into a proper `M.Component` and open via `openFramedModal`. Non-trivial due to tight coupling with ViewerDetail (shared task rendering, measurement container, post-print actions).
- [ ] **Migrate remaining View.Text imports to View.Typography** — Several components still import `View.Text` instead of `View.Typography` for heading/paragraph primitives.

### Entity menus

- [ ] **LockButton for assignment edit** — Assignment entity menu still uses a simple click for Edit (due to draft routing complexity). Should use LockButton with edit/steal like Task/Resource/LessonNotes.

### Assignment refactor

- [ ] **Extra tasks in evaluator** — Allow teachers to add tasks beyond the assignment's task list when evaluating
- [ ] **AssignmentKind enum** — Replace `studentIds` role with explicit `WholeClass | Individual` enum
- [ ] **Self-assigned assignments** — Third `AssignmentKind` variant for student-initiated work

### TODO items (planned feature)

- [ ] **TODO entity** — Per-class todo list with: text, optional due date, optional student list, optional task, optional lesson, state (Pending|Doing|Done). Created from: task assessment notes in the evaluator, lesson notes, a dedicated TODO list page. Later: orgmode aggregation for cross-class integration.
- [ ] **Evidence date → datetime** — Change `Evidence.date` from `Day` to `UTCTime` (default end-of-day). Current workaround: `>` instead of `>=` in submission open check treats same-day as reviewed.

## Common / Architecture

- [ ] **Reconsider `mkEntityCommandContext` abstraction** — The complex cases (Tasks, DraftTasks, Lessons) diverge significantly from the standard pattern and lead to code duplication. Evaluate whether a different abstraction would reduce boilerplate.
- [ ] **Deduplicate Tasks.hs / DraftTasks.hs** — Shared logic between these command modules (reduced after TaskGroup removal but still duplicated).
- [ ] **Extract shared AffectedUsers helpers to Command.Common** — Currently scattered across individual command modules.

## Backend

- [ ] **Split Database.hs** — 722 lines mixing migrations, queries, and snapshot logic. Separate into focused modules.
- [ ] **Split WebSocket.hs** — 380 lines mixing auth, message handling, and file upload. Separate concerns.
- [ ] **Add backend tests** — Currently `main = pure ()`.

## Tests

- [ ] **Fix flaky P12 property test** — `P12: monotonicity — easier level mastery <= harder level mastery` in `competences-common-test` occasionally fails. Investigate whether the property is too strict or the generator produces edge cases that violate the expected ordering.
- [ ] **Add command handler tests** — Currently zero test coverage for command handlers.
