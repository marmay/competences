# TODO

## Frontend

- [ ] **Migrate PrintEngine modal to WindowManager** — `PrintEngine/Modal.hs` calls `modalFrame` directly, bypassing the window manager stack. `ViewerDetail.hs` manages the modal state/lifecycle manually via `Maybe PrintModalModel`. To migrate: extract into a proper `M.Component` and open via `openFramedModal`. Non-trivial due to tight coupling with ViewerDetail (shared task rendering, measurement container, post-print actions).
- [ ] **Migrate remaining View.Text imports to View.Typography** — Several components still import `View.Text` instead of `View.Typography` for heading/paragraph primitives.

### Task system remaining (from Phase 1)

- [ ] **Competence fields in task editor** — Add primary/secondary competence selection to task editor
- [ ] **TaskGroupEditor** — Component for managing task groups and subtasks
- [ ] **Resource view integration** — Show tasks in competence grid resource view
- [ ] **Evidence task selection** — Replace text input with task selector in evidence editor

### Assignment refactor

- [ ] **Extra tasks in evaluator** — Allow teachers to add tasks beyond the assignment's task list when evaluating
- [ ] **AssignmentKind enum** — Replace `studentIds` role with explicit `WholeClass | Individual` enum
- [ ] **Self-assigned assignments** — Third `AssignmentKind` variant for student-initiated work

## Common / Architecture

- [ ] **Reconsider `mkEntityCommandContext` abstraction** — The complex cases (Tasks, DraftTasks, Lessons) diverge significantly from the standard pattern and lead to code duplication. Evaluate whether a different abstraction would reduce boilerplate.
- [ ] **Deduplicate Tasks.hs / DraftTasks.hs** — ~150 LOC of shared logic between these command modules.
- [ ] **Extract shared AffectedUsers helpers to Command.Common** — Currently scattered across individual command modules.

## Backend

- [ ] **Split Database.hs** — 722 lines mixing migrations, queries, and snapshot logic. Separate into focused modules.
- [ ] **Split WebSocket.hs** — 380 lines mixing auth, message handling, and file upload. Separate concerns.
- [ ] **Add backend tests** — Currently `main = pure ()`.

## Tests

- [ ] **Fix flaky P12 property test** — `P12: monotonicity — easier level mastery <= harder level mastery` in `competences-common-test` occasionally fails. Investigate whether the property is too strict or the generator produces edge cases that violate the expected ordering.
- [ ] **Add command handler tests** — Currently zero test coverage for command handlers.
