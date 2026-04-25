# TODO

## Frontend

- [ ] **Migrate PrintEngine modal to WindowManager** — `PrintEngine/Modal.hs` calls `modalFrame` directly, bypassing the window manager stack. `ViewerDetail.hs` manages the modal state/lifecycle manually via `Maybe PrintModalModel`. To migrate: extract into a proper `M.Component` and open via `openFramedModal`. Non-trivial due to tight coupling with ViewerDetail (shared task rendering, measurement container, post-print actions).
- [ ] **Migrate remaining View.Text imports to View.Typography** — Several components still import `View.Text` instead of `View.Typography` for heading/paragraph primitives.
- [ ] **Integrate "Add" buttons into the selector control** — The Add task / Add resource buttons in `addableSearchSelectEditorField` currently sit in a separate row below the SearchSelect. Place them inline with the selector's own controls, to the left of "Alle abwählen", so they read as part of the selector rather than a detached row.
- [ ] **Drop mandatory competence on resources** — Creating/editing a resource currently requires at least one competence. This constraint comes from the long-since-removed competence-grid edit path and no longer matches how resources are authored; resources should be valid with an empty competence list.
- [ ] **Persist assignment evaluator pin state** — The assignment evaluator pin does not preserve its edit-time state across minimize/restore. Wire it into the standard `pinSaveStates` binding, same pattern as the editor-framework pin editors and the now-fixed lesson pin editor.
- [ ] **Release B: drop LessonNotes entirely** — One release after the lesson-records migration ships, once production snapshots all have `lessonNotesMigrated = True`. Cleanup inventory:
    - **common/** — `Document/LessonNotes.hs` (entire module), `Command/LessonNotes.hs` (entire module); strip `Command.LessonNotes`, `Command.MigrateLessonNotesIntoLessons`, `MigrationPlan`, and `validateLessonNotesMigration` from `Command.hs`; drop `Document.lessonNotes` and `Document.lessonNotesMigrated` fields and their projection exclusions in `Document.hs`; drop `LessonNotesLock` from `Document/Lock.hs`; drop `Lesson.notes` and `LessonPhase.notes` legacy fields from `Document/Lesson.hs` (already cleared by the migration); drop `Query/DefaultSelection.defaultLessonNotes`.
    - **backend/** — `checkLessonNotesMigration` + `buildAndDispatch` in `backend/app/Main.hs`, and the startup-gate call-site.
    - **frontend/** — `Component/LessonNotes/*` (Detailed, PinEditor, ViewerDetail), `Component/Selector/LessonNotesSelector.hs`, `Page/LessonNotes.hs`; `ManageLessonNotes` route in `Page.hs`; the `Archiv` teacher-nav group in `View/NavBar.hs` (LessonNotes was its only entry); `PinLessonNotesViewer` constructor in `SyncContext/SyncDocument.hs` and its handler in `SyncContext/LockWatching.hs`; `LessonNotesLock` handling in `SyncContext/SyncDocument.hs` and `Component/Entity/Assembly.hs`.
    - **translation labels** — `LblLegacyLessonNotes`, `LblLessonNotesEntries`, `LblFilterLessonNotes`, `LblNewLessonNotes`, `LblLessonNotesDate`, `LblLessonNotesTitle`, `LblLessonNotesResources`, `LblLessonNotesItems`.
    - **sanity check** — `grep -rn 'LessonNotes\|lessonNotes' --include='*.hs'` should only match the removal diff itself after.
- [ ] **Drop `Lesson.resources` legacy field** (also Release B) — see the separate entry below; bundle with the LessonNotes removal since both touch `Lesson`.
- [ ] **Inline "Add task" / "Add resource" in the phase items editor** — Currently teachers pick from existing entities (matching `assignmentsSection`). Either re-add the spawn flow inside the hand-written `LessonPinEditor`, or redesign the lesson editor on the Editor framework. Decide when revisited.
- [ ] **Teacher list page (Schulübung, grouped by meso plan)** — Present iteration: teachers preview via the Lesson `EntityMenu` pin. A dedicated grouped list page is a post-MVP refinement.
- [ ] **Assignment status icon in Schulübung rows** — Surface submission / evaluation status per assignment (overdue, submitted, corrected). Requires an assignment-level status query.
- [ ] **Reorder UX within a phase's items list** — Current editor preserves insertion order; no drag-reorder yet.
- [ ] **Drop `Lesson.resources` legacy field** — Unused by the current editor; cleanup during or after release B.
- [ ] **Per-phase file attachments** — Not supported; teachers use a dedicated Resource for media-bearing content. Revisit only if friction proves real.

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

- [ ] **File attachments in import/export** — Same-server imports already round-trip attachment metadata via the shared CAS (the `sha256` from `ExchangeAttachment` resolves directly). Cross-server imports currently leave attachments dangling. Plan: extend the payload with a resolver hint (source URL + short-lived access token) so the receiving instance can fetch the blob from the source CAS on apply. The fetch can run server-side (one CAS to another) or client-side; either way the YAML stays compact for the common case. Embedded base64 content stays available as a second path for use cases where no source CAS exists — notably LLM-generated YAML, where the LLM produces the bytes directly. Only matters when teachers start sharing across instances; expected timeline is a few months out.
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
