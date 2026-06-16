# TODO

## Roadmap

- **1.6.0 — Toolchain refresh**
  - [x] Upgrade to upstream `haskell-miso` 1.9.0. Fork retired —
    component-lifecycle fixes are upstreamed; the `propagateChildren`
    safe-lookup patch became unnecessary after upstream restructured
    component bookkeeping.
  - [ ] Build the WASM binary via haskell.nix (it now supports this)
    instead of the current shell-script flow.
- **1.7.x — Student-driven assignments**: improvements to handing in
  assignments, plus handing in tasks without an assignment.
- **1.8.x — TODO tracking**: capture todos from class and from
  corrections (see existing TODO entity entry below).
- **Summer 2026 hardening** (calendar-bound, not release-bound): see
  dedicated section below. The visibility unification must land
  before 1.9.x.
- **1.9.x — Q&A**: students raise questions, teachers answer.
- **Consolidation phase** (post-1.9): clean up and streamline the
  application.

## Summer 2026 hardening (before school year 2026/27)

From the architecture review of 2026-06-10. The first two remove the only
silent data-loss paths; the third must land before Q&A (1.9.x) gives
students write access.

- [ ] **Fail-stop the command processor on persist failure** —
  `processorLoop` runs under a bare `forkIO`
  (`CommandProcessor.hs:102`) and `DB.saveCommandWithAudience` has no
  exception handler. A failed DB write kills the processor thread
  alone: the server stays up, the bounded TBQueue fills, submitters
  block — a zombie with in-memory state ahead of the DB, and the
  crash/restart that the reconnect-rollback model relies on never
  happens. Fix: catch around the persist, log, terminate the process.
  The ack-before-persist design itself stays as is (conscious
  trade-off; clients roll back via `performSync` → unknown CommandId
  → snapshot).
- [ ] **Make dropped commands loud at replay** — The replay list
  comprehension in `Database.hs:330-335` pattern-filters on `Success`
  / `Right`, silently discarding any command that no longer parses.
  Count failures and refuse to start (or at minimum log each one)
  when commands *after the latest snapshot* fail to parse. Replaces
  the implicit "never break log and snapshots at the same time"
  discipline with a checked invariant.
- [ ] **Golden-file corpus for command serializations** — Store
  serialized commands from each historical format; CI test asserts
  they all still parse. Append whenever a format changes. Companion
  to the loud-replay check: that one detects breakage at startup,
  this one before merge.
- [ ] **Unify per-entity visibility into one source of truth** —
  "Who sees what" is currently computed in four uncoordinated
  places: `projectDocument` (student snapshot), the per-entity
  `CommandAudience` closures scattered across `Command/*.hs`
  (broadcast filtering), `clientCommands` in `CommandProcessor.hs`
  (hand-written synthetic Create/Delete for assignment `studentIds`
  changes), and the authorization stack (`isAuthorized` WebSocket
  gate + `teacherOnly` wrappers + ad-hoc ownership checks in
  `Submissions.hs`). Nothing forces these to agree; the assignment
  special case exists because two of them already diverged once.
  Derive projection, audience, and the diff-rewriting from a single
  per-entity visibility function. **Prerequisite for Q&A** — student
  write access multiplies the cost of a missed `teacherOnly` or a
  projection/audience mismatch.
- [ ] **Centralize referential integrity / cascades** — Delete
  validation and cascading are per-handler folklore: Task checks
  evidences, Assignment checks lessons/evidences, but Competence,
  CompetenceGrid, and User deletion check nothing, and only
  MesoPlan/Lesson cascade. Orphaned IDs (e.g. stale `TaskId`s in
  `Evidence.tasks`) corrupt silently and replay reproducibly.
  Declare per-entity references in one place and derive both
  delete-validation and cascades from it. Overlaps with the
  visibility unification above — "who is affected" and "what
  references what" are closely related queries.

## Frontend

- [ ] **Migrate PrintEngine modal to WindowManager** — `PrintEngine/Modal.hs` calls `modalFrame` directly, bypassing the window manager stack. `ViewerDetail.hs` manages the modal state/lifecycle manually via `Maybe PrintModalModel`. To migrate: extract into a proper `M.Component` and open via `openFramedModal`. Non-trivial due to tight coupling with ViewerDetail (shared task rendering, measurement container, post-print actions).
- [ ] **Refactor RichContent component to receive resolver state via the model** — `renderRichTextWithResolver` (in `Component/RichContent.hs`) mounts the rich-content as a Miso component keyed by the document hash. The `FileResolver` is baked into the component at mount time, so subsequent parent re-renders with a different resolver are silently ignored (Miso reuses the keyed instance). The print engine works around this by appending a `T.pack . show $ tcs.imageSettings` string to the component key, forcing a full re-mount on every slider tick — works for the visible wrapper styling, but the re-mount **tears down every descendant component** including the `filePreviewComponent` instances that render images. Each remount restarts the async `LoadFile` → `downloadFile` → data-URL chain (`Component/FilePreview.hs:43-70`), so embedded images briefly fall back to the small loading placeholder. This raced with `doRemeasure`'s height read in 1.6.5 and corrupted page grouping; 1.6.6 papered over it by polling images for completion before measuring (see `allMeasureImagesLoaded`), but the root cause is still the per-tick remount churn. Proper fix: expose an action that updates the resolver on the existing component (or pass the resolver-state via bindings) so parent state changes propagate without remounting. Removes the `resolverKey` parameter, the `T.pack . show $ imgSettings` hack at the call site, and the image-wait poll in `doRemeasure`.
- [ ] **Migrate remaining View.Text imports to View.Typography** — Several components still import `View.Text` instead of `View.Typography` for heading/paragraph primitives.
- [ ] **Integrate "Add" buttons into the selector control** — The Add task / Add resource buttons in `addableSearchSelectEditorField` currently sit in a separate row below the SearchSelect. Place them inline with the selector's own controls, to the left of "Alle abwählen", so they read as part of the selector rather than a detached row.
- [ ] **Persist assignment evaluator pin state** — The assignment evaluator pin does not preserve its edit-time state across minimize/restore. Wire it into the standard `pinSaveStates` binding, same pattern as the editor-framework pin editors and the now-fixed lesson pin editor.
- [ ] **Inline "Add task" / "Add resource" in the phase items editor** — Currently teachers pick from existing entities (matching `assignmentsSection`). Either re-add the spawn flow inside the hand-written `LessonPinEditor`, or redesign the lesson editor on the Editor framework. Decide when revisited.
- [ ] **Teacher list page (Schulübung, grouped by meso plan)** — Present iteration: teachers preview via the Lesson `EntityMenu` pin. A dedicated grouped list page is a post-MVP refinement.
- [ ] **Assignment status icon in Schulübung rows** — Surface submission / evaluation status per assignment (overdue, submitted, corrected). Requires an assignment-level status query.
- [ ] **Reorder UX within a phase's items list** — Current editor preserves insertion order; no drag-reorder yet.
- [ ] **Per-phase file attachments** — Not supported; teachers use a dedicated Resource for media-bearing content. Revisit only if friction proves real.
- [ ] **Selector binding direction is implicit** — `Component/Selector/EnumSelector.hs` uses bidirectional `<--->`; the rest of the selector family (`List`, `CustomSelect`, `MultiStageSelector`, `LessonSelector`, `SearchSelect`) goes through `mkSelectorBinding` which is hardcoded to uni `<---`. The current EnumSelector use sites happen to need `<--->` (so external parent-state changes flow back into the dropdown, e.g. on `LoadStudentEvidence`), so things work — but the choice is invisible to the caller. Introduce a `data Direction p a = Uni (Lens' p a) | Bi (Lens' p a)` (or similar) so each call site explicitly opts into uni- or bi-directional binding instead of inheriting whichever the component happened to pick. Keeps the reactive graph simple by default, lets the rare cases declare the bidirectional dependency at the use site.

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

- [ ] **`Map Level _` JSON object-form migration — follow-ups** — `Document.Competence` defines `OVERLAPPING` `ToJSON`/`FromJSON` instances for `Map Level a` that write the human-friendly object form (`{ "BasicLevel": v }`) and read *both* it and the legacy array-of-pairs form. This applies to every `Map Level _` (exchange payloads, `Competence.levels`, the legacy `levelDescriptions` fallback, `CompetencePatch.levels`, …) — new writes are object form, old snapshots/command logs still parse via the array fallback. Remaining cleanup, deferrable: (1) once all live snapshots have been rewritten in the object form, the `Array` fallback in the `FromJSON` instance and `Level`'s now-unused default `FromJSONKey`/`ToJSONKey` instances can be removed; (2) note the overlap caveat — a *polymorphic* `ToJSON (Map k v)`/`FromJSON (Map k v)` use site applied at `k ~ Level` resolves to the generic (array) instance, not this one (instance selection happens where the constraint is discharged). None exist today; keep an eye out. (3) Object-form snapshots are not readable by pre-migration binaries, so a rollback past this point would need the old array form.
- [ ] **File attachments in import/export** — Same-server imports already round-trip attachment metadata via the shared CAS (the `sha256` from `ExchangeAttachment` resolves directly). Cross-server imports currently leave attachments dangling. Plan: extend the payload with a resolver hint (source URL + short-lived access token) so the receiving instance can fetch the blob from the source CAS on apply. The fetch can run server-side (one CAS to another) or client-side; either way the YAML stays compact for the common case. Embedded base64 content stays available as a second path for use cases where no source CAS exists — notably LLM-generated YAML, where the LLM produces the bytes directly. Only matters when teachers start sharing across instances; expected timeline is a few months out.
- [ ] **Reconsider `mkEntityCommandContext` abstraction** — The complex cases (Tasks, DraftTasks, Lessons) diverge significantly from the standard pattern and lead to code duplication. Evaluate whether a different abstraction would reduce boilerplate.
- [ ] **Deduplicate Tasks.hs / DraftTasks.hs** — Shared logic between these command modules (reduced after TaskGroup removal but still duplicated).

## Backend

- [ ] **Warn teachers about an expiring/expired OAuth client secret** — A
  silently expired Azure client secret (AADSTS7000222) took down login in
  production on 2026-06-16 (no code change, no warning — the secret just
  lapsed on its date). Make the server check its own OAuth credential
  health periodically (on startup and once a day) and surface the result
  to teachers as a permanent warning banner ("login will stop working —
  ask your administrator to renew the OAuth secret") so availability for
  students is never interrupted by a lapse that only an admin can fix.
  Check via the secret's `endDateTime` from Graph (warn ~30 days ahead),
  or, failing that, a `client_credentials` probe that detects
  AADSTS7000222 (detects only after expiry — prefer the Graph date).
  Banner is teacher-only (students can't act on it). Related: the bad
  `_ -> "No access_token in response"` branch in `exchangeCodeForToken`
  hid Microsoft's real `error_description` and turned this into a blind
  guessing game — return/log the actual error too. Folds naturally into
  the auth-service consolidation below (one app registration = one
  credential to monitor).
- [ ] **Split Database.hs** — 722 lines mixing migrations, queries, and snapshot logic. Separate into focused modules.
- [ ] **Split WebSocket.hs** — 380 lines mixing auth, message handling, and file upload. Separate concerns.
- [ ] **Add backend tests** — Currently `main = pure ()`.
- [ ] **Shared auth service + Microsoft Teams integration (Stage 1 before next school year, 2026/27)** — Superseded and expanded by the full plan in [teams-integration-plan.md](teams-integration-plan.md) (2026-06-12). Summary: standalone `competences-auth` identity provider (one Azure app registration, browser OAuth + Teams SSO), front-channel exchange — 60 s Ed25519-signed identity assertion → instance `POST /api/login` → instance-minted 24h HS256 session JWT (WebSocket auth untouched); Teams configurable channel tab per class Team. Staged: (1) shared auth service browser-only, (2) Teams SSO + shell + real CSP headers, (3) manifest/config page/org catalog + one-class pilot, (4) mobile evaluation.

## Tests

- [ ] **Fix flaky P12 property test** — `P12: monotonicity — easier level mastery <= harder level mastery` in `competences-common-test` occasionally fails. Investigate whether the property is too strict or the generator produces edge cases that violate the expected ordering.
- [ ] **Add command handler tests** — Currently zero test coverage for command handlers.
