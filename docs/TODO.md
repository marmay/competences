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
- **1.9.x — Q&A**: students raise questions, teachers answer.
- **Consolidation phase** (post-1.9): clean up and streamline the
  application.

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

- [ ] **Migrate `Map Level _` JSON to the object form (batched)** — `Level`'s default `FromJSONKey`/`ToJSONKey` instances encode every `Map Level v` as an array of `[key, value]` pairs (e.g. `[["BasicLevel", v]]`) instead of the human-friendly object `{ "BasicLevel": v }`. The exchange format is already migrated (`Exchange.Types` uses `levelMapToJSON` / `parseLevelMap` from `Document.Competence`, which writes the object form and reads *both* forms). Remaining persisted sites still on the legacy array form: `Competence.levels`, the legacy `Competence.levelDescriptions` fallback, and `CompetencePatch.levels` (and any other `Map Level _` reachable from a snapshot or command). A single global `FromJSONKey`/`ToJSONKey` switch can't be made backward-compatible — the `Map` parser commits to one wire shape per key-function constructor — so migrate per field: swap each `ToJSON` to `levelMapToJSON` and, in the same change, point its `FromJSON` at `parseLevelMap` (tolerant of old snapshots/command log). Keep `FromJSONKey Level` on its default encoding while any array-form data may still be read (the `parseLevelMap` array fallback depends on it). Once every site is migrated and old snapshots are rewritten, the array fallback and the default key instances can be dropped.
- [ ] **File attachments in import/export** — Same-server imports already round-trip attachment metadata via the shared CAS (the `sha256` from `ExchangeAttachment` resolves directly). Cross-server imports currently leave attachments dangling. Plan: extend the payload with a resolver hint (source URL + short-lived access token) so the receiving instance can fetch the blob from the source CAS on apply. The fetch can run server-side (one CAS to another) or client-side; either way the YAML stays compact for the common case. Embedded base64 content stays available as a second path for use cases where no source CAS exists — notably LLM-generated YAML, where the LLM produces the bytes directly. Only matters when teachers start sharing across instances; expected timeline is a few months out.
- [ ] **Reconsider `mkEntityCommandContext` abstraction** — The complex cases (Tasks, DraftTasks, Lessons) diverge significantly from the standard pattern and lead to code duplication. Evaluate whether a different abstraction would reduce boilerplate.
- [ ] **Deduplicate Tasks.hs / DraftTasks.hs** — Shared logic between these command modules (reduced after TaskGroup removal but still duplicated).

## Backend

- [ ] **Split Database.hs** — 722 lines mixing migrations, queries, and snapshot logic. Separate into focused modules.
- [ ] **Split WebSocket.hs** — 380 lines mixing auth, message handling, and file upload. Separate concerns.
- [ ] **Add backend tests** — Currently `main = pure ()`.
- [ ] **Shared OAuth callback service (before next school year, 2026/27)** — Extract `oauthCallbackHandler` plus JWT generation into a small standalone service mounted at the shared domain root (e.g. `mathe.example.com/auth/`). One Azure app registration, one shared JWT secret, one redirect URI for the whole tenant. Each instance drops its `/oauth/callback` route and 302s unauthenticated requests to the auth service with `?return=<url>`. Per-instance authorization is unaffected — `findUserByEmail` still gates access via the local user table, so a shared JWT carrying email is safe to accept across instances. Optional follow-on: a `.<shared-domain>` session cookie mapping `session_id → email` so subsequent logins to other instances short-circuit the OAuth round-trip (real SSO across classes for teachers). Subdomain-mode instances cannot share the session cookie and would either keep their own callback or migrate to the shared domain. Motivation: spinning up a new class becomes "add an instance entry to the NixOS module" instead of "add an instance entry + register an Azure app + set redirect URI + put credentials in agenix".

## Tests

- [ ] **Fix flaky P12 property test** — `P12: monotonicity — easier level mastery <= harder level mastery` in `competences-common-test` occasionally fails. Investigate whether the property is too strict or the generator produces edge cases that violate the expected ordering.
- [ ] **Add command handler tests** — Currently zero test coverage for command handlers.
