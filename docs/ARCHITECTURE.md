# Architecture

This document describes the core architectural patterns of the competences tracking application.

## Core Architecture Pattern: Command-Driven Document Synchronization

The application uses a command-sourcing pattern where:
1. All state is held in a `Document` (from `Competences.Document`)
2. Changes are expressed as `Command`s (from `Competences.Command`)
3. Commands are handled via `handleCommand` which validates and applies changes
4. The frontend maintains a `SyncDocument` that tracks local and remote document state

## Document Projection and Access Control

**Security Model**: Not all users have access to all data. The backend projects the full `Document` to a user-specific view based on their identity.

**Projection Rules**:

Students see (based on their user identity):
- **Users**: Only themselves (not other users)
- **CompetenceGrids**: All competence grids
- **Evidences**: Only evidences where they are a participant (their ID is in `evidence.userIds`)
- **Other entities**: Filtered similarly based on relevance to the student

Teachers see:
- **Full document**: No filtering, all entities visible

**Implementation**:
```haskell
-- In common/lib/Competences/Document.hs
projectDocument :: User -> Document -> Document
-- If user.role == Teacher: return full document
-- If user.role == Student: filter entities based on user.id
--   - Uses UserId index on Evidence for efficient filtering
```

**Status**: ✅ Implemented
- Location: `common/lib/Competences/Document.hs`
- Students see: own User, own Evidences (via UserId index), all CompetenceGrids/Competences/Resources/Templates
- Locks filtered to show only locks on accessible entities

**InitialSnapshot**:
- When a user connects, backend sends `InitialSnapshot` with **projected document**
- This ensures students never receive data they shouldn't see
- Frontend's `remoteDocument` is always a projection (full for teachers, filtered for students)

**Status**: ✅ Implemented
- Location: `backend/lib/Competences/Backend/WebSocket.hs:71-74`
- Calls `projectDocument user doc` before sending `InitialSnapshot`

**AffectedUsers and Broadcasting**:
- When a command is applied, `handleCommand` returns `AffectedUsers`
- `AffectedUsers` = list of users whose **projected document** changes
- Only affected users receive the `ApplyCommand` broadcast
- This is efficient: users only get updates relevant to them

**Examples**:
- Student creates their own evidence → `AffectedUsers = [student, all teachers]`
  - Student's projection gains the evidence
  - Teachers' projections gain the evidence
  - Other students don't see it (not in their projection)
- Teacher modifies Student A's evidence → `AffectedUsers = [Student A, all teachers]`
  - Student A's projection changes
  - All teachers' projections change
  - Other students unaffected
- Teacher modifies a competence grid → `AffectedUsers = [all users]`
  - Everyone's projection includes competence grids

**Deduplication**:
- Backend deduplicates `AffectedUsers` before broadcasting
- Prevents sending the same command multiple times to the same user
- Uses `Map.keys $ Map.fromList [(uid, ()) | uid <- userIds]`

**Authorization**:
- **Current**: All commands require Teacher role (validated server-side)
- **Future**: Student-specific commands will be added with per-command authorization
- Backend validates user has permission to execute each command
- Frontend can make optimistic updates, but backend is authoritative

**Status**: ✅ Implemented
- Location: `backend/lib/Competences/Backend/WebSocket.hs:95-99`
- Checks `user.role /= Teacher` before processing commands
- Sends `CommandRejected` with "Only teachers can execute commands" for non-teachers

**Property-Based Testing**:
- Verify `AffectedUsers` correctness with QuickCheck properties:
  ```haskell
  -- AffectedUsers must match users whose projection changes
  prop_affectedUsersMatchProjection :: User -> Command -> Document -> Property
  prop_affectedUsersMatchProjection sender cmd doc =
    case handleCommand sender.id cmd doc of
      Right (doc', AffectedUsers affected) ->
        let changedProjections = filter
              (\u -> projectDocument u doc /= projectDocument u doc')
              allUsers
        in sort affected === sort (map (.id) changedProjections)
  ```
- Other properties: teacher projections are identity, student projections are subsets, etc.

**Status**: ⏸️ TODO
- Requires: QuickCheck dependency in `competences-common.cabal`
- Requires: `Arbitrary` instances for `Document`, `User`, `Command`, `Evidence`, etc.
- Suggested location: `common/test/ProjectionProperties.hs`
- **Note**: This is important for verifying that `AffectedUsers` calculations are correct across all command types

**Session Integrity**:
- User identity (id, role, name, office365Id) must not change during a session
- On reconnection, if user differs → error logged, session should terminate
- Future: `ConnectedUserInfo` projection for fields that CAN change without breaking session
- Current check in `Main.hs`: verifies `user.id` matches on reconnect

## Client-Server Communication Model

**HTTP + WebSocket Architecture:**
- HTTP endpoints handle OAuth flow and static file serving
- WebSocket handles all real-time data synchronization
- Single port serves both HTTP and WebSocket (via `websocketsOr`)

**HTTP Endpoints:**
- `GET /`: Redirects to Office365 login page
- `GET /oauth/callback?code=...`: Handles OAuth callback, generates JWT, serves frontend HTML
  - **Authentication**: Matches Office365 email to `office365Id` field in existing users
  - **No auto-creation**: Login fails if no matching user exists
  - **Email-based matching**: Uses `mail` or `userPrincipalName` from Microsoft Graph
- `GET /static/*`: Serves static WASM files (app.wasm, ghc_wasm_jsffi.js, index.js)

**WebSocket Connection:**
- Frontend connects with JWT in query parameter: `ws://host:port/?token=<jwt>`
- All data synchronization happens over this connection
- No REST endpoints for data operations
- Connection is stored in `SyncDocumentRef.webSocket` for sending commands

**Message Protocol** (defined in `Competences.Protocol`):

Client → Server:
- `SendCommand Command`: Send a command to be applied (sent one at a time)
- `KeepAlive`: Maintain connection (prevent timeout)

Server → Client:
- `InitialSnapshot Document User`: **Projected** document + authenticated user sent on connection
  - Document is projected based on user identity (full for teachers, filtered for students)
- `ApplyCommand Command`: Command to apply (echo or broadcast)
  - Only sent to users in `AffectedUsers` (those whose projection changes)
- `CommandRejected Command Text`: Command validation failed
- `KeepAliveResponse`: Acknowledge keep-alive

**Synchronization Flow:**

1. **Connection**:
   - Frontend connects, receives `InitialSnapshot Document User`
   - Document is **projected** based on user identity (filtered for students, full for teachers)
   - Sets `remoteDocument` and `localDocument` to projected document
   - Sets `connectedUser` from server (authoritative)
   - Stores WebSocket connection in `SyncDocumentRef.webSocket`

2. **Local Action**:
   - User performs action → generates `Command` → validated via `handleCommand`
   - If valid: added to `localChanges` → applied optimistically to `localDocument`
   - `trySendNextCommand` called to send if no command pending

3. **Sending Commands** (One at a Time):
   - If `pendingCommand == Nothing` && WebSocket connected && `localChanges` non-empty:
     - Move first command from `localChanges` to `pendingCommand`
     - Send via `SendCommand`
   - **Non-idempotent safe**: Commands sent at most once
   - **Ordered**: FIFO queue ensures correct sequencing

4. **Server Processing**:
   - Server validates **authorization** (currently: all commands require Teacher role)
   - Server validates and applies command using `handleCommand`
   - On success:
     - Command is **automatically saved to database** (via `updateDocument`)
     - Snapshot created if threshold reached (25 commands or 15 min + ≥1 command)
     - Broadcasts `ApplyCommand` to `AffectedUsers` (deduplicated) including sender
     - `AffectedUsers` = users whose projected document changes
   - On failure: sends `CommandRejected` to sender only

5. **Remote Update - Echo** (ApplyCommand matches `pendingCommand`):
   - Apply to `remoteDocument`
   - Clear `pendingCommand`
   - Replay `localChanges` on new `remoteDocument` to get `localDocument`
   - Call `trySendNextCommand` to send next queued command

6. **Remote Update - Broadcast** (ApplyCommand from another user):
   - Apply to `remoteDocument`
   - Keep `pendingCommand` unchanged
   - Replay `pendingCommand + localChanges` on new `remoteDocument` to get `localDocument`

7. **Command Rejection**:
   - If matches `pendingCommand`: clear it, remove from `localChanges`, try send next
   - If doesn't match: log warning (shouldn't happen)

8. **Reconnection**:
   - Receive `InitialSnapshot` with new server state
   - Clear `pendingCommand` (accept loss of in-flight command)
   - **Keep `localChanges`** (preserve user's work)
   - Replay `localChanges` on new `remoteDocument`
   - Call `trySendNextCommand` to resume sending

9. **Invariant**: `localDocument = remoteDocument + apply(pendingCommand) + apply(localChanges)`

**Command Ordering:**
- WebSocket guarantees message ordering within a connection
- Server applies commands in received order
- Frontend sends commands one at a time (queue in `localChanges`)
- Echo detection via `pendingCommand` match

## Key Modules

**Competences.Document** (`common/lib/Competences/Document.hs`):
- Central data model containing all entity types
- Uses `ixset-typed` for indexed collections of entities
- Entities: `User`, `Competence`, `CompetenceGrid`, `Evidence`, `Resource`, `Template`
- **Document Projection**: `projectDocument :: User -> Document -> Document`
  - Filters document based on user identity for access control
  - Teachers see full document, students see filtered view

**Competences.Command** (`common/lib/Competences/Command.hs`):
- Defines command types: `EntityCommand` (Create, Delete, Modify)
- `ModifyCommand`: Lock, Release (with optional before/after state)
- `handleCommand`: Pure function that validates and applies commands to documents
- Returns `Either Text (Document, AffectedUsers)` - the `AffectedUsers` determines who receives broadcasts
- Commands are designed to be mostly non-conflicting (especially student commands)
- `ReorderCompetence` expresses user intent clearly to minimize conflicts

**Competences.Frontend.SyncDocument** (`frontend/lib/Competences/Frontend/SyncDocument.hs`):
- Manages document state in the frontend
- **`remoteDocument`**: Server's authoritative **projected** state (filtered for students, full for teachers)
- **`localDocument`**: Computed as `remoteDocument + pendingCommand + localChanges` (always)
- **`pendingCommand`**: Command currently sent to server, awaiting acknowledgment (at most one)
- **`localChanges`**: Queue of commands not yet sent to server
- **`webSocket`**: MVar holding WebSocket connection for sending commands
- When server sends command: apply to `remoteDocument`, then replay `pendingCommand + localChanges` to recompute `localDocument`
- Commands that fail during replay are stripped (indicates they're already applied or conflicted)
- **Note**: Replay works correctly on projected documents because users only issue commands about entities in their projection
- Provides subscription mechanism via `ChangedHandler`s
- Wrapped in `SyncDocumentRef` (MVar-based) for concurrent access

**Key Functions**:
- `modifySyncDocument`: Add command to `localChanges`, call `trySendNextCommand`
- `trySendNextCommand`: Move first `localChanges` to `pendingCommand` and send if no pending
- `applyRemoteCommand`: Handle server echo or broadcast, replay changes
- `rejectCommand`: Handle rejected command, try send next
- `setWebSocket`: Set WebSocket connection (called after connecting)
- `setSyncDocument`: Set remote document (handles both initial connection and reconnection)

**Competences.Frontend.App** (`frontend/lib/Competences/Frontend/App.hs`):
- Main application entry point using Miso framework
- Router-based navigation between views
- User role-based UI (Teacher vs Student views)
- Views: CompetenceGrid editing/viewing, Evidence editing, Statistics, User management

## Assignments (replacing Templates)

**Current Status**: Template exists in codebase but not used productively. Will be replaced by Assignment in Phase 2.

**Purpose**: Assignments make creating evidences easier for multi-task assignments. They address two pain points:
1. Tedious to manually select competences and tasks for each student
2. Difficult to mentally aggregate observations across multiple tasks

**Design Overview**:

Assignments are concrete instances given to specific students on specific dates:
- A collection of tasks (with rich competence metadata)
- Assigned to specific students
- Assignment and due dates
- Default properties for resulting evidence (activity type, social form)

**Workflow**:
1. **Assignment Creation**: Teacher creates assignment (date, students, tasks)
2. **Student Visibility**: Students see their assignments (phased implementation - Phase 4)
3. **Evaluation**: Teacher uses AssignmentEvaluation (ephemeral UI state) to assess task-by-task
4. **Automatic Aggregation**: System aggregates assessments per competence level (using max)
5. **Review and Override**: Teacher reviews aggregated results, can modify if automatic aggregation doesn't match judgment
6. **Finalization**: Creates regular Evidence entity(ies) with the observations

**Data Model** (to be refined after Task implementation):

```haskell
-- Persisted in Document
data Assignment = Assignment
  { id :: !AssignmentId
  , name :: !Text                   -- Assignment name (e.g., "Homework Week 5")
  , assignmentDate :: !Day          -- When assigned (controls student visibility)
  , dueDate :: !(Maybe Day)         -- Optional deadline
  , userIds :: !(Set UserId)        -- Assigned to these students
  , tasks :: ![TaskId]              -- Tasks to complete (reference Task entities)
  , activityType :: !ActivityType   -- Type of activity
  , socialForm :: !SocialForm       -- Individual, pair, group work
  }

-- Non-persistent (UI state only)
data AssignmentEvaluation = AssignmentEvaluation
  { assignment :: !Assignment
  , assessments :: ![(TaskId, Maybe Ability)]  -- Teacher's assessment per task
  }
```

**Key Design Decisions**:

1. **Assignments are concrete, not reusable schemas**: Unlike templates, assignments are actual instances given to specific students on specific dates. This is more natural for a student-facing system.

2. **Tasks are rich enough**: With the Task system (see TASKS-DESIGN.md), tasks carry competence metadata, making separate "aspects" unnecessary.

3. **AssignmentEvaluation is non-persistent**: It's ephemeral UI state during evaluation. Only the final Evidence is persisted.

4. **Evidence is self-contained**: Created evidence contains all necessary information. Assignment modifications don't affect existing evidences.

5. **Multi-student evaluation**: `userIds` allows evaluating multiple students together (primarily for group assignments where all performed identically).

6. **Aggregation strategy**:
   - Multiple tasks can target the same competence level
   - Aggregation uses `max` Ability per competence level (best performance across tasks)
   - Teacher can override during review
   - Example: Student makes mistakes in task 1 but succeeds in tasks 2 and 3 → gets higher ability

7. **Student visibility** (phased):
   - **Phase 2-3**: Assignments exist but filtered from student projected view
   - **Phase 4**: Students can see their assignments (filtered by assignmentDate for tests)
   - Students cannot see in-progress AssignmentEvaluations
   - Students see final evidences like any other evidence

**Phased Implementation**:
- **Phase 1**: Implement Tasks and TaskGroups
- **Phase 2**: Implement Assignment (teachers only)
- **Phase 3**: UI polish → first semi-productive release
- **Phase 4**: Enable student visibility of Tasks and Assignments
- **Phase 5**: Markup language for inline task content

**Implementation Status**: ⏳ Planned for Phase 2 (after Tasks complete)

**See also**: `TASKS-DESIGN.md` for complete task system design and rationale

## Frontend Component Pattern

Components follow a pattern using Miso's component system:
- Each component is defined as a `M.Component` with its own Model, Action, and View
- Components communicate via `SyncDocument` modifications
- Located in `frontend/lib/Competences/Frontend/Component/`
- Views use Tailwind CSS via the `Competences.Frontend.View.Tailwind` module

## Data Flow

**Local Action:**
1. User interacts with frontend component
2. Component generates a `Command`
3. Command is applied optimistically via `modifySyncDocument`:
   - Validates via `handleCommand`
   - If valid: updates `localDocument`, adds to `localChanges`, sends to server
   - If invalid: logs error, no change
4. Document change triggers subscribed handlers
5. UI updates reactively via Miso subscriptions

**Remote Update:**
1. Server sends `ApplyCommand` (echo or from another user)
2. Apply to `remoteDocument`
3. Replay all `localChanges` on new `remoteDocument`:
   - Each command that succeeds: keep in `localChanges`
   - Each command that fails: strip from `localChanges` (already applied)
4. Result becomes new `localDocument`
5. If first local command matched: echo (silent)
6. If local commands were stripped: conflict (show error)
7. Trigger subscribed handlers with new state

## Conflict Resolution Strategy

- **Target Users**: 1-2 teachers per class, mostly online
- **Approach**: Conflicts are rare, so handling is simple:
  - **Echo detection**: If `ApplyCommand` matches `pendingCommand`, it's an echo (clear pending, send next)
  - **Broadcast handling**: If `ApplyCommand` doesn't match, apply to remote and replay local changes
  - **Conflict detection**: If remote command causes local commands to fail during replay, drop conflicted commands
  - **Command rejection**: If server rejects pending command, clear it and try send next
  - **Reconnection**: Clear `pendingCommand` (accept loss), keep `localChanges` (preserve work)
  - No sophisticated merge strategies needed
- **Command Design**: Commands (especially student commands) are designed to minimize conflicts
- **Non-idempotent safety**: Commands sent at most once (one pending at a time)
- `ReorderCompetence` expresses intent clearly to succeed in most reordering scenarios
