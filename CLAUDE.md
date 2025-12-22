# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a competences tracking application written in Haskell, using a multi-package Cabal project structure. It consists of:
- **backend**: Server-side component with Office365 OAuth, JWT authentication, WebSocket sync, and static file serving
- **frontend**: Miso-based web frontend compiled to WASM, served via backend
- **common**: Shared code including domain models, commands, and business logic
- **csvconvert**: Utility for converting CSV files to the application's format

The application is built with Nix flakes and uses haskell.nix for reproducible builds.

## Building and Running

### Development with Cabal

Build all packages:
```bash
cabal build all
```

Build specific package:
```bash
cabal build competences-backend
cabal build competences-frontend
cabal build competences-csvconvert
cabal build competences-common
```

Run tests:
```bash
cabal test all
cabal test competences-backend-test
cabal test competences-common-test
```

### Dependency Management

**Reproducible builds:**
The project uses two mechanisms for reproducible builds:

1. **Backend (haskell.nix)**:
   - Uses haskell.nix's materialized dependencies (fully reproducible)
   - Managed via `cabal.project` at project root

2. **Frontend (WASM)**:
   - Separate `frontend/cabal.project` with index-state pinning
   - `frontend/cabal.project.freeze` locks all dependencies
   - Includes `../common` package and necessary git dependencies

**Updating frontend dependencies:**
```bash
# Enter WASM development shell
nix develop .#wasmShell.x86_64-linux

# Update index and regenerate freeze file
cd frontend
wasm32-wasi-cabal update
wasm32-wasi-cabal freeze
cd ..

# Commit the updated freeze file
git add frontend/cabal.project.freeze
git commit -m "Update frontend dependencies"
```

**Note:** Backend and frontend have separate cabal.project files because they use different GHC versions (native vs WASM) with incompatible boot library versions.

### Backend Development

**Prerequisites:**
1. PostgreSQL database initialized with schema: `psql < backend/schema.sql`
2. Configuration file with secrets (see `backend/config.example.json`)

Run backend server:
```bash
competences-backend \
  --port <PORT> \
  --database <CONNSTRING> \
  --config <CONFIG_FILE> \
  --static <STATIC_DIR> \
  [--init-document <INITIAL_JSON>]
```

Example:
```bash
cabal run competences-backend -- \
  --port 8080 \
  --database "host=localhost dbname=competences_class_9a" \
  --config backend/config.json \
  --static ./static \
  --init-document data/class_9a.json  # Only for first-time initialization
```

**Configuration file** (`config.json`):
```json
{
  "jwtSecret": "your-secret-key-here-minimum-32-characters",
  "oauth2": {
    "clientId": "your-azure-client-id",
    "clientSecret": "your-azure-client-secret",
    "redirectUri": "http://localhost:8080/oauth/callback",
    "tenantId": "your-azure-tenant-id"
  }
}
```

**Command-line options:**
- `--port, -p`: Port to listen on
- `--database, -d`: PostgreSQL connection string
- `--config, -c`: Configuration file (JSON) containing secrets
- `--static, -s`: Static files directory
- `--init-document`: Initial document JSON (used only if database is empty)

The server provides:
- HTTP OAuth endpoints at `/` and `/oauth/callback`
- Static file serving at `/static/*`
- WebSocket connections for real-time sync
- PostgreSQL persistence with command sourcing and snapshots

### Frontend Development

**Development mode** (JSaddle-based, for rapid iteration):
```bash
./start.sh <CLASS_NAME>
```
This looks for `data/<CLASS_NAME>.json`, creates a backup, and starts the JSaddle-based development server.

**Production mode** (WASM deployment):
```bash
./deploy_frontend.sh
```
Compiles frontend to WASM and places files in `static/` directory:
- `static/app.wasm`: Compiled Haskell frontend
- `static/ghc_wasm_jsffi.js`: GHC WASM FFI runtime
- `static/index.html`: Entry point (only used for standalone testing)
- `static/index.js`: WASM loader (updated to use absolute paths: `/static/app.wasm`, `/static/ghc_wasm_jsffi.js`)

Requires WASM toolchain from Nix:
```bash
nix develop .#wasmShell.x86_64-linux
```

**IMPORTANT Notes:**
- When running via backend, the OAuth callback endpoint serves dynamically generated HTML with embedded JWT, not the static `index.html`
- The `index.js` was updated to use absolute paths so resources load correctly from `/oauth/callback` page
- WebSocket integration is complete and functional (see Frontend WebSocket Integration section)

### Nix Development

Enter development shell:
```bash
nix develop
```

Enter WASM development shell:
```bash
nix develop .#wasmShell.x86_64-linux
```

## Code Formatting

Use Fourmolu for formatting with settings in `fourmolu.yaml`:
```bash
fourmolu --mode inplace $(find . -name '*.hs' -not -path './dist-newstyle/*')
```

Configuration highlights:
- 2-space indentation
- 100 column limit
- Leading function arrows, commas, imports
- No Unicode

## Architecture

### Core Architecture Pattern: Command-Driven Document Synchronization

The application uses a command-sourcing pattern where:
1. All state is held in a `Document` (from `Competences.Document`)
2. Changes are expressed as `Command`s (from `Competences.Command`)
3. Commands are handled via `handleCommand` which validates and applies changes
4. The frontend maintains a `SyncDocument` that tracks local and remote document state

### Document Projection and Access Control

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

### Client-Server Communication Model

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

### Key Modules

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

### Frontend Component Pattern

Components follow a pattern using Miso's component system:
- Each component is defined as a `M.Component` with its own Model, Action, and View
- Components communicate via `SyncDocument` modifications
- Located in `frontend/lib/Competences/Frontend/Component/`
- Views use Tailwind CSS via the `Competences.Frontend.View.Tailwind` module

### Data Flow

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

## Common Extensions

The project uses GHC2024 as default language with additional extensions:
- `DuplicateRecordFields`: Multiple records can share field names
- `NoFieldSelectors`: Disable automatic field selector generation
- `OverloadedRecordDot`: Enable `record.field` syntax
- `OverloadedLabels`: Enable `#field` syntax
- `OverloadedStrings`: String literals are polymorphic
- `GADTs`: Generalized algebraic data types
- `TypeFamilies`: Type-level functions

## Persistence

**Current implementation:** PostgreSQL with command sourcing + snapshots

### Command Sourcing + Snapshots Architecture

The application uses a hybrid persistence approach that provides both full audit trail and fast startup:

**Storage Strategy:**
- **Commands table**: Every state change is stored as a command (event sourcing)
- **Snapshots table**: Periodic full document snapshots for fast recovery
- **Metadata table**: Tracks last snapshot generation and timestamp
- **Schema_migrations table**: Tracks database schema version
- **Startup_log table**: Logs backend instance starts and stops for debugging

**Key Features:**
- Full audit trail: every change is logged as a Command
- Fast startup: load latest snapshot + replay recent commands (not entire history)
- Point-in-time recovery: restore to any snapshot + replay to specific generation
- Non-graceful shutdown recovery: creates snapshot on startup if needed
- Automatic persistence: commands saved to database immediately on application, snapshots created periodically
- **Versioned envelopes**: Commands and snapshots use versioned JSON envelopes for schema evolution
- Clean architecture: deprecated file-based persistence fully removed from production code

### Database Schema

Located in `backend/schema.sql`:

```sql
-- Commands table (generation = auto-incrementing sequence)
CREATE TABLE commands (
  generation BIGSERIAL PRIMARY KEY,
  command_id UUID NOT NULL UNIQUE,
  user_id UUID NOT NULL,
  command_data JSONB NOT NULL,  -- Versioned envelope: { version, userId, payload }
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Snapshots table
CREATE TABLE snapshots (
  id BIGSERIAL PRIMARY KEY,
  snapshot_id UUID NOT NULL UNIQUE,
  generation BIGINT NOT NULL,  -- Links to commands.generation
  document_data JSONB NOT NULL,  -- Versioned envelope: { version, payload }
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

**Versioning Strategy:**
Commands and snapshots are stored with versioned JSON envelopes to support schema evolution:
- **Command envelope**: `{ "version": 1, "userId": "<uuid>", "payload": <Command> }`
- **Snapshot envelope**: `{ "version": 1, "payload": <Document> }`
- Migrations applied automatically when loading old versions
- See `Competences.Backend.Envelope` for implementation details

### Snapshot Strategy

**Snapshot triggers:**
1. **Every 25 commands**: Checked after each command in `updateDocument`
2. **Every 15 minutes** (if ≥1 command applied): Periodic timer in Main.hs
3. **On graceful shutdown**: Final snapshot in `gracefulShutdown`
4. **On startup** (non-graceful shutdown recovery): If commands newer than latest snapshot

**Retention policy:**
- Currently: keep all snapshots indefinitely
- Future: implement GC with daily/weekly/monthly retention strategy

### Database Modules

**Competences.Backend.Envelope** (`backend/lib/Competences/Backend/Envelope.hs`):
- **Versioned envelope types**: `CommandEnvelope` and `SnapshotEnvelope`
- **Current versions**: Both at version 1
- `wrapCommand`: Wrap Command in versioned envelope before storage
- `unwrapCommand`: Unwrap and migrate Command from envelope
- `wrapSnapshot`: Wrap Document in versioned envelope before storage
- `unwrapSnapshot`: Unwrap and migrate Document from envelope
- **Migration pattern**: When schema changes, increment version and add migration function
- Example migration (for future use):
  ```haskell
  unwrapCommand env = case env.version of
    1 -> parseJSON env.payload  -- Current version
    2 -> do
      cmdV1 <- parseJSON env.payload
      pure $ migrateV1toV2 cmdV1  -- Apply migration
  ```

**Competences.Backend.Database** (`backend/lib/Competences/Backend/Database.hs`):
- `initPool`: Create connection pool (3 connections, 60s idle timeout)
- `closePool`: Cleanup on shutdown
- `checkSchemaVersion`: Validates schema version = 1, fails on mismatch
- `saveCommand`: Wraps command in envelope, saves to DB, returns generation number
- `loadCommandsSince`: Loads and unwraps commands, applying migrations if needed
- `saveSnapshot`: Wraps document in envelope, saves snapshot at specific generation
- `loadLatestSnapshot`: Loads and unwraps latest snapshot, applying migrations if needed
- `shouldTakeSnapshot`: Checks if snapshot needed (25 commands OR 15 min + ≥1 command)
- `logStartup` / `logShutdown`: Tracks backend instance lifecycle

**Competences.Backend.Config** (`backend/lib/Competences/Backend/Config.hs`):
- `loadConfig`: Loads secrets from JSON file (keeps secrets out of CLI args)
- Config file format: see `backend/config.example.json`

### Startup Sequence

1. Parse command-line options (optparse-applicative)
2. Load configuration file (secrets)
3. Initialize database connection pool
4. **Check schema version** (fails if mismatch)
5. **If `--init-document` provided AND database empty:**
   - Load document from JSON
   - Create `SetDocument` command (with system user ID = nil UUID)
   - Save initial snapshot at generation 1
6. **Load document from database:**
   - Load latest snapshot
   - Load commands since that snapshot
   - Replay commands to rebuild current state
   - **Fail if no document found** (prevents accidental empty state)
7. **Create recovery snapshot if needed:**
   - If any commands were replayed (non-graceful shutdown)
   - Create snapshot at current generation
   - Avoids replaying same commands on next startup
8. Initialize AppState with loaded document
9. Log startup to startup_log table
10. Start periodic snapshot timer (15 minute interval)
11. Start HTTP + WebSocket server

### Migration from File-Based Storage

**For existing deployments:**
1. Initialize PostgreSQL database: `psql < backend/schema.sql`
2. Use `--init-document <existing-data.json>` on first startup
3. Backend detects empty database and initializes from JSON
4. All subsequent changes go to database
5. Old JSON file can be kept as backup but is no longer used

**Backward compatibility:**
- `loadAppState` and `saveAppState` functions still exist in `State.hs` for potential migration scripts
- File-based persistence has been **fully removed** from all production code paths:
  - `wsHandler` no longer takes a file path parameter
  - Commands are persisted only to database (via `updateDocument`)
  - No file saves occur during normal operation

### Lock Mechanism

- Optimistic concurrency via `Lock`/`Release` commands
- `Release` includes before/after state for conflict detection
- Lock expiration may be added in future

## Frontend WebSocket Integration

**Status:** ✅ Fully functional - see DEBUGGING.md for JSaddle FFI patterns and troubleshooting

## JWT Token Generation and Validation

**IMPORTANT:** Never use `show` on newtypes - use proper conversion functions.

Generating JWT (in `backend/lib/Competences/Backend/Auth.hs`):
```haskell
-- ✅ CORRECT - import constructor and use UUID.toText
import Competences.Document.Id (Id (..), mkId)
import Data.UUID.Types qualified as UUID

JWT.sub = JWT.stringOrURI $ UUID.toText user.id.unId  -- NOT show user.id
```

Extracting from JWT:
```haskell
-- ✅ CORRECT - use stringOrURIToText
import Web.JWT (stringOrURIToText)

Just uri -> Right $ stringOrURIToText uri  -- NOT show uri
```

See DEBUGGING.md for detailed JWT debugging tips.

## Authentication Flow

1. User visits `/` → redirected to Office365 login
2. OAuth callback receives auth code → exchanges for access token
3. Backend retrieves user profile from Microsoft Graph
4. Backend matches email to existing user's `office365Id` field
   - **Login fails if no matching user** (no auto-creation)
5. Backend generates JWT with user claims (id, name, role, office365Id)
6. Backend serves HTML with JWT in `window.COMPETENCES_JWT`
7. Frontend reads JWT and connects via WebSocket: `ws://host:port/?token=<jwt>`
8. Backend validates JWT and sends `InitialSnapshot Document User`
9. Real-time sync begins

**Key points:**
- Users must be created manually via UI before they can log in
- JWT passed as WebSocket query parameter
- JWT expires after 24 hours
- Invalid/missing token → connection rejected

## Conventions

- Use optics-core for record manipulation (`^.`, `.~`, `%~`, `&`)
- Entity IDs use newtype wrappers from `Competences.Document.Id`
- Translations via `Competences.Frontend.Common.Translate`
- Order/positioning uses `Competences.Document.Order`
- Frontend styling via `Competences.Frontend.View.Tailwind`
- When importing types with `NoFieldSelectors`, use `Type(..)` to access record fields:
  - ✓ `import Competences.Document (Document(..), User(..))`
  - ✗ `import Competences.Document (Document, User)` (won't allow `doc.users` access)

## Testing the Full Stack

**Prerequisites:**
1. WASM toolchain installed: `nix develop .#wasmShell.x86_64-linux`
2. Frontend compiled: `./deploy_frontend.sh`
3. Azure OAuth app configured with Client ID, Secret, Tenant ID

**Running:**
1. Start backend:
   ```bash
   cabal run competences-backend -- \
     8080 data.json "secret-key" \
     "client-id" "client-secret" \
     "http://localhost:8080/oauth/callback" \
     "tenant-id" \
     "./static"
   ```

2. Visit `http://localhost:8080/`
3. Authenticate with Office365
4. Should see frontend load with JWT embedded
5. Check browser console for WebSocket connection status (currently will show errors until WebSocket integration is complete)

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

## Office365 OAuth Configuration

To set up Office365 authentication:

1. **Azure Portal Setup:**
   - Register application in Azure Active Directory
   - Note: Client ID, Client Secret, Tenant ID
   - Set Redirect URI to `http://localhost:8080/oauth/callback` (or your deployment URL)
   - Grant API permissions: `User.Read`, `openid`, `profile`, `email`

2. **Backend Configuration:**
   - Pass OAuth credentials as command-line arguments when starting backend
   - JWT secret should be a strong random string (used for signing tokens)
   - Static directory should point to where WASM files are deployed (typically `./static`)

3. **Security Considerations:**
   - JWT tokens expire after 24 hours
   - Tokens are validated on every WebSocket connection attempt
   - Office365 access tokens are only used during OAuth callback (not stored)
   - User sessions persist via JWT until expiration or server restart

## Common Pitfalls

> **For detailed debugging help, see DEBUGGING.md** - grep that file when you encounter errors!

**NoFieldSelectors + OverloadedRecordDot:**
```
ERROR: "No instance for 'HasField "field" ...'"
FIX: Import constructor with (..) - e.g., import Module (Type(..))
```

**Type defaulting in JSaddle FFI:**
```
ERROR: "Defaulting the type variable 'name0' to type '[Char]'"
FIX: Add explicit type annotations: ("text" :: Text)
```

**Missing extensions in executables:**
```
ERROR: HasField errors in executable but library builds fine
FIX: Add default-extensions to executable stanza matching library
```

**Pattern matching on PostgreSQL query results:**
```
ERROR: "No instance for 'FromField (Int64, ByteString)'"
FIX: query_ returns [(col1, col2)], pattern match: (x, y) : _ not [(x, y) : _]
```

**JWT with newtypes:**
```
ERROR: JWT contains "Id {unId = ...}" instead of UUID
FIX: Use UUID.toText userId.unId, NOT show userId
```

## Document Projection and Access Control

**`projectDocument`** (`common/lib/Competences/Document.hs`) filters documents based on user identity:
- Teachers: see full document
- Students: see only own User, own Evidences (via UserId index), all public materials
- `InitialSnapshot` sends projected documents
- Commands broadcast only to users whose projection changes (`AffectedUsers`)
- All commands currently require Teacher role

**TODO:** Add QuickCheck property tests for `AffectedUsers` correctness

## Testing Checklist

### Database Persistence Testing

Before deploying to production, verify the following:

**Schema and Initialization:**
- [ ] PostgreSQL database created for each class
- [ ] Schema initialized: `psql -d <dbname> < backend/schema.sql`
- [ ] Schema version check works (rejects mismatched versions)
- [ ] Connection pool initialization succeeds

**First-Time Initialization:**
- [ ] `--init-document` works with empty database
- [ ] SetDocument command created with system user ID (nil UUID)
- [ ] Initial snapshot created at generation 1
- [ ] Subsequent startups skip initialization (database not empty)

**Command Persistence:**
- [ ] Commands saved to database after each state change
- [ ] Generation numbers auto-increment correctly
- [ ] Commands can be loaded from database
- [ ] Command replay rebuilds correct document state

**Snapshot Creation:**
- [ ] Snapshot created every 25 commands
- [ ] Periodic snapshot timer works (15 minute interval)
- [ ] Snapshot created on graceful shutdown (Ctrl+C)
- [ ] Snapshot includes correct generation number
- [ ] Metadata table updated after snapshot

**Non-Graceful Shutdown Recovery:**
- [ ] Kill backend (SIGKILL) while commands exist after last snapshot
- [ ] Restart backend
- [ ] Verify recovery snapshot created on startup
- [ ] Verify document state is correct after recovery

**Document Loading:**
- [ ] Latest snapshot loads correctly
- [ ] Commands since snapshot are replayed
- [ ] Final document state matches expected
- [ ] Backend fails to start if no document exists (prevents empty state)

**Startup/Shutdown Logging:**
- [ ] Startup logged to startup_log table with instance UUID
- [ ] Initial generation recorded correctly
- [ ] Shutdown updates startup_log.stopped_at timestamp

**Configuration and CLI:**
- [ ] Config file loads correctly (JWT secret, OAuth credentials)
- [ ] CLI options parsed with optparse-applicative
- [ ] Help message displays: `--help`
- [ ] Secrets NOT visible in process list (`ps aux | grep competences-backend`)

### Multi-Instance Testing

For production deployment with multiple classes:
- [ ] Create separate databases: `competences_class_9a`, `competences_class_9b`, etc.
- [ ] Create separate config files: `config_9a.json`, `config_9b.json`, etc.
- [ ] Start multiple backend instances on different ports
- [ ] Verify each instance connects to correct database
- [ ] Verify each instance loads correct document
- [ ] Verify instances don't interfere with each other

### WebSocket and Real-Time Sync:
- [ ] Commands sent over WebSocket are persisted to database
- [ ] Snapshots created during live sessions
- [ ] Multiple users connected to same instance receive broadcasts
- [ ] Projection works correctly for students (only see own evidences)

## Production Deployment

**See [DEPLOYMENT.md](DEPLOYMENT.md) for complete deployment guide.**

The application includes a NixOS module for declarative deployment with:
- Multi-instance support (one backend per class)
- Optimized WASM frontend build (wasm-opt + wasm-tools strip)
- Nginx reverse proxy with automatic HTTPS (Let's Encrypt)
- PostgreSQL database management
- Secrets management via agenix
- Build on powerful PC, deploy to weak server via `nixos-rebuild --target-host`

**Quick start:**
```nix
services.competences = {
  enable = true;
  instances.class-9a = {
    port = 8081;
    subdomain = "9a";
    database = "competences_class_9a";
    secretsFile = config.age.secrets.competences-9a.path;
  };
  nginx.enable = true;
  nginx.domain = "competences.example.com";
  postgresql.enable = true;
};
```

**Deploy:**
```bash
sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

See `docs/example-nixos-config.nix` for complete example and DEPLOYMENT.md for detailed documentation.

## Next Steps and Future Work

### Immediate (Before Production)

1. ✅ **Versioning envelope (COMPLETED):**
   - Commands and snapshots now use versioned JSON envelopes
   - Supports schema evolution and backward compatibility
   - Implemented in `Competences.Backend.Envelope`
   - Migration pattern ready for future schema changes

2. **Test full deployment workflow:**
   - Complete testing checklist above
   - Test NixOS deployment on staging server
   - Verify all instances start correctly
   - Test secrets management with agenix
   - Test versioned envelope migrations with sample data
   - Document any issues found

### Short Term

4. **Snapshot garbage collection:**
   - Implement retention policy (keep all today, 1/day current week, 1/week current month, 1/month forever)
   - Add database maintenance job
   - Test that old snapshots are safely removed

5. **Monitoring and observability:**
   - Add structured logging
   - Track metrics (commands/second, snapshot creation time, replay time)
   - Alert on database connection failures
   - Dashboard for instance health

6. **Backup and disaster recovery:**
   - Automated PostgreSQL backups (pg_dump)
   - Test restore from backup procedure
   - Document recovery procedures
   - Consider point-in-time recovery (PITR)

### Medium Term

7. **Student command authorization:**
   - Currently all commands require Teacher role
   - Add per-command authorization logic
   - Students should be able to create/modify own evidences
   - Server validates student can only modify own data

8. **Property-based testing:**
   - Add QuickCheck dependency
   - Create Arbitrary instances for Document, Command, etc.
   - Write properties for `AffectedUsers` correctness
   - Write properties for projection correctness

9. **Performance optimization:**
   - Profile database queries
   - Add database indexes if needed (currently has indexes on generation, created_at, user_id)
   - Consider connection pooling tuning
   - Benchmark snapshot creation time on large documents

### Long Term

10. **Command replay optimization:**
    - Currently replays all commands since last snapshot
    - Consider incremental snapshots or snapshot diffs
    - Investigate CRDT-based approaches for conflict-free merges

11. **Multi-tenancy in single backend:**
    - If managing 10+ classes becomes unwieldy
    - Refactor to support multiple documents in single backend
    - Add tenant ID to AppState and database queries
    - More complex but reduces operational overhead

12. **Analytics and reporting:**
    - Analyze command history for insights
    - Student progress reports
    - Teacher activity dashboards
    - Export capabilities for external analysis
