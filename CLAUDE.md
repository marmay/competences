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

### Backend Development

Run backend server:
```bash
competences-backend <port> <data-file> <jwt-secret> <client-id> <client-secret> <redirect-uri> <tenant-id> <static-dir>
```

Example:
```bash
cabal run competences-backend -- 8080 data.json "my-secret-key" \
  "azure-client-id" "azure-client-secret" \
  "http://localhost:8080/oauth/callback" \
  "azure-tenant-id" \
  "./static"
```

The server provides:
- HTTP OAuth endpoints at `/` and `/oauth/callback`
- Static file serving at `/static/*`
- WebSocket connections for real-time sync

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
--   - Updates checksums after projection
```

**Status**: ✅ Implemented
- Location: `common/lib/Competences/Document.hs`
- Students see: own User, own Evidences (via UserId index), all CompetenceGrids/Competences/Resources/Templates
- Locks filtered to show only locks on accessible entities
- Checksums updated after filtering

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
   - On success: broadcasts `ApplyCommand` to `AffectedUsers` (deduplicated) including sender
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
- Maintains checksums for partial sync
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

**Current implementation:** In-memory state with file-based persistence
- `loadAppState`: Load document from JSON file on startup
- `saveAppState`: Save document to JSON file
- Auto-save every 60 seconds
- Save on graceful shutdown

Future options under consideration:
- Event sourcing: store all commands, replay to rebuild state
- Snapshot + recent commands (hybrid approach)
- PostgreSQL with JSONB for `Document` storage

Current `backend/schema.sql` only contains user table as placeholder.

**Lock Mechanism:**
- Optimistic concurrency via `Lock`/`Release` commands
- `Release` includes before/after state for conflict detection
- Lock expiration may be added in future

## Frontend WebSocket Integration (COMPLETED)

**Status:** ✅ Fully functional and deployed

**Implementation:**
The frontend successfully establishes WebSocket connections using JSaddle FFI directly (no JavaScript wrapper needed).

**Modules:**
- `frontend/lib/Competences/Frontend/WebSocket.hs` - WebSocket connection management
  - `getJWTToken :: JSM (Maybe Text)` - reads JWT from `window.COMPETENCES_JWT`
  - `connectWebSocket` - creates WebSocket connection with event handlers
  - `sendMessage` - sends ClientMessage to server (one at a time via pendingCommand)

- `frontend/app/Main.hs` (WASM section):
  - Reads JWT token from `window.COMPETENCES_JWT`
  - Establishes WebSocket connection to same host as HTTP server
  - Handles ServerMessage types:
    - `InitialSnapshot Document User` - sets connectedUser and remoteDocument
    - `ApplyCommand` - calls `applyRemoteCommand` (handles echo or broadcast)
    - `CommandRejected` - calls `rejectCommand`
    - `KeepAliveResponse` - acknowledges keep-alive
  - Stores WebSocket connection via `setWebSocket` for sending commands
  - Falls back to test user if no JWT found (development mode)

**Key Implementation Details:**

1. **JSaddle FFI Patterns (IMPORTANT - Common pitfalls fixed):**
   - Constructor calls: `new (jsg "WebSocket") [toJSVal url]` (NOT `jsg "WebSocket" # [url]`)
   - Method calls: `ws # "send" $ [jsonVal]`
   - Property access: `msgEvent ! "data"`
   - Setting properties: `ws <# "onmessage" $ callback`
   - Creating callbacks: `fun $ \this fn args -> case args of (arg:_) -> ... _ -> pure ()`
     - Pattern matching on args list, NOT in lambda parameters
     - Signature: `JSVal -> JSVal -> [JSVal] -> JSM ()`
   - **Type annotations required:** All string literals need explicit `:: Text` to avoid type-defaulting errors with `-Werror`
   - **Import requirements:** Need `new`, `#`, `<#`, `!`, `fun`, `jsg`, `toJSVal`, `valToText` from `Language.Javascript.JSaddle`

2. **WebSocket URL Construction:**
   - Must include trailing `/` before query params: `ws://host:port/?token=<jwt>`
   - Backend expects path format: `/?token=<jwt>` (see `backend/lib/Competences/Backend/WebSocket.hs:53`)
   - Frontend constructs: `wsProtocol <> host <> "/" <> "?token=" <> jwtToken`

3. **WASM Build Dependencies:**
   - Main executable needs: `jsaddle`, `jsaddle-wasm`, `optics-core` when building with `wasm` flag
   - Main executable needs: `OverloadedLabels` extension (for `#field` syntax)
   - Uses `liftIO` for IO operations within JSM monad

4. **Common Compilation Errors Fixed:**
   ```
   ERROR: "Couldn't match expected type: JSM JSVal with actual type: args0 -> JSM JSVal"
   FIX: Use correct operator precedence and argument passing for # operator

   ERROR: "Defaulting the type variable 'name0' to type '[Char]'"
   FIX: Add explicit type annotations: ("propertyName" :: Text)

   ERROR: "Could not load module 'jsaddle'" (in WASM build)
   FIX: Add jsaddle and optics-core to executable build-depends under wasm flag

   ERROR: "Variable not in scope: (#)"
   FIX: Add OverloadedLabels to executable's default-extensions

   ERROR: "Couldn't match type 'IO' with 'JSM'"
   FIX: Use liftIO for IO operations: liftIO $ putStrLn "message"
   ```

5. **Connection Flow:**
   - User authenticates via OAuth → receives JWT in `window.COMPETENCES_JWT`
   - Frontend reads JWT, constructs WebSocket URL
   - Connects to `ws://host:port/?token=<jwt>`
   - Backend validates JWT, sends `InitialSnapshot Document User`
   - Frontend sets `connectedUser` from server (authoritative)
   - Frontend updates `remoteDocument` and replays any `localChanges` (for reconnection)
   - Frontend stores WebSocket connection via `setWebSocket`
   - Real-time sync begins (commands sent one at a time via `pendingCommand`)

## JWT Token Generation and Validation (IMPORTANT)

**Critical Bug Fixed:** The JWT generation was using `show` on the user ID, which created malformed tokens.

**Correct Implementation:**

1. **Import Requirements:**
   ```haskell
   import Competences.Document.Id (Id (..), mkId)  -- Must import constructor
   import Data.UUID.Types qualified as UUID
   import Web.JWT (stringOrURIToText)
   ```

2. **Generating JWT (in `backend/lib/Competences/Backend/Auth.hs`):**
   ```haskell
   -- ❌ WRONG - creates "Id {unId = <uuid>}"
   JWT.sub = JWT.stringOrURI $ T.pack $ show user.id

   -- ✅ CORRECT - creates just the UUID string
   JWT.sub = JWT.stringOrURI $ UUID.toText user.id.unId
   ```
   - With `NoFieldSelectors` + `OverloadedRecordDot`, you MUST import `Id(..)` constructor to access `.unId`
   - Use `UUID.toText` to convert UUID to Text (NOT `show`)

3. **Extracting from JWT:**
   ```haskell
   -- ❌ WRONG - double-wraps the value
   Just uri -> Right $ T.pack $ show uri

   -- ✅ CORRECT - properly extracts the text
   Just uri -> Right $ stringOrURIToText uri
   ```
   - Use `stringOrURIToText` from `Web.JWT` to extract Text from StringOrURI
   - Then use `mkId` to parse the UUID string

**Common Pitfalls:**
- Using `show` on newtypes creates Haskell-formatted strings like `"Id {unId = ...}"`
- Forgetting to import constructors with `NoFieldSelectors` prevents field access
- Not using `stringOrURIToText` for JWT claims extraction
- `OverloadedRecordDot` requires constructor import: `Id(..)` enables `value.unId` syntax

## Backend Implementation Status

**Completed modules:**
- `Competences.Backend.State`: Application state management with STM
  - `AppState`: Holds document (TVar) and connected clients (TVar Map)
  - `loadAppState`/`saveAppState`: File-based persistence
  - `updateDocument`: Apply commands with `handleCommand`
  - `registerClient`/`unregisterClient`: Manage WebSocket connections
  - `broadcastToUsers`: Send ServerMessage to specific users

- `Competences.Backend.WebSocket`: WebSocket handler
  - `wsHandler`: Main WebSocket application handler with JWT validation
  - `handleClient`: Per-client connection management
    - Sends `InitialSnapshot` on connection
    - Handles incoming `ClientMessage` (SendCommand, KeepAlive)
    - Cleans up on disconnect
  - `handleClientMessage`: Routes messages and applies commands
    - Calls `updateDocument` which uses `handleCommand`
    - Broadcasts `ApplyCommand` to affected users on success
    - Sends `CommandRejected` to sender on failure
    - Triggers async save after successful command
  - `extractUserFromRequest`: Validates JWT from query param and extracts user info

- `backend/app/Main.hs`: Server entry point
  - Command-line arguments: `<port> <data-file> <jwt-secret> <client-id> <client-secret> <redirect-uri> <tenant-id> <static-dir>`
  - Loads state from file or initializes empty Document
  - Creates combined WAI application using `websocketsOr`:
    - WebSocket requests → `wsHandler`
    - HTTP requests → Servant server (OAuth + static files)
  - Runs both on single port via `Network.Wai.Handler.Warp`
  - Periodic auto-save (60 seconds) in background thread
  - Graceful shutdown with final save on Ctrl+C

- `Competences.Backend.Auth`: JWT and OAuth2 implementation
  - `OAuth2Config`: Configuration for Office365 OAuth
  - `getAuthorizationUrl`: Generate O365 login URL
  - `exchangeCodeForToken`: Exchange auth code for access token
  - `getUserInfo`: Get user profile from Microsoft Graph API
  - `generateJWT`: Create JWT tokens for users
  - `validateJWT`: Validate JWT signatures
  - `extractUserFromJWT`: Extract user claims from validated JWT

- `Competences.Backend.HTTP`: HTTP server with OAuth and static files
  - `AppAPI`: Servant API type definition
  - `oauthInitHandler`: Redirects to Office365 login (GET /)
  - `oauthCallbackHandler`: OAuth callback handler (GET /oauth/callback)
    - Exchanges authorization code for Microsoft access token
    - Retrieves user info from Microsoft Graph API
    - Finds existing user by `office365Id` or creates new Student user
    - Generates JWT token with user claims (id, name, role, office365Id)
    - Serves dynamically generated HTML with JWT in `window.COMPETENCES_JWT`
  - `renderFrontendHTML`: Generates HTML that loads `/static/index.js` with embedded JWT
  - `findOrCreateUser`: Looks up user by Office365Id index or creates default Student
  - Static file serving via Servant's `serveDirectoryWebApp`

**Complete Authentication Flow:**
1. User visits `http://localhost:8080/` → redirected to Office365 login
2. User authenticates with Microsoft credentials
3. Office365 redirects to `/oauth/callback?code=<auth-code>`
4. Backend exchanges code for Microsoft Graph API access token
5. Backend retrieves user profile (id, displayName, mail, userPrincipalName)
6. Backend searches Document for user with matching `office365Id` (email address)
   - Uses `mail` field, or falls back to `userPrincipalName`
   - If found: use existing user
   - **If not found: Login fails with HTTP 400 error** (no auto-creation)
7. Backend generates JWT containing: `sub` (userId), `name`, `role`, `o365Id`
8. Backend serves HTML with JWT embedded in `window.COMPETENCES_JWT` global variable
9. Frontend JavaScript (`static/index.js`) loads and initializes WASM app
10. Frontend reads JWT from `window.COMPETENCES_JWT`
11. Frontend connects to WebSocket: `ws://localhost:8080/?token=<jwt>`
12. Backend validates JWT signature, extracts user claims, accepts connection
13. Backend sends `InitialSnapshot Document User` with current state + authenticated user
14. Frontend sets `connectedUser` from server (authoritative source)
15. Frontend stores WebSocket connection for sending commands
16. Real-time sync begins

**WebSocket Authentication:**
- JWT token passed as query parameter: `ws://host:port/?token=<jwt>`
- `extractUserFromRequest` validates JWT signature and extracts user claims
- Invalid/missing token → connection rejected with "Authentication required"
- JWT expires after 24 hours (configurable in `generateJWT`)

**User Management:**
- Users must be created manually via User management UI before they can log in
- `office365Id` field stores the user's email address (editable in UI)
- Teachers set user roles (`Student` or `Teacher`) via User management UI
- On OAuth login, email from Microsoft Graph is matched against `office365Id`
- Login fails if no matching user exists (no auto-creation)
- Uses `ixset-typed` index on `Maybe Office365Id` for fast lookup

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

## Lessons Learned & Debugging Guide

### General Development Workflow

1. **Always read files before editing** - Use the Read tool to understand existing code structure
2. **Build incrementally** - Fix one error at a time, rebuild frequently
3. **Check dependencies** - Missing packages in `.cabal` files are a common issue
4. **Enable all extensions** - Ensure executable sections have same extensions as library (especially `OverloadedLabels`)

### WASM Frontend Development

**Build Command:**
```bash
./deploy_frontend.sh
```

**Common Issues:**
1. **Missing dependencies in WASM build:**
   - Check executable's `if flag(wasm)` section in `.cabal` file
   - Need: `jsaddle`, `jsaddle-wasm`, `optics-core`

2. **Type defaulting errors:**
   - Always add explicit type annotations to string literals: `("text" :: Text)`
   - Affects all JSaddle FFI calls (property names, method names)

3. **Import errors with OverloadedRecordDot:**
   - Import data constructors: `Type(..)` not just `Type`
   - Example: `import Competences.Document.Id (Id(..))` enables `.unId` access

### JSaddle FFI Quick Reference

```haskell
-- Imports needed
import Language.Javascript.JSaddle
  ( JSM, JSVal, fun, jsg, new, toJSVal, valToText, (!), (#), (<#) )

-- Global object access
window <- jsg ("window" :: Text)

-- Property access (reading)
value <- obj ! ("propertyName" :: Text)

-- Property setting
_ <- obj <# ("propertyName" :: Text) $ someValue

-- Constructor calls (like 'new WebSocket(url)')
ws <- new (jsg ("WebSocket" :: Text)) [toJSVal url]

-- Method calls (like 'ws.send(data)')
jsonVal <- toJSVal textData
_ <- ws # ("send" :: Text) $ [jsonVal]

-- Creating callbacks
callback <- fun $ \this fn args -> do
  case args of
    (firstArg:_) -> do
      -- handle event
      pure ()
    _ -> pure ()

-- Setting event handlers
_ <- element <# ("onclick" :: Text) $ callback
```

### Backend JWT Debugging

**Check JWT contents:**
```bash
# Decode JWT (just the payload, base64)
echo "eyJ..." | base64 -d
```

**Common JWT issues:**
- User ID not found: Check `sub` field is valid UUID (not `"Id {unId = ...}"`)
- Authentication fails: Verify JWT secret matches between generation and validation
- User not linked: Office365 ID must match a user in the database

### WebSocket Connection Debugging

**Frontend (Browser Console):**
```javascript
// Check if JWT is set
console.log(window.COMPETENCES_JWT)

// Check WebSocket connection
// Should see: "WebSocket connected"
```

**Backend (Terminal):**
```
# Should see:
Client connected: <name> (<user-id>)

# If authentication fails:
Authentication failed: <error message>
```

**Common Connection Issues:**
1. **400 Bad Request:**
   - Check WebSocket URL includes `/` before query: `ws://host:port/?token=...`
   - Verify JWT token is being passed correctly

2. **401 Unauthorized:**
   - JWT validation failed
   - Check JWT secret matches
   - Check JWT hasn't expired

3. **Connection refused:**
   - Backend not running
   - Wrong port number
   - Firewall blocking WebSocket

### NoFieldSelectors + OverloadedRecordDot Pattern

When using both extensions together:

```haskell
-- Import constructor to enable field access
import MyModule (MyType(..))  -- Note the (..)

-- Now you can use record dot syntax
value = myRecord.field.nestedField

-- For newtypes like Id:
import Competences.Document.Id (Id(..))  -- Enables .unId
uuid = userId.unId
```

**Why this matters:**
- `NoFieldSelectors` disables automatic field selector generation
- `OverloadedRecordDot` provides `.` syntax as an alternative
- But you MUST import the constructor for GHC to know about the fields

## Recent Implementation: Document Projection (2025-12-19)

Implemented document projection and access control for data privacy:

### What Was Implemented

1. **`projectDocument` function** (`common/lib/Competences/Document.hs`):
   - Filters document based on user identity
   - Teachers: see full document
   - Students: see only own User, own Evidences (via UserId index), all public materials (CompetenceGrids, Competences, Resources, Templates)
   - Locks filtered to show only accessible entities
   - Uses efficient UserId index on Evidence for filtering

2. **Backend Projection** (`backend/lib/Competences/Backend/WebSocket.hs:71-74`):
   - `InitialSnapshot` now sends projected documents
   - Students never receive unauthorized data
   - Frontend's `remoteDocument` is always a projection

3. **Authorization** (`backend/lib/Competences/Backend/WebSocket.hs:95-99`):
   - All commands currently require Teacher role
   - Non-teachers receive `CommandRejected` with error message
   - Future: per-command authorization for student commands

4. **Deduplication** (`backend/lib/Competences/Backend/State.hs:110`):
   - `broadcastToUsers` deduplicates user IDs
   - Prevents sending same command multiple times to same user

### Key Design Decisions

- **Projection based on identity, not role**: Students see data about THEM specifically, not just "any student data"
- **Efficient indexing**: Used UserId index on Evidence for O(log n) filtering instead of O(n) scan
- **AffectedUsers = projection changes**: Users receive broadcasts only if their projected document changes
- **Server authoritative**: All authorization on backend, frontend can make optimistic updates

### TODO

- **Property-based tests**: Add QuickCheck tests to verify `AffectedUsers` correctness
  - Requires: QuickCheck dependency, Arbitrary instances
  - Property: `AffectedUsers(cmd) = {u | project(u, doc) ≠ project(u, apply(cmd, doc))}`
  - Location: `common/test/ProjectionProperties.hs`
