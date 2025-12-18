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
- `static/index.js`: WASM loader

Requires WASM toolchain from Nix:
```bash
nix develop .#wasmShell.x86_64-linux
```

**Note**: When running via backend, the OAuth callback endpoint serves dynamically generated HTML with embedded JWT, not the static `index.html`.

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

### Client-Server Communication Model

**HTTP + WebSocket Architecture:**
- HTTP endpoints handle OAuth flow and static file serving
- WebSocket handles all real-time data synchronization
- Single port serves both HTTP and WebSocket (via `websocketsOr`)

**HTTP Endpoints:**
- `GET /`: Redirects to Office365 login page
- `GET /oauth/callback?code=...`: Handles OAuth callback, generates JWT, serves frontend HTML
- `GET /static/*`: Serves static WASM files (app.wasm, ghc_wasm_jsffi.js, index.js)

**WebSocket Connection:**
- Frontend connects with JWT in query parameter: `ws://host:port/?token=<jwt>`
- All data synchronization happens over this connection
- No REST endpoints for data operations

**Message Protocol** (defined in `Competences.Protocol`):

Client → Server:
- `SendCommand Command`: Send a command to be applied
- `KeepAlive`: Maintain connection (prevent timeout)

Server → Client:
- `InitialSnapshot Document`: Initial state sent on connection
- `ApplyCommand Command`: Command to apply (echo or broadcast)
- `CommandRejected Command Text`: Command validation failed
- `KeepAliveResponse`: Acknowledge keep-alive

**Synchronization Flow:**

1. **Connection**: Frontend connects, receives `InitialSnapshot`, sets `remoteDocument` and `localDocument`

2. **Local Action**: User performs action → generates `Command` → applied optimistically to `localDocument` → added to `localChanges` → sent via `SendCommand`

3. **Server Processing**: Server validates and applies command → broadcasts `ApplyCommand` to affected users (including sender)

4. **Remote Update**: Frontend receives `ApplyCommand`:
   - Apply to `remoteDocument`
   - Recompute `localDocument` by replaying all `localChanges` on new `remoteDocument`
   - Commands that fail during replay (already applied) are stripped from `localChanges`
   - **Echo Detection**: If command equals first item in `localChanges`, it's an echo (silent drop)
   - **Conflict Detection**: If command doesn't match and causes local commands to fail, show error

5. **Invariant**: `localDocument = remoteDocument + apply(localChanges)`

**Command Ordering:**
- WebSocket guarantees message ordering within a connection
- Server applies commands in received order
- Echo detection relies on this ordering (first local command should be next echo)

### Key Modules

**Competences.Document** (`common/lib/Competences/Document.hs`):
- Central data model containing all entity types
- Uses `ixset-typed` for indexed collections of entities
- Entities: `User`, `Competence`, `CompetenceGrid`, `Evidence`, `Resource`, `Template`
- Maintains checksums for partial sync

**Competences.Command** (`common/lib/Competences/Command.hs`):
- Defines command types: `EntityCommand` (Create, Delete, Modify)
- `ModifyCommand`: Lock, Release (with optional before/after state)
- `handleCommand`: Pure function that validates and applies commands to documents
- Returns `Either Text (Document, AffectedUsers)` - the `AffectedUsers` determines who receives broadcasts
- Commands are designed to be mostly non-conflicting (especially student commands)
- `ReorderCompetence` expresses user intent clearly to minimize conflicts

**Competences.Frontend.SyncDocument** (`frontend/lib/Competences/Frontend/SyncDocument.hs`):
- Manages document state in the frontend
- **`remoteDocument`**: Server's authoritative state
- **`localDocument`**: Computed as `remoteDocument + localChanges` (always)
- **`localChanges`**: List of commands not yet acknowledged by server
- When server sends command: apply to `remoteDocument`, then replay `localChanges` to recompute `localDocument`
- Commands that fail during replay are stripped (indicates they're already applied or conflicted)
- Provides subscription mechanism via `ChangedHandler`s
- Wrapped in `SyncDocumentRef` (MVar-based) for concurrent access

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
6. Backend searches Document for user with matching `office365Id`
   - If found: use existing user
   - If not found: create new User with `role = Student` and `office365Id` set
7. Backend generates JWT containing: `sub` (userId), `name`, `role`, `o365Id`
8. Backend serves HTML with JWT embedded in `window.COMPETENCES_JWT` global variable
9. Frontend JavaScript (`static/index.js`) loads and initializes WASM app
10. Frontend reads JWT from `window.COMPETENCES_JWT`
11. Frontend connects to WebSocket: `ws://localhost:8080/?token=<jwt>`
12. Backend validates JWT signature, extracts user claims, accepts connection
13. Backend sends `InitialSnapshot` with current Document state
14. Real-time sync begins

**WebSocket Authentication:**
- JWT token passed as query parameter: `ws://host:port/?token=<jwt>`
- `extractUserFromRequest` validates JWT signature and extracts user claims
- Invalid/missing token → connection rejected with "Authentication required"
- JWT expires after 24 hours (configurable in `generateJWT`)

**User Management:**
- Users are automatically created on first Office365 login with default `Student` role
- Teachers must manually promote users to `Teacher` role via User management UI
- `office365Id` field links application users to Microsoft identities
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

## Conflict Resolution Strategy

- **Target Users**: 1-2 teachers per class, mostly online
- **Approach**: Conflicts are rare, so handling is simple:
  - If remote command matches first `localChange`: silent drop (echo)
  - If remote command causes local commands to fail: show error, drop conflicted commands
  - No sophisticated merge strategies needed
- **Command Design**: Commands (especially student commands) are designed to minimize conflicts
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
