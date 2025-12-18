# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a competences tracking application written in Haskell, using a multi-package Cabal project structure. It consists of:
- **backend**: Server-side component (currently minimal, only API/auth stubs)
- **frontend**: Miso-based web frontend (runs either via JSaddle-Warp or compiled to WASM)
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

### Frontend Development

Run frontend locally (requires class data file):
```bash
./start.sh <CLASS_NAME>
```
This looks for `data/<CLASS_NAME>.json`, creates a backup, and starts the JSaddle-based development server.

Deploy frontend to WASM:
```bash
./deploy_frontend.sh
```
Requires WASM toolchain from Nix:
```bash
nix develop .#wasmShell.x86_64-linux
```

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

**WebSocket-Only Architecture:**
- Backend serves static frontend files with embedded JWT token
- Frontend establishes WebSocket connection using JWT for authentication
- All client-server communication happens over this single WebSocket
- No REST endpoints for data synchronization

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
  - `wsHandler`: Main WebSocket application handler
  - `handleClient`: Per-client connection management
  - `handleClientMessage`: Route ClientMessage (SendCommand, KeepAlive)
  - `extractUserFromRequest`: Stub for JWT validation (currently returns test user)

- `backend/app/Main.hs`: Server entry point
  - Command-line arguments: `<port> <data-file>`
  - Loads state from file or starts empty
  - WebSocket-only server (no HTTP endpoints yet)
  - Periodic auto-save (60 seconds)
  - Graceful shutdown with final save

- `Competences.Backend.Auth`: JWT and OAuth2 implementation
  - `OAuth2Config`: Configuration for Office365 OAuth
  - `getAuthorizationUrl`: Generate O365 login URL
  - `exchangeCodeForToken`: Exchange auth code for access token
  - `getUserInfo`: Get user profile from Microsoft Graph API
  - `generateJWT`: Create JWT tokens for users
  - `validateJWT`: Validate JWT signatures
  - `extractUserFromJWT`: Extract user claims from validated JWT

**JWT Authentication Flow:**
1. User clicks login → redirected to Office365
2. OAuth callback receives authorization code
3. Exchange code for access token
4. Get user info from Microsoft Graph API
5. Find or create User in Document (by office365Id)
6. Generate JWT token with user claims
7. Serve frontend HTML with JWT embedded
8. Frontend connects to WebSocket with JWT in query parameter
9. Backend validates JWT and establishes connection

**WebSocket Authentication:**
- JWT token passed as query parameter: `ws://host:port/?token=<jwt>`
- `extractUserFromRequest` validates JWT and extracts user info
- Invalid/missing token → connection rejected

**TODO:**
- OAuth callback endpoint implementation (integrate `Competences.Backend.Auth` functions)
- Static file serving with JWT embedding in HTML
- Frontend HTML/JavaScript entry point

## Conventions

- Use optics-core for record manipulation (`^.`, `.~`, `%~`, `&`)
- Entity IDs use newtype wrappers from `Competences.Document.Id`
- Translations via `Competences.Frontend.Common.Translate`
- Order/positioning uses `Competences.Document.Order`
- Frontend styling via `Competences.Frontend.View.Tailwind`

## Conflict Resolution Strategy

- **Target Users**: 1-2 teachers per class, mostly online
- **Approach**: Conflicts are rare, so handling is simple:
  - If remote command matches first `localChange`: silent drop (echo)
  - If remote command causes local commands to fail: show error, drop conflicted commands
  - No sophisticated merge strategies needed
- **Command Design**: Commands (especially student commands) are designed to minimize conflicts
- `ReorderCompetence` expresses intent clearly to succeed in most reordering scenarios
