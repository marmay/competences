# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Documentation Structure

This is a quick-start guide. **Read the detailed documentation when needed:**

**When to read [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md):**
- Working on command handling or document synchronization
- Modifying client-server communication (WebSocket, HTTP)
- Implementing new entity types or commands
- Understanding document projection and access control
- Working on frontend components or data flow
- Debugging synchronization or conflict issues

**When to read [docs/DATABASE.md](docs/DATABASE.md):**
- Working on persistence layer or database queries
- Modifying command sourcing or snapshot logic
- Database schema changes or migrations
- Understanding startup sequence or recovery
- Performance optimization of database operations
- Before production deployment (testing checklist)

**When to read [docs/DEBUGGING.md](docs/DEBUGGING.md):**
- Encountering compilation errors or type errors
- JSaddle FFI issues or type defaulting warnings
- PostgreSQL query errors
- JWT token problems
- WebSocket connection issues
- WASM build or loading problems
- Any error messages or unexpected behavior

**Always read relevant docs before making significant changes to avoid common pitfalls.**

## Project Overview

This is a competences tracking application written in Haskell, using a multi-package Cabal project structure. It consists of:
- **backend**: Server-side component with Office365 OAuth, JWT authentication, WebSocket sync, and static file serving
- **frontend**: Miso-based web frontend compiled to WASM, served via backend
- **common**: Shared code including domain models, commands, and business logic
- **csvconvert**: Utility for converting CSV files to the application's format

The application is built with Nix flakes and uses haskell.nix for reproducible builds.

**See also:** [DEPLOYMENT.md](DEPLOYMENT.md) for production deployment, [docs/TASKS-DESIGN.md](docs/TASKS-DESIGN.md) for task system design.

## Quick Start

### Building

**IMPORTANT: Always use these exact commands for consistency.**

```bash
# Build all packages (STANDARD - use this by default)
cabal build all

# Build individual packages (only when needed)
cabal build competences-common
cabal build competences-backend
cabal build competences-frontend
cabal build competences-csvconvert

# Check build status (shows if build is up to date or has errors)
cabal build all 2>&1 | head -20

# Clean build (when needed, e.g., after changing dependencies)
cabal clean
cabal build all

# Run tests
cabal test all
```

**Build Notes:**
- Always use `cabal build all` as the default build command
- Package names are: `competences-common`, `competences-backend`, `competences-frontend`, `competences-csvconvert`
- The build order is automatically managed by Cabal (common → backend/frontend/csvconvert)
- Warnings are treated as errors in some packages (e.g., unused imports)
- Build output: "Up to date" means successful build with no changes needed

### Running Backend

**Prerequisites:**
1. PostgreSQL database: `psql < backend/schema.sql`
2. Configuration file: `backend/config.json` (see `backend/config.example.json`)

**Start server:**
```bash
cabal run competences-backend -- \
  --port 8080 \
  --database "host=localhost dbname=competences_class_9a" \
  --config backend/config.json \
  --static ./static \
  --init-document data/class_9a.json  # Only for first-time initialization
```

### Frontend Development

**Development mode** (JSaddle, for rapid iteration):
```bash
./start.sh <CLASS_NAME>
```

**Production mode** (WASM):
```bash
# Requires WASM toolchain
nix develop .#wasmShell.x86_64-linux

# Compile and deploy
./deploy_frontend.sh
```

### Nix Development

```bash
# Regular development
nix develop

# WASM development
nix develop .#wasmShell.x86_64-linux
```

## Code Formatting

Use Fourmolu for formatting with settings in `fourmolu.yaml`:
```bash
fourmolu --mode inplace $(find . -name '*.hs' -not -path './dist-newstyle/*')
```

Configuration: 2-space indentation, 100 column limit, leading arrows/commas/imports, no Unicode.

## Common Extensions

The project uses GHC2024 with these additional extensions:
- `DuplicateRecordFields`: Multiple records can share field names
- `NoFieldSelectors`: Disable automatic field selector generation
- `OverloadedRecordDot`: Enable `record.field` syntax
- `OverloadedLabels`: Enable `#field` syntax
- `OverloadedStrings`: String literals are polymorphic
- `GADTs`, `TypeFamilies`: Advanced type features

## Coding Conventions

- Use optics-core for record manipulation (`^.`, `.~`, `%~`, `&`)
- Entity IDs use newtype wrappers from `Competences.Document.Id`
- Translations via `Competences.Frontend.Common.Translate`
- Frontend styling via `Competences.Frontend.View.Tailwind`
- When importing types with `NoFieldSelectors`, use `Type(..)` to access record fields:
  - ✓ `import Competences.Document (Document(..), User(..))`
  - ✗ `import Competences.Document (Document, User)` (won't allow `doc.users` access)

## Essential Patterns

### Document and Commands

All state is held in a `Document`. Changes are expressed as `Command`s:
```haskell
-- Create a command
cmd :: Command
cmd = OnUsers (Create user)

-- Handle command (pure function)
result :: Either Text (Document, AffectedUsers)
result = handleCommand userId cmd document
```

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for details on:
- Command-driven synchronization
- Document projection and access control
- Client-server communication
- Conflict resolution

### Frontend SyncDocument

The frontend maintains local and remote document state:
- `remoteDocument`: Server's authoritative state (projected for students)
- `localDocument`: `remoteDocument + pendingCommand + localChanges`
- `pendingCommand`: Command sent to server, awaiting acknowledgment
- `localChanges`: Queue of commands not yet sent

See `frontend/lib/Competences/Frontend/SyncDocument.hs`.

### Database Persistence

PostgreSQL with command sourcing + snapshots:
- Every command saved to `commands` table
- Periodic snapshots in `snapshots` table
- Fast startup: load latest snapshot + replay recent commands
- Versioned envelopes for schema evolution

See [docs/DATABASE.md](docs/DATABASE.md) for complete persistence details.

### Task System

The task system allows teachers to create and manage learning tasks with competence associations.

**Core Types** (in `common/lib/Competences/Document/Task.hs`):
- `Task` - Atomic work unit with identifier, content, and type (SelfContained or SubTask)
- `TaskGroup` - Organizational unit for related tasks with shared defaults
- `TaskAttributes` - Core attributes: primary/secondary competences, purpose, displayInResources
- `TaskPurpose` - Practice (develops competence) vs Assessment (proves competence)

**Commands** (in `common/lib/Competences/Command/Tasks.hs`):
- `OnTasks` - Create/Delete/Modify self-contained tasks (uses TaskLock)
- `OnTaskGroups` - Create/Delete/Modify task groups (uses TaskGroupLock)
- `OnSubTasks` - Create/Delete/Modify subtasks (uses parent TaskGroupLock)

**Frontend Components**:
- `SelfContainedTaskEditor` - Edit standalone tasks (route: `/tasks`)
  - Three fields: identifier (TaskIdentifier → Text), content (Maybe Text → Text), purpose (enum)
  - Lens conversions handle newtype and nested structure transformations
  - Teacher-only feature, accessible via navigation menu

**Key Patterns**:
```haskell
-- Create a task
let task = Task
      { id = taskId
      , identifier = TaskIdentifier "Book-1.2.3"
      , content = Just "Solve equations..."
      , taskType = SelfContained defaultTaskAttributes
      }
modifySyncDocument r $ Tasks (OnTasks (Create task))

-- Edit task purpose (nested in TaskType → SelfContained → TaskAttributes)
-- Uses custom lens to extract/update purpose from nested structure
purposeViewLens :: Lens' Task TaskPurpose
```

**Gradual Migration**:
- Evidence now has `tasks :: [TaskId]` field
- Old text-based tasks preserved in `oldTasks :: Maybe Text`
- Allows smooth transition from free-text to structured tasks

See [docs/TASKS-DESIGN.md](docs/TASKS-DESIGN.md) and [docs/TASKS-IMPLEMENTATION-PLAN.md](docs/TASKS-IMPLEMENTATION-PLAN.md) for complete design and implementation details.

## Authentication Flow

1. User visits `/` → Office365 login
2. OAuth callback → JWT generation
3. Frontend connects WebSocket with JWT: `ws://host:port/?token=<jwt>`
4. Backend validates JWT, sends `InitialSnapshot` (projected document + user)
5. Real-time sync begins

**Key points:**
- Users must exist in database before login (no auto-creation)
- JWT expires after 24 hours
- All commands currently require Teacher role
- Students see projected document (own data only)

## Dependency Management

**Backend:** Uses haskell.nix materialized dependencies via `cabal.project`

**Frontend (WASM):** Separate `frontend/cabal.project` with index-state pinning and `frontend/cabal.project.freeze`

**Update frontend dependencies:**
```bash
nix develop .#wasmShell.x86_64-linux
cd frontend
wasm32-wasi-cabal update
wasm32-wasi-cabal freeze
cd ..
git add frontend/cabal.project.freeze
git commit -m "Update frontend dependencies"
```

**Note:** Backend and frontend have separate cabal.project files due to different GHC versions (native vs WASM).

## Common Pitfalls

> **For detailed debugging help, see [docs/DEBUGGING.md](docs/DEBUGGING.md)**

**NoFieldSelectors:**
```
ERROR: "No instance for 'HasField "field" ...'"
FIX: Import constructor with (..) - e.g., import Module (Type(..))
```

**JSaddle FFI:**
```
ERROR: "Defaulting the type variable 'name0' to type '[Char]'"
FIX: Add explicit type annotations: ("text" :: Text)
```

**JWT with newtypes:**
```
ERROR: JWT contains "Id {unId = ...}" instead of UUID
FIX: Use UUID.toText userId.unId, NOT show userId
```

## Testing the Full Stack

1. Compile frontend: `./deploy_frontend.sh` (in WASM shell)
2. Start backend: `cabal run competences-backend -- ...` (see Backend section)
3. Visit `http://localhost:8080/`
4. Authenticate with Office365
5. Frontend should load with WebSocket connection

## Production Deployment

See [DEPLOYMENT.md](DEPLOYMENT.md) for complete guide.

Quick start:
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

Deploy:
```bash
sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

## Next Steps and Current Work

### Immediate (Before Production)

1. ✅ **Versioning envelope (COMPLETED)**
2. **Test full deployment workflow** - Complete testing checklist in [docs/DATABASE.md](docs/DATABASE.md)

### Short Term

3. **Snapshot garbage collection** - Implement retention policy
4. **Monitoring and observability** - Structured logging, metrics
5. **Backup and disaster recovery** - Automated PostgreSQL backups

### Medium Term

6. **Student command authorization** - Currently all commands require Teacher role
7. **Property-based testing** - QuickCheck properties for `AffectedUsers` and projection correctness
8. **Performance optimization** - Profile queries, benchmark snapshots

### Long Term

9. **Command replay optimization** - Incremental snapshots or diffs
10. **Multi-tenancy** - Multiple documents in single backend (if managing 10+ classes)
11. **Analytics and reporting** - Student progress, teacher dashboards
