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

**When to read [docs/RELEASING.md](docs/RELEASING.md):**
- Creating a new release (read it first - has exact steps, files, and commands)
- Updating version numbers across packages (6 files, two formats)
- Understanding the blobs submodule / flake lock workflow

**Always read relevant docs before making significant changes to avoid common pitfalls.**

## Working Style

When multiple reasonable approaches exist, present them with trade-offs rather than silently choosing.
Flag simplifying assumptions explicitly so they can be reviewed.
The human is not always right — actively look for simpler or more effective approaches and push back constructively.
When uncertain whether something needs human input, err toward asking.

## Project Overview

This is a competences tracking application written in Haskell, using a multi-package Cabal project structure. It consists of:
- **backend**: Server-side component with Office365 OAuth, JWT authentication, WebSocket sync, and static file serving
- **frontend**: Miso-based web frontend compiled to WASM, served via backend
- **common**: Shared code including domain models, commands, and business logic
- **csvconvert**: Utility for converting CSV files to the application's format

The application is built with Nix flakes and uses haskell.nix for reproducible builds.

**Static assets** (WASM binary, CSS, vendored JS) live in a separate repo
[`competences-blobs`](https://github.com/marmay/competences-blobs), included as a
git submodule at `static/`. Source files (`index.js`, `input.css`) live in
`frontend/static-src/` and are copied into `static/` by `deploy_frontend.sh`.
After cloning, run `git submodule update --init` to populate `static/`.

**See also:** [DEPLOYMENT.md](DEPLOYMENT.md) for production deployment.

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
  --ensure-teacher-o365 teacher@school.at  # Optional: ensure a teacher user exists
```

### Frontend Development

**Development mode** (JSaddle, for rapid iteration):
```bash
./start.sh <CLASS_NAME>
```

**Production mode** (WASM):
```bash
# Requires WASM toolchain
nix develop  # wasm toolchain is in the default shell

# Compile and deploy
./deploy_frontend.sh
```

### Nix Development

```bash
# Regular development
nix develop

# WASM development
nix develop  # wasm toolchain is in the default shell
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
- When importing types with `NoFieldSelectors`, use `Type(..)` to access record fields:
  - ✓ `import Competences.Document (Document(..), User(..))`
  - ✗ `import Competences.Document (Document, User)` (won't allow `doc.users` access)

## IxSet-typed Patterns

The codebase uses `ixset-typed` for efficient indexed data storage. **Key principle: remain in IxSet domain as long as possible.** Convert to list only at the final rendering step. Models should store `IxSet`, not pre-converted lists.

### Filtering by Index

Use `@=` to filter by a single index value:
```haskell
-- Get all evidences for a user
doc.evidences Ix.@= userId

-- Chain multiple filters (like AND in SQL)
doc.competences Ix.@= gridId Ix.@= competenceOrder
```

### Filtering by Multiple Values

Use `@+` to filter by multiple values (like IN in SQL):
```haskell
-- Get tasks matching a list of IDs
m.tasks Ix.@+ taskIdList

-- Get users from a set of IDs
m.users Ix.@+ Set.toList userIdSet
```

### Sorted Retrieval

Use `toAscList`/`toDescList` with `Proxy @IndexType` instead of `sortOn`:
```haskell
-- GOOD: Sort by Order index
Ix.toAscList (Proxy @Order) doc.competences

-- GOOD: Sort by Day (descending for most recent first)
Ix.toDescList (Proxy @Day) doc.evidences

-- GOOD: Filter then sort
Ix.toAscList (Proxy @Text) $ m.users Ix.@+ userIdList

-- BAD: Don't do this
sortOn (.order) $ Ix.toList doc.competences
```

### Single Element Lookup

Use `getOne` after filtering for unique lookups:
```haskell
-- Get a single user by ID
Ix.getOne $ doc.users Ix.@= userId

-- Returns Maybe - Nothing if not found or multiple matches
```

### Anti-patterns to Avoid

```haskell
-- BAD: filter + toList
filter (\g -> g.userId == userId) $ Ix.toList grades

-- GOOD: Use index
grades Ix.@= userId

-- BAD: sortOn after toList
sortOn (Down . (.date)) $ Ix.toList evidences

-- GOOD: Use toDescList
Ix.toDescList (Proxy @Day) evidences

-- BAD: Multiple list operations
listToMaybe $ sortOn (Down . (.date)) $ filter (...) $ Ix.toList xs

-- GOOD: Chain IxSet operations, then toDescList
listToMaybe $ Ix.toDescList (Proxy @Day) $ xs Ix.@= userId Ix.@= gridId
```

## UI and View Patterns

The frontend uses Basecoat-inspired design patterns with Tailwind CSS v4.

### Core Principles

1. **Direct CSS classes** via `class_` / `classes` helpers (not TailwindCls enum)
2. **Basecoat color palette**: sky (primary), stone (neutral), red (destructive)
3. **Builder pattern** for component configuration (`with*` functions on Button, Input, etc.)

### View Primitives

View modules live in `Competences.Frontend.View.*`. Import the re-export module for convenience:
```haskell
import Competences.Frontend.View qualified as V
```

Key modules: `Typography`, `Button` (builder pattern), `Input` (builder pattern), `Card`, `Table`, `Layout`, `Badge`, `WindowFrame` (modals/panels), `Color` + `Color/*` (domain-specific color mappings), `Text` (inline text helpers).

```haskell
-- Buttons use builder pattern
Button.button Button.Primary
  & Button.withClick MyAction
  & Button.render "Click Me"

-- Inputs use builder pattern
Input.textInput
  & Input.withPlaceholder "Enter name..."
  & Input.withValue model.name
  & Input.withOnInput SetName
  & Input.renderInput

-- Direct CSS classes
M.div_ [V.class_ "flex gap-4 items-center bg-stone-50 p-4 rounded-lg"] [...]
```

### CSS Build System

CSS is built from `frontend/static-src/input.css` → `static/output.css` via Tailwind v4 CLI (`npm run build:css`, integrated into `deploy_frontend.sh`). The input file imports `tailwindcss` and `basecoat-css`, plus theme overrides. `@source inline()` directives safelist dynamic class names that Tailwind can't detect in Haskell source.

### WindowManagement

Modals and pinned panels are managed by the **WindowManager** (`SyncContext.WindowManager`). Components open windows via `openModal`/`pinDialog`; the **WindowHost** owns all state directly via `WindowEventSink` events and handles stacking, backdrop, and lifecycle. `View.WindowFrame` provides the rendering primitives (`modalFrame`, `modalDialog`, `windowTitleBar`).

### Selector Pattern

The **SelectorDetail** component (`Component.SelectorDetail`) is a reusable left-right layout: selector on the left, detail view on the right. Used by `AssignmentSelector`, `CompetenceGridSelector`, and others. It takes a `SelectorDetailConfig` with mode switching support.

### Unidirectional Data Flow

Data flows up the component tree. Components subscribe to projected state from SyncContext and emit commands upward. Models contain only what the view needs — project from the Document, don't store redundant copies.

## Module Structure Principles

- Don't bundle multiple stateful components in one module
- Tabs belong in separate modules
- Prefer self-contained submodules (`Component/Foo/Bar.hs`) over umbrella re-exports (`Component/Foo.hs`). Umbrellas force every touch of any submodule to invalidate all importers; self-contained modules keep the recompile graph narrow.
- Models must be minimal: project onto exactly what the view needs

### Namespace layering

Frontend code is organized into four namespaces with a strict dependency direction:

```
Page         -- route-bound top-level components
Component    -- Miso components (state internal) + effectful helpers that extend Fragments
Fragment     -- entity-specific pure views with external state (no IO, no SyncContext, no Command)
View         -- entity-agnostic primitives (Layout, Button, Icon, Color, …)
```

A greppable rule: anything under `Fragment.*` must be pure — imports of `SyncContext`, `Command`, or `IO` are forbidden there. Effects always escalate to `Component.*`.

### Fragment / Embed / Component for entity views

Entity views with their own state machine (expansion, hold-to-delete, etc.) that may be reused across parent contexts follow a three-layer split. Example: the detailed task view uses

```
Fragment/Task/Detailed.hs         -- pure: view + state machine + pure update
Component/Task/Detailed/Embed.hs  -- effectful: lens-taking updateTaskDetailed
Component/Task/Detailed.hs        -- full Miso component wrapping Fragment + Embed
```

**Layering rules:**
- Fragment module is pure. No `SyncContext`, no `Command`, no `IO`. Owns the state type, action type, pure update, and view functions.
- Embed module depends only on the Fragment module (not on other entities' components). Exposes a lens-taking effectful update plus any list-rendering helpers that need to dispatch commands. Safe to import from any entity's component module without creating cycles.
- Component module mounts a full Miso component. Embeds the state in its model and delegates its update to the Embed helper.

**Why both forms exist:**
- Component form is an isolation boundary. Narrow projection, filtered updates — one task mounted as a child re-renders only when its own data changes.
- Fragment+Embed form is a composition primitive. A parent renders the view inline and holds the state itself. Used when recursion between two entities must be broken on this side (e.g. a resource rendering its tasks inline via `Task.Detailed.Embed` rather than mounting each as a child component).

Parents that embed the state machine collapse their `update` branch to one line:

```haskell
update (TaskListAction a) =
  TaskComp.updateTaskDetailed #taskListState r TaskListAction a
```

Follow this split only when the state machine is reused across multiple parents. For one-off components, keep state and effects together.

## Command Handler Conventions

Commands follow a standard pattern via `mkEntityCommandContext` (in `Command.Interpret`):
1. Define a `Patch` type for modifications
2. Define command constructors (`Create | Delete | Modify patch`)
3. Implement `applyPatch :: entity -> patch -> Either Text entity`
4. Wire up via `mkEntityCommandContext` with lens, ID accessor, lock, patch applier, and affected-users function

Complex entities (Tasks, DraftTasks, Lessons) extend this pattern with custom cascading and validation logic. `AffectedUsers` helpers are currently scattered per-module (see `docs/TODO.md` for unification plan).

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

### Frontend SyncContext

The frontend synchronization is managed by modules in `Competences.Frontend.SyncContext`:

**SyncDocument** (`SyncContext/SyncDocument.hs`):
- `remoteDocument`: Server's authoritative state (projected for students)
- `localDocument`: `remoteDocument` + pending commands
- `localChanges`: Queue of unconfirmed commands
- Use `subscribeDocument` for document change notifications

**Focused User State** (`SyncContext/UIState.hs`):
- Tracks which user the teacher is currently viewing
- Students always focus on themselves
- Use `subscribeFocusedUser` for focus change notifications

**Projected Subscriptions** (`SyncContext/ProjectedSubscription.hs`):
- Combines document + focused user subscriptions
- Allows components to define efficient projections
- Deduplicates updates when projection hasn't changed

```haskell
-- Define a projection type with only needed data
data MyProjection = MyProjection
  { userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , focusedUser :: !(Maybe User)
  }

-- Projection function filters data by focused user
myProjection :: Document -> Maybe User -> MyProjection
myProjection doc mUser = MyProjection
  { userEvidences = case mUser of
      Nothing -> Ix.empty
      Just u -> doc.evidences Ix.@= u.id
  , focusedUser = mUser
  }

-- Subscribe with projection in component
M.subs = [subscribeWithProjection r myProjection ProjectionChanged]
```

Import via the re-export module: `import Competences.Frontend.SyncContext`

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
- `Task` - Atomic work unit with identifier, content, and flat attributes (primary/secondary competences, purpose, displayInResources)
- `TaskPurpose` - Practice (develops competence) vs Assessment (proves competence)

**Commands** (in `common/lib/Competences/Command/Tasks.hs`):
- `OnTasks` - Create/Delete/Modify tasks (uses TaskLock)

**Frontend Components**:
- `TaskEditor` - Task editor (route: `/tasks`)
  - Teacher-only feature, accessible via navigation menu

**Gradual Migration**:
- Evidence now has `tasks :: [TaskId]` field
- Old text-based tasks preserved in `oldTasks :: Maybe Text`
- Allows smooth transition from free-text to structured tasks


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
nix develop  # wasm toolchain is in the default shell
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

## Current TODOs

See [docs/TODO.md](docs/TODO.md) for the current task list.
