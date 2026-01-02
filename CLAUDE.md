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
- When importing types with `NoFieldSelectors`, use `Type(..)` to access record fields:
  - ✓ `import Competences.Document (Document(..), User(..))`
  - ✗ `import Competences.Document (Document, User)` (won't allow `doc.users` access)

## UI and View Patterns

**See [docs/UI-REFACTORING-PROGRESS.md](docs/UI-REFACTORING-PROGRESS.md) for complete details.**

The frontend uses Basecoat-inspired design patterns with Tailwind CSS. All View modules follow consistent styling and component patterns.

### Core Principles

1. **Direct CSS classes** via `class_` helper instead of TailwindCls enum
2. **Basecoat color palette**: sky (primary), stone (neutral), red (destructive)
3. **Builder pattern** for component configuration (`with*` functions)
4. **Semantic components** with clear purpose and consistent API

### View Module Organization

**Core Components** (`Competences.Frontend.View.*`):
- `Typography` - Headings, paragraphs, text utilities (h1-h4, paragraph, lead, small, muted, code, kbd)
- `Badge` - Status indicators with variants (Primary, Secondary, Destructive, Outline)
- `Card` - Content containers (card, cardWithHeader, cardWithFooter, cardFull)
- `Button` - Interactive buttons with Basecoat styling and builder pattern
- `Input` - Form inputs with configuration builders (text, password, email, number, date, textarea)

**Layout & Structure**:
- `Layout` - FlowLayout foundation + higher-level primitives (pageLayout, splitView, formLayout, section)
- `Form` - Form layout helpers + Input re-exports for convenience
- `Table` - Data tables with Basecoat styling (rounded borders, hover states)
- `Modal` - Overlay dialogs with backdrop and proper layering

**Utilities**:
- `DesignTokens` - Basecoat constants (spacing, colors, radii, typography)
- `Tailwind` - Enhanced with `class_` and `classes` helpers for direct CSS

### Common Usage Patterns

```haskell
-- Import pattern for View modules
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button as Button
import Competences.Frontend.View.Input as Input

-- Using Typography
V.h2 "Section Title"
V.paragraph "Description text..."

-- Using Buttons with builder pattern
Button.button Button.Primary
  & Button.withClick MyAction
  & Button.render "Click Me"

-- Using Inputs with configuration
Input.textInput
  & Input.withPlaceholder "Enter name..."
  & Input.withValue model.name
  & Input.withOnInput SetName
  & Input.renderInput

-- Using Cards
V.cardWithHeader "Card Title" (Just "Description")
  [ V.paragraph "Card content..."
  , V.paragraph "More content..."
  ]

-- Using Tables
V.viewTable $ V.defTable
  & #columns .~ myColumns
  & #rows .~ myRows
  & #columnSpec .~ myColumnSpec
  & #rowContents .~ myRowRenderer

-- Using direct CSS classes
M.div_ [V.class_ "flex gap-4 items-center bg-stone-50 p-4 rounded-lg"] [...]
```

### Basecoat UI Integration

This project integrates [Basecoat UI](https://basecoatui.com/) - a framework-agnostic component library built on Tailwind CSS.

**Architecture**:
- **CSS**: Self-hosted from `/static/basecoat.cdn.min.css` (~134KB)
- **JavaScript**: Available in static/ for interactive components (~10KB total)
  - `basecoat.min.js`, `dropdown-menu.min.js`, `popover.min.js`, `tabs.min.js`, `toast.min.js`
- **Integration**: CSS loaded via backend HTML generation in `backend/lib/Competences/Backend/HTTP.hs`

**Current Status**:
- ✅ Basecoat CSS self-hosted and integrated
- ✅ Most View modules already use Basecoat-aligned classes (Button, Input, Card, Badge, Table)
- ⏳ JavaScript components available but not yet integrated (testing needed)
- ⏳ MutationObserver for re-initialization after Miso renders (pending JavaScript integration)

**Testing JavaScript Integration**:
The project currently uses CSS-only Basecoat patterns. JavaScript-powered components (dropdown, tabs, tooltips, toast) are available in static/ but not yet integrated into the Miso app. To test if JavaScript is needed:

1. Start the backend: `cabal run competences-backend -- [options]`
2. Check Network tab in browser DevTools: verify `basecoat.cdn.min.css` loads from `/static/`
3. Inspect components: ensure no styling conflicts between Tailwind and Basecoat
4. Test interactivity: verify current components work without Basecoat JavaScript

**Adding Interactive Components** (when needed):
1. Add JavaScript to HTML in `backend/lib/Competences/Backend/HTTP.hs`
2. Implement MutationObserver in `static/index.js` for re-initialization
3. Create Haskell View modules wrapping Basecoat patterns (e.g., `View/Dropdown.hs`)
4. Let Basecoat handle presentation, Haskell handle business logic

**See also**: Plan file `/home/markus/.claude/plans/dapper-fluttering-globe.md` for complete integration plan.

### CSS Build System

**Production CSS**: Generated via Tailwind CLI, served from `/static/output.css`
- **Size**: ~321KB minified (includes full Basecoat color palette via safelist)
- **Build**: `npm run build:css` (integrated into `deploy_frontend.sh`)
- **Config**: `tailwind.config.js` with safelist for dynamic class names

**Why safelist?** Tailwind's content scanner can't detect dynamically constructed class names in Haskell code (via `class_` helper), so we safelist the complete Basecoat color palette to ensure all needed classes are available.

### Migration Guide

When updating components to use new View patterns:

1. **Replace old imports**:
   ```haskell
   -- Old
   import Competences.Frontend.View.Colors qualified as C
   import Competences.Frontend.View.Text qualified as V

   -- New
   import Competences.Frontend.View.Typography qualified as Typography
   import Competences.Frontend.View.Button as Button
   ```

2. **Replace TailwindCls enum usage**:
   ```haskell
   -- Old
   M.div_ [T.tailwind [T.Flex, T.Gap4, T.ItemsCenter]] [...]

   -- New
   M.div_ [V.class_ "flex gap-4 items-center"] [...]
   ```

3. **Use View primitives**:
   ```haskell
   -- Old
   M.h2_ [T.tailwind [T.Text2xl, T.FontBold]] [M.text "Title"]

   -- New
   Typography.h2 "Title"
   ```

4. **Test thoroughly** - View changes affect rendering, verify UI looks correct

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
