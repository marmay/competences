# Database and Persistence

This document describes the database persistence layer using PostgreSQL with command sourcing and snapshots.

## Current Implementation

PostgreSQL with command sourcing + snapshots

## Command Sourcing + Snapshots Architecture

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

## Database Schema

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

## Snapshot Strategy

**Snapshot triggers:**
1. **Every 25 commands**: Checked after each command in `updateDocument`
2. **Every 15 minutes** (if ≥1 command applied): Periodic timer in Main.hs
3. **On graceful shutdown**: Final snapshot in `gracefulShutdown`
4. **On startup** (non-graceful shutdown recovery): If commands newer than latest snapshot

**Retention policy:**
- Currently: keep all snapshots indefinitely
- Future: implement GC with daily/weekly/monthly retention strategy

## Database Modules

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

## Startup Sequence

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

## Migration from File-Based Storage

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

## Lock Mechanism

- Optimistic concurrency via `Lock`/`Release` commands
- `Release` includes before/after state for conflict detection
- Lock expiration may be added in future

## Testing Checklist

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

**Multi-Instance Testing:**
For production deployment with multiple classes:
- [ ] Create separate databases: `competences_class_9a`, `competences_class_9b`, etc.
- [ ] Create separate config files: `config_9a.json`, `config_9b.json`, etc.
- [ ] Start multiple backend instances on different ports
- [ ] Verify each instance connects to correct database
- [ ] Verify each instance loads correct document
- [ ] Verify instances don't interfere with each other

**WebSocket and Real-Time Sync:**
- [ ] Commands sent over WebSocket are persisted to database
- [ ] Snapshots created during live sessions
- [ ] Multiple users connected to same instance receive broadcasts
- [ ] Projection works correctly for students (only see own evidences)

## Future Work

### Short Term

**Snapshot garbage collection:**
- Implement retention policy (keep all today, 1/day current week, 1/week current month, 1/month forever)
- Add database maintenance job
- Test that old snapshots are safely removed

**Monitoring and observability:**
- Add structured logging
- Track metrics (commands/second, snapshot creation time, replay time)
- Alert on database connection failures
- Dashboard for instance health

**Backup and disaster recovery:**
- Automated PostgreSQL backups (pg_dump)
- Test restore from backup procedure
- Document recovery procedures
- Consider point-in-time recovery (PITR)

### Medium Term

**Performance optimization:**
- Profile database queries
- Add database indexes if needed (currently has indexes on generation, created_at, user_id)
- Consider connection pooling tuning
- Benchmark snapshot creation time on large documents

### Long Term

**Command replay optimization:**
- Currently replays all commands since last snapshot
- Consider incremental snapshots or snapshot diffs
- Investigate CRDT-based approaches for conflict-free merges

**Multi-tenancy in single backend:**
- If managing 10+ classes becomes unwieldy
- Refactor to support multiple documents in single backend
- Add tenant ID to AppState and database queries
- More complex but reduces operational overhead
