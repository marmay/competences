CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Commands and snapshots use versioned envelopes for schema evolution
-- Structure: { "version": <int>, "userId": <uuid>, "payload": <actual data> }
-- This allows backward-compatible migrations when Command/Document structure changes.

-- Schema version tracking
CREATE TABLE schema_migrations (
  version INTEGER PRIMARY KEY,
  description TEXT NOT NULL,
  applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Commands table (event log / command sourcing)
CREATE TABLE commands (
  generation BIGSERIAL PRIMARY KEY,
  command_id UUID NOT NULL UNIQUE,
  user_id UUID NOT NULL,
  command_data JSONB NOT NULL,
  audience TEXT NOT NULL DEFAULT 'all',
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_commands_created_at ON commands(created_at);
CREATE INDEX idx_commands_user_id ON commands(user_id);

-- Snapshots table (periodic full document snapshots)
CREATE TABLE snapshots (
  id BIGSERIAL PRIMARY KEY,
  snapshot_id UUID NOT NULL UNIQUE,
  generation BIGINT NOT NULL,
  document_data TEXT NOT NULL,
  protected BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  FOREIGN KEY (generation) REFERENCES commands(generation)
);

CREATE INDEX idx_snapshots_generation ON snapshots(generation DESC);
CREATE INDEX idx_snapshots_created_at ON snapshots(created_at DESC);

-- Metadata for snapshot timing
CREATE TABLE metadata (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Initialize metadata for snapshot tracking
INSERT INTO metadata (key, value) VALUES
  ('last_snapshot_generation', '0'),
  ('last_snapshot_time', NOW()::TEXT)
ON CONFLICT (key) DO NOTHING;

-- Startup log for tracking backend instances
CREATE TABLE startup_log (
  id BIGSERIAL PRIMARY KEY,
  instance_id UUID NOT NULL,
  started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  stopped_at TIMESTAMPTZ,
  schema_version INTEGER NOT NULL,
  initial_generation BIGINT NOT NULL,
  init_document_provided BOOLEAN NOT NULL,
  metadata JSONB
);

CREATE INDEX idx_startup_log_started_at ON startup_log(started_at DESC);

-- Command recipients (for commands with audience 'teachers_and_recipients' or 'recipients')
CREATE TABLE command_recipients (
  generation BIGINT NOT NULL REFERENCES commands(generation),
  user_id UUID NOT NULL,
  PRIMARY KEY (generation, user_id)
);
CREATE INDEX idx_command_recipients_user_gen ON command_recipients(user_id, generation);

-- Record schema versions
INSERT INTO schema_migrations (version, description)
VALUES (1, 'Initial schema: commands, snapshots, metadata, schema_migrations, startup_log');
INSERT INTO schema_migrations (version, description)
VALUES (2, 'Command audience tracking for incremental sync');
INSERT INTO schema_migrations (version, description)
VALUES (3, 'Add protected flag for snapshot garbage collection');
INSERT INTO schema_migrations (version, description)
VALUES (4, 'Convert snapshot document_data from JSONB to TEXT for byte-exact comparison');
