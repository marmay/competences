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
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_commands_created_at ON commands(created_at);
CREATE INDEX idx_commands_user_id ON commands(user_id);

-- Snapshots table (periodic full document snapshots)
CREATE TABLE snapshots (
  id BIGSERIAL PRIMARY KEY,
  snapshot_id UUID NOT NULL UNIQUE,
  generation BIGINT NOT NULL,
  document_data JSONB NOT NULL,
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

-- Record this schema as version 1
INSERT INTO schema_migrations (version, description)
VALUES (1, 'Initial schema: commands, snapshots, metadata, schema_migrations, startup_log');
