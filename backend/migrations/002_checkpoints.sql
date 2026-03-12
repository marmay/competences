-- Migration 002: Checkpoint-based incremental sync
--
-- Adds infrastructure for tracking command audiences and per-user checkpoints,
-- enabling incremental sync on WebSocket reconnection.

-- Add audience column to commands table
-- Values: 'all', 'teachers', 'teachers_and_recipients', 'recipients'
ALTER TABLE commands ADD COLUMN audience TEXT NOT NULL DEFAULT 'all';

-- Specific recipients (only for 'teachers_and_recipients' and 'recipients' audiences)
CREATE TABLE command_recipients (
  generation BIGINT NOT NULL REFERENCES commands(generation),
  user_id UUID NOT NULL,
  PRIMARY KEY (generation, user_id)
);
CREATE INDEX idx_command_recipients_user_gen ON command_recipients(user_id, generation);

-- Per-user checkpoint hashes
-- checkpoint_id is the external UUID the client uses; generation is backend-internal
CREATE TABLE checkpoints (
  checkpoint_id UUID PRIMARY KEY,
  generation BIGINT NOT NULL,
  user_id UUID NOT NULL,
  checksum TEXT NOT NULL,  -- SHA-256 hex (64 chars)
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX idx_checkpoints_user ON checkpoints(user_id);

-- Record this migration
INSERT INTO schema_migrations (version, description)
VALUES (2, 'Checkpoint-based incremental sync: command audience, recipients, checkpoints');
