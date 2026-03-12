-- | Checkpoint computation for incremental sync.
--
-- Computes deterministic SHA-256 checksums of projected documents,
-- enabling the server to verify whether a client's stored document
-- matches the expected state at a given generation.
module Competences.Backend.Checkpoint
  ( computeDocumentChecksum
  )
where

import Competences.Backend.CAS (computeSHA256)
import Competences.Document (Document)
import Data.Binary qualified as Bin
import Data.Text (Text)

-- | Compute a SHA-256 checksum of a document's Binary encoding.
--
-- The checksum is deterministic because:
-- - IxSet.toList orders by primary index regardless of construction path
-- - Map serializes in key order
-- - Binary encoding is deterministic for identical document states
computeDocumentChecksum :: Document -> Text
computeDocumentChecksum = computeSHA256 . Bin.encode
