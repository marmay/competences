-- | Evidence queries on the Document.
-- Provides reusable lookups for user evidences.
module Competences.Query.Evidence
  ( -- * Single-entity lookup
    getEvidence
    -- * User-scoped queries
  , userEvidences
  , userEvidencesDesc
  , userEvidencesAsc
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence, EvidenceId, EvidenceIxs, UserId)
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | Lookup an evidence by primary key.
getEvidence :: Document -> EvidenceId -> Maybe Evidence
getEvidence doc evidenceId = Ix.getOne $ doc.evidences Ix.@= evidenceId

-- | All evidences for a user (as IxSet for further filtering).
userEvidences :: Document -> UserId -> Ix.IxSet EvidenceIxs Evidence
userEvidences doc userId = doc.evidences Ix.@= userId

-- | All evidences for a user, sorted newest-first.
userEvidencesDesc :: Document -> UserId -> [Evidence]
userEvidencesDesc doc userId =
  Ix.toDescList (Proxy @Day) $ doc.evidences Ix.@= userId

-- | All evidences for a user, sorted oldest-first.
userEvidencesAsc :: Document -> UserId -> [Evidence]
userEvidencesAsc doc userId =
  Ix.toAscList (Proxy @Day) $ doc.evidences Ix.@= userId
