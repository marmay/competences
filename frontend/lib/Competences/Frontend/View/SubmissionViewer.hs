module Competences.Frontend.View.SubmissionViewer
  ( isSubmissionOpen
  )
where

import Competences.Document.Evidence (Evidence)
import Competences.Document.Submission (Submission)

-- | Determine if a submission is "open" (not yet reviewed).
-- TODO: Implement proper logic (check evidence dates or explicit references).
isSubmissionOpen :: [Evidence] -> Submission -> Bool
isSubmissionOpen _evidences _submission = True
