module Competences.Frontend.View.SubmissionViewer
  ( isSubmissionOpen
  )
where

import Competences.Document.Evidence (Evidence (..))
import Competences.Document.Submission (Submission (..))
import Competences.Document.User (UserId)
import Data.Time (utctDay)

-- | Determine if a submission is "open" (not yet reviewed) for a specific user.
-- A submission is open when it was submitted on or after the user's latest evidence date,
-- meaning the teacher hasn't yet reviewed it. Uses @>=@ so same-day submissions count as open
-- (matches 'isAssignmentOpen' semantics).
isSubmissionOpen :: UserId -> [Evidence] -> Submission -> Bool
isSubmissionOpen userId evidences submission =
  let submDay = utctDay submission.submittedAt
      userEvs = [ev | ev <- evidences, ev.userId == Just userId]
   in case userEvs of
        [] -> True
        _ -> submDay >= maximum (map (.date) userEvs)
