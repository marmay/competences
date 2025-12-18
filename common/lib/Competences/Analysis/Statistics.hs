module Competences.Analysis.Statistics
  ( userEvidenceByActivity
  , ActivityStats (..)
  ) where

import Competences.Document.Evidence
  ( Evidence (..)
  , ActivityType (..)
  )
import Competences.Document.User (UserId)
import Competences.Document (Document (..))
import Data.IxSet.Typed qualified as Ix

-- | Statistics for evidence by activity type, including weight
-- For now, weight is always 1, but this allows for future weighting based on task analysis
data ActivityStats = ActivityStats
  { activityType :: !ActivityType
  , count :: !Int
  , weight :: !Double  -- Always 1 for now, but can be extended later
  }
  deriving (Eq, Show)

-- | Compute evidence statistics by activity type for a specific user
-- Returns a list of ActivityStats with count and weight (currently always 1)
userEvidenceByActivity :: Document -> UserId -> [ActivityStats]
userEvidenceByActivity doc userId =
  let
    -- Get all evidence that includes this user using the UserId index
    userEvidences = Ix.toList $ doc.evidences Ix.@= userId
    
    -- Group by activity type and count
    activityGroups = foldl' accumulateActivities [] userEvidences
    
    -- Convert to ActivityStats with weight = 1
    stats = map (\ (activityType, count) -> ActivityStats activityType count 1.0) activityGroups
   in
    stats
  where
    -- Helper function to accumulate activity counts
    accumulateActivities :: [(ActivityType, Int)] -> Evidence -> [(ActivityType, Int)]
    accumulateActivities [] evidence = [(evidence.activityType, 1)]
    accumulateActivities ((actType, count) : rest) evidence
      | actType == evidence.activityType = (actType, count + 1) : rest
      | otherwise = (actType, count) : accumulateActivities rest evidence