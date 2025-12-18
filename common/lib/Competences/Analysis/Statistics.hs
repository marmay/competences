module Competences.Analysis.Statistics
  ( userEvidenceByActivity
  , ActivityStats (..)
  , allUsersEvidenceByActivity
  , UserActivityStats (..)
  , userObservationsByAbility
  , AbilityStats (..)
  , allUsersObservationsByAbility
  , UserAbilityStats (..)
  ) where

import Competences.Document.Evidence
  ( Evidence (..)
  , ActivityType (..)
  , Ability (..)
  , Observation (..)
  )
import Competences.Document.User (UserId, User (..))
import Competences.Document (Document (..))
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)

-- | Statistics for evidence by activity type, including weight
-- For now, weight is always 1, but this allows for future weighting based on task analysis
data ActivityStats = ActivityStats
  { activityType :: !ActivityType
  , count :: !Int
  , weight :: !Double  -- Always 1 for now, but can be extended later
  }
  deriving (Eq, Show)

-- | Statistics for observations by ability
data AbilityStats = AbilityStats
  { ability :: !Ability
  , count :: !Int
  , weight :: !Double  -- Always 1 for now, but can be extended later
  }
  deriving (Eq, Show)

-- | User-specific activity statistics for overview
data UserActivityStats = UserActivityStats
  { userId :: !UserId
  , userName :: !Text
  , activityStats :: ![ActivityStats]
  , totalCount :: !Int
  }
  deriving (Eq, Show)

-- | User-specific ability statistics for overview
data UserAbilityStats = UserAbilityStats
  { userId :: !UserId
  , userName :: !Text
  , abilityStats :: ![AbilityStats]
  , totalCount :: !Int
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

-- | Compute evidence statistics by activity type for all users
-- Returns a list of UserActivityStats with activity breakdown for each user
allUsersEvidenceByActivity :: Document -> [UserActivityStats]
allUsersEvidenceByActivity doc =
  let
    -- Get all users
    users = Ix.toList doc.users
    
    -- Compute statistics for each user
    userStats = map computeUserStats users
  in
    userStats
  where
    computeUserStats user =
      let
        stats = userEvidenceByActivity doc user.id
        totalCount = sum (map (.count) stats)
      in
        UserActivityStats
          { userId = user.id
          , userName = user.name
          , activityStats = stats
          , totalCount = totalCount
          }

-- | Compute observation statistics by ability for a specific user
-- Returns a list of AbilityStats with count and weight (currently always 1)
userObservationsByAbility :: Document -> UserId -> [AbilityStats]
userObservationsByAbility doc userId =
  let
    -- Get all evidence that includes this user
    userEvidences = Ix.toList $ doc.evidences Ix.@= userId
    
    -- Extract all observations from the evidence
    allObservations = concatMap (Ix.toList . (.observations)) userEvidences
    
    -- Group by ability and count
    abilityGroups = foldl' accumulateAbilities [] allObservations
    
    -- Convert to AbilityStats with weight = 1
    stats = map (\(ability, count) -> AbilityStats ability count 1.0) abilityGroups
  in
    stats
  where
    -- Helper function to accumulate ability counts
    accumulateAbilities :: [(Ability, Int)] -> Observation -> [(Ability, Int)]
    accumulateAbilities [] observation = [(observation.ability, 1)]
    accumulateAbilities ((ability, count) : rest) observation
      | ability == observation.ability = (ability, count + 1) : rest
      | otherwise = (ability, count) : accumulateAbilities rest observation

-- | Compute observation statistics by ability for all users
-- Returns a list of UserAbilityStats with ability breakdown for each user
allUsersObservationsByAbility :: Document -> [UserAbilityStats]
allUsersObservationsByAbility doc =
  let
    -- Get all users
    users = Ix.toList doc.users
    
    -- Compute statistics for each user
    userStats = map computeUserStats users
  in
    userStats
  where
    computeUserStats user =
      let
        stats = userObservationsByAbility doc user.id
        totalCount = sum (map (.count) stats)
      in
        UserAbilityStats
          { userId = user.id
          , userName = user.name
          , abilityStats = stats
          , totalCount = totalCount
          }