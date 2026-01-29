-- | Mastery classification queries
-- Pure functions for determining student mastery status at competence levels
-- Designed for reuse across analytics views and conditional task assignment
module Competences.Query.Mastery
  ( -- * Mastery Classification
    MasteryStatus (..)
  , classifyMasteryConstrained

    -- * Cross-Level Ability Bounds
  , AbilityBounds (..)
  , abilityFloor
  , abilityCeiling
  , boundsHasIndividual

    -- * Document Queries
  , getUserMastery
  , getClassMasteryStats
  , getClassMasteryWithStudents

    -- * Low-level helpers (for conditional task predicates)
  , getConstrainedObservations
  , hasSuccessStreak
  , isCurrentlyProficient
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Competence (CompetenceId, CompetenceLevelId, Level)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..), SocialForm (..))
import Competences.Document.User (User (..), UserId)
import Competences.Query.Evidence qualified as QEvidence
import Competences.Query.User qualified as QUser
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)

-- | Mastery status for a student at a specific competence-level
--
-- Categories are mutually exclusive and ordered by "proficiency":
-- - StreakTwoPlus: Student has demonstrated mastery (2+ consecutive successes)
-- - OneSuccess: Student is emerging (1 recent success)
-- - OnlySillyMistakes: Student shows understanding but makes silly errors
-- - MasteryNotYet: Student's most recent real attempt was unsuccessful
-- - NotTried: Student has no observations for this level
data MasteryStatus
  = StreakTwoPlus
  -- ^ 2+ consecutive SelfReliant observations (SillyMistakes don't break streak)
  | OneSuccess
  -- ^ Latest "real" attempt is SelfReliant, but no streak yet
  | OnlySillyMistakes
  -- ^ Only SelfReliantWithSillyMistakes observations, no SelfReliant
  | MasteryNotYet
  -- ^ Latest real attempt is WithSupport or Ability.NotYet
  | NotTried
  -- ^ No observations for this competence-level
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- | Constrained ability bounds from cross-level inference within one evidence.
--
-- When a student has observations at multiple levels of the same competence,
-- we infer bounds:
-- - noLowerThan (floor): best ability at the target level or any higher level
-- - noHigherThan (ceiling): worst ability at the target level or any lower level
--
-- Sum type with exactly the 3 meaningful states (no impossible Nothing/Nothing).
data AbilityBounds
  = FromAbove !Ability !Bool
  -- ^ Observations only strictly above target level.
  --   Ability = noLowerThan (floor); Bool = hasIndividual at/above target.
  | FromBelow !Ability
  -- ^ Observations only strictly below target level.
  --   Ability = noHigherThan (ceiling). No hasIndividual (no obs at/above target).
  | FromBoth !Ability !Ability !Bool
  -- ^ Observations at target level (and possibly others), giving both bounds.
  --   First Ability = noLowerThan (floor), Second = noHigherThan (ceiling),
  --   Bool = hasIndividual at/above target.
  deriving (Eq, Show, Generic)

-- | Get the noLowerThan (floor) bound, if available.
abilityFloor :: AbilityBounds -> Maybe Ability
abilityFloor (FromAbove a _) = Just a
abilityFloor (FromBelow _) = Nothing
abilityFloor (FromBoth a _ _) = Just a

-- | Get the noHigherThan (ceiling) bound, if available.
abilityCeiling :: AbilityBounds -> Maybe Ability
abilityCeiling (FromAbove _ _) = Nothing
abilityCeiling (FromBelow a) = Just a
abilityCeiling (FromBoth _ a _) = Just a

-- | Whether any observation at/above the target level has SocialForm Individual.
boundsHasIndividual :: AbilityBounds -> Bool
boundsHasIndividual (FromAbove _ b) = b
boundsHasIndividual (FromBelow _) = False
boundsHasIndividual (FromBoth _ _ b) = b

-- | Classify mastery from a unified timeline of ability bounds (sorted newest-first).
--
-- Uses both bounds for classification:
-- - noHigherThan (ceiling) for negative classification (MasteryNotYet) — acts as a veto
-- - noLowerThan (floor) for positive classification (streak counting for +1, +2)
--
-- For StreakTwoPlus, at least one contributing evidence must have SocialForm Individual.
classifyMasteryConstrained :: [AbilityBounds] -> MasteryStatus
classifyMasteryConstrained [] = NotTried
classifyMasteryConstrained bounds
  -- Negative check: latest noHigherThan value vetoes if negative
  | Just latestCeiling <- findLatestRealCeiling bounds
  , latestCeiling == WithSupport || latestCeiling == NotYet =
      MasteryNotYet
  -- Positive check: count streak from noLowerThan values
  | otherwise =
      let (streakLen, hasIndiv) = countConstrainedStreak bounds
       in case () of
            _
              | (streakLen :: Int) >= 2, hasIndiv -> StreakTwoPlus
              | streakLen >= 1 -> OneSuccess
              | otherwise -> classifyRemaining bounds
  where
    -- Get the noHigherThan (ceiling) from the newest entry.
    -- FromAbove entries have no ceiling — that means "no negative info", not "keep looking".
    -- Older ceilings must not veto newer positive evidence.
    findLatestRealCeiling [] = Nothing
    findLatestRealCeiling (b : _) = abilityCeiling b

    -- Count consecutive SelfReliant from noLowerThan values (newest first).
    -- Skip entries where floor is SillyMistakes or Nothing.
    -- Also track whether any streak entry has Individual.
    countConstrainedStreak = go 0 False
      where
        go !n !indiv [] = (n, indiv)
        go !n !indiv (b : rest) = case abilityFloor b of
          Just SelfReliant -> go (n + 1) (indiv || boundsHasIndividual b) rest
          Just SelfReliantWithSillyMistakes -> go n indiv rest -- skip, don't break
          Nothing -> go n indiv rest -- no floor info, skip
          _ -> (n, indiv) -- WithSupport or NotYet breaks the streak

    -- Classify when there's no positive streak: check floors for remaining info
    classifyRemaining :: [AbilityBounds] -> MasteryStatus
    classifyRemaining bs = case findLatestRealFloor bs of
      Just SelfReliant -> OneSuccess -- shouldn't normally happen (streak would be >= 1)
      Just WithSupport -> MasteryNotYet
      Just NotYet -> MasteryNotYet
      Just SelfReliantWithSillyMistakes -> OnlySillyMistakes
      Nothing ->
        -- No real floor found; only SillyMistakes floors count as OnlySillyMistakes.
        -- Ceiling-only entries (FromBelow) don't imply the student tried at this level.
        if any ((/= Nothing) . abilityFloor) bs
          then OnlySillyMistakes
          else NotTried

    findLatestRealFloor :: [AbilityBounds] -> Maybe Ability
    findLatestRealFloor [] = Nothing
    findLatestRealFloor (b : rest) = case abilityFloor b of
      Nothing -> findLatestRealFloor rest
      Just SelfReliantWithSillyMistakes -> findLatestRealFloor rest
      Just a -> Just a

-- ============================================================================
-- Document Queries
-- ============================================================================

-- | Get constrained observation bounds for a (user, competence, level).
--
-- For each evidence (newest first), extracts all observations for the target
-- competence (any level), aggregates worst ability per level within the evidence,
-- then computes cross-level noLowerThan/noHigherThan bounds for the target level.
--
-- Returns a unified timeline of 'AbilityBounds', sorted newest-first.
getConstrainedObservations :: Document -> UserId -> CompetenceLevelId -> [AbilityBounds]
getConstrainedObservations doc userId (compId, targetLevel) =
  let userEvidences = QEvidence.userEvidencesDesc doc userId
   in mapMaybe (evidenceToBounds compId targetLevel) userEvidences

-- | Compute AbilityBounds for a single evidence at a target competence/level.
-- Returns Nothing if the evidence has no observations for this competence.
evidenceToBounds :: CompetenceId -> Level -> Evidence -> Maybe AbilityBounds
evidenceToBounds compId targetLevel ev =
  let -- Extract all observations for this competence, grouped by level
      relevantObs =
        [ (lvl, obs)
        | obs <- Ix.toList ev.observations
        , let (cId, lvl) = obs.competenceLevelId
        , cId == compId
        ]
   in case relevantObs of
        [] -> Nothing
        _ ->
          let -- Aggregate: worst ability per level (Haskell maximum = domain worst)
              aggMap = Map.fromListWith max [(lvl, obs.ability) | (lvl, obs) <- relevantObs]
              -- Compute bounds
              aboveOrAt = [(lvl, a) | (lvl, a) <- Map.toList aggMap, lvl >= targetLevel]
              belowOrAt = [(lvl, a) | (lvl, a) <- Map.toList aggMap, lvl <= targetLevel]
              mFloor = if null aboveOrAt then Nothing else Just (minimum $ map snd aboveOrAt)
              mCeiling = if null belowOrAt then Nothing else Just (maximum $ map snd belowOrAt)
              -- Check for Individual social form at/above target
              hasIndiv = any (\(lvl, obs) -> lvl >= targetLevel && obs.socialForm == Individual) relevantObs
           in case (mFloor, mCeiling) of
                (Just f, Nothing) -> Just (FromAbove f hasIndiv)
                (Nothing, Just c) -> Just (FromBelow c)
                (Just f, Just c) -> Just (FromBoth f c hasIndiv)
                (Nothing, Nothing) -> Nothing -- shouldn't happen since relevantObs is non-empty

-- | Get mastery status for one (user, competence-level)
--
-- Uses cross-level constrained observations for classification.
getUserMastery :: Document -> UserId -> CompetenceLevelId -> MasteryStatus
getUserMastery doc userId compLevelId =
  classifyMasteryConstrained $ getConstrainedObservations doc userId compLevelId

-- | Get class-wide mastery statistics for one competence-level
--
-- Returns a map from MasteryStatus to count of students in that category
getClassMasteryStats :: Document -> CompetenceLevelId -> Map MasteryStatus Int
getClassMasteryStats doc compLevelId =
  let studs = QUser.students doc
      statuses = map (\u -> getUserMastery doc u.id compLevelId) studs
   in foldl' countStatus Map.empty statuses
  where
    countStatus acc status = Map.insertWith (+) status 1 acc

-- | Get class-wide mastery with student lists for one competence-level
--
-- Returns a map from MasteryStatus to list of students in that category,
-- sorted alphabetically by student name.
getClassMasteryWithStudents :: Document -> CompetenceLevelId -> Map MasteryStatus [User]
getClassMasteryWithStudents doc compLevelId =
  let studs = QUser.students doc
      pairs = map (\u -> (getUserMastery doc u.id compLevelId, u)) studs
   in Map.map (sortOn (.name)) $ foldl' groupStudent Map.empty pairs
  where
    groupStudent acc (status, user) = Map.insertWith (++) status [user] acc

-- ============================================================================
-- Predicates for Conditional Task Assignment
-- ============================================================================

-- | Predicate: does user have a success streak >= n for this level?
--
-- Uses cross-level constrained observations. Streak is counted from
-- noLowerThan (floor) values, requiring at least one Individual observation
-- for streaks >= 2.
hasSuccessStreak :: Int -> Document -> UserId -> CompetenceLevelId -> Bool
hasSuccessStreak n doc userId compLevelId =
  let bounds = getConstrainedObservations doc userId compLevelId
      (streakLen, hasIndiv) = countFloorStreak bounds
   in if n >= 2
        then streakLen >= n && hasIndiv
        else streakLen >= n
  where
    countFloorStreak = go 0 False
    go !cnt !indiv [] = (cnt, indiv)
    go !cnt !indiv (b : rest) = case abilityFloor b of
      Just SelfReliant -> go (cnt + 1) (indiv || boundsHasIndividual b) rest
      Just SelfReliantWithSillyMistakes -> go cnt indiv rest
      Nothing -> go cnt indiv rest
      _ -> (cnt, indiv)

-- | Predicate: is user "proficient" (streak >= 2 with at least one Individual)?
isCurrentlyProficient :: Document -> UserId -> CompetenceLevelId -> Bool
isCurrentlyProficient = hasSuccessStreak 2
