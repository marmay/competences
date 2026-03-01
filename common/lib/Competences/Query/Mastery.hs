-- | Mastery classification queries
-- Pure functions for determining student mastery status at competence levels
-- Designed for reuse across analytics views and conditional task assignment
module Competences.Query.Mastery
  ( -- * Mastery Classification
    MasteryStatus (..)
  , classifyMasteryConstrained
  , classifyWithReasoning

    -- * Cross-Level Ability Bounds
  , AbilityBounds (..)
  , EvidenceQuality (..)
  , abilityFloor
  , abilityCeiling
  , boundsHasIndividual
  , boundsHasAssessmentActivity

    -- * Pure Testable Core
  , LevelObservation (..)
  , EvidenceCompetenceObservations
  , ObservationTimeline
  , observationBounds
  , classifyAllLevels

    -- * Document Queries
  , getUserMastery
  , getUserMasteryWithReasoning
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
import Competences.Document.Competence (CompetenceId, CompetenceLevelId, Level, allLevels)
import Competences.Document.ActivityType (ActivityType (..), isAssessmentActivity)
import Competences.Document.Evidence (Ability (..), Evidence (..), EvidenceId, Observation (..), SocialForm (..))
import Competences.Document.User (User (..), UserId)
import Competences.Query.Evidence qualified as QEvidence
import Competences.Query.User qualified as QUser
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)

-- | Mastery status for a student at a specific competence-level
--
-- Categories are mutually exclusive and ordered by "proficiency":
-- - StreakTwoAssessed: Assessment-ready (2+ streak with Individual AND Exam/Conversation)
-- - StreakTwoPlus: Student has demonstrated mastery (2+ consecutive successes)
-- - OneSuccess: Student is emerging (1 recent success)
-- - OnlySillyMistakes: Student shows understanding but makes silly errors
-- - MasteryNotYet: Student's most recent real attempt was unsuccessful
-- - NotTried: Student has no observations for this level
data MasteryStatus
  = StreakTwoAssessed
  -- ^ 2+ consecutive SelfReliant with Individual AND Exam/Conversation activity (++2)
  | StreakTwoPlus
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

-- | Quality flags for observations at/above the target level within one evidence.
data EvidenceQuality = EvidenceQuality
  { hasIndividual :: !Bool
  -- ^ At least one observation at/above target has SocialForm Individual.
  , hasAssessmentActivity :: !Bool
  -- ^ The evidence's activity type is Exam or Conversation.
  }
  deriving (Eq, Show, Generic)

-- | Constrained ability bounds from cross-level inference within one evidence.
--
-- When a student has observations at multiple levels of the same competence,
-- we infer bounds:
-- - noLowerThan (floor): best ability at the target level or any higher level
-- - noHigherThan (ceiling): worst ability at the target level or any lower level
--
-- Sum type with exactly the 3 meaningful states (no impossible Nothing/Nothing).
data AbilityBounds
  = FromAbove !Ability !EvidenceQuality
  -- ^ Observations only strictly above target level.
  --   Ability = noLowerThan (floor); EvidenceQuality for obs at/above target.
  | FromBelow !Ability
  -- ^ Observations only strictly below target level.
  --   Ability = noHigherThan (ceiling). No positive quality (no obs at/above target).
  | FromBoth !Ability !Ability !EvidenceQuality
  -- ^ Observations at target level (and possibly others), giving both bounds.
  --   First Ability = noLowerThan (floor), Second = noHigherThan (ceiling),
  --   EvidenceQuality for obs at/above target.
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
boundsHasIndividual (FromAbove _ q) = q.hasIndividual
boundsHasIndividual (FromBelow _) = False
boundsHasIndividual (FromBoth _ _ q) = q.hasIndividual

-- | Whether the evidence has an assessment activity type (Exam or Conversation).
boundsHasAssessmentActivity :: AbilityBounds -> Bool
boundsHasAssessmentActivity (FromAbove _ q) = q.hasAssessmentActivity
boundsHasAssessmentActivity (FromBelow _) = False
boundsHasAssessmentActivity (FromBoth _ _ q) = q.hasAssessmentActivity

-- ============================================================================
-- Pure Testable Core
-- ============================================================================

-- | One observation stripped to just what mastery logic needs.
-- No IDs, no dates, no IxSets.
-- The 'activityType' is per-evidence metadata (all observations in one evidence
-- share the same activity type), stored per-observation for simplicity.
data LevelObservation = LevelObservation
  { level :: !Level
  , ability :: !Ability
  , socialForm :: !SocialForm
  , activityType :: !ActivityType
  }
  deriving (Eq, Show, Generic)

-- | Observations from a single evidence, already filtered to one competence.
type EvidenceCompetenceObservations = [LevelObservation]

-- | Timeline of evidences (newest first), each already filtered to one competence.
type ObservationTimeline = [EvidenceCompetenceObservations]

-- | Compute 'AbilityBounds' for a target level from one evidence's observations.
--
-- Returns 'Nothing' if the observation list is empty.
observationBounds :: Level -> EvidenceCompetenceObservations -> Maybe AbilityBounds
observationBounds _targetLevel [] = Nothing
observationBounds targetLevel obs =
  let -- Aggregate: worst ability per level (Haskell maximum = domain worst)
      aggMap = Map.fromListWith max [(o.level, o.ability) | o <- obs]
      -- Compute bounds
      aboveOrAt = [a | (lvl, a) <- Map.toList aggMap, lvl >= targetLevel]
      belowOrAt = [a | (lvl, a) <- Map.toList aggMap, lvl <= targetLevel]
      mFloor = if null aboveOrAt then Nothing else Just (minimum aboveOrAt)
      mCeiling = if null belowOrAt then Nothing else Just (maximum belowOrAt)
      -- Check for Individual social form at/above target
      hasIndiv = any (\o -> o.level >= targetLevel && o.socialForm == Individual) obs
      -- Check for assessment activity type (Exam or Conversation) at/above target
      hasAssessAct = any (\o -> o.level >= targetLevel && isAssessmentActivity o.activityType) obs
      quality = EvidenceQuality hasIndiv hasAssessAct
   in case (mFloor, mCeiling) of
        (Just f, Nothing) -> Just (FromAbove f quality)
        (Nothing, Just c) -> Just (FromBelow c)
        -- Both floor and ceiling present: always keep both bounds.
        -- This covers direct observations at the target level and the
        -- "surrounding" case (observations above and below but not at the
        -- target). Keeping both preserves monotonicity — a failure below
        -- the target level must still constrain intermediate levels.
        (Just f, Just c) -> Just (FromBoth f c quality)
        (Nothing, Nothing) -> Nothing -- shouldn't happen since obs is non-empty

-- | Classify mastery at all levels from a timeline of evidences (newest first).
--
-- Each inner list is the observations within one evidence for a single competence,
-- already filtered to that competence. Returns a 'Map' from each 'Level' to its
-- 'MasteryStatus'.
classifyAllLevels :: ObservationTimeline -> Map Level MasteryStatus
classifyAllLevels timeline =
  Map.fromList
    [ (lvl, classifyMasteryConstrained $ mapMaybe (observationBounds lvl) timeline)
    | lvl <- allLevels
    ]

-- ============================================================================
-- Classification
-- ============================================================================

-- | Classify mastery from a unified timeline of ability bounds (sorted newest-first).
--
-- Uses both bounds for classification:
-- - noHigherThan (ceiling) for negative classification (MasteryNotYet) — acts as a veto
-- - noLowerThan (floor) for positive classification (streak counting for +1, +2, ++2)
--
-- Bias towards success: only direct observation at the target level (FromBoth) can
-- trigger MasteryNotYet. Indirect evidence (FromAbove, FromBelow) contributes to
-- positive classification but never condemns.
--
-- For StreakTwoPlus, at least one contributing evidence must have SocialForm Individual.
-- For StreakTwoAssessed (++2), additionally requires at least one Exam/Conversation.
classifyMasteryConstrained :: [AbilityBounds] -> MasteryStatus
classifyMasteryConstrained bounds =
  fst $ classifyWithReasoning (zip [(0 :: Int) ..] bounds)

-- | Like 'classifyMasteryConstrained' but carries tags through and returns the
-- tags of bounds that influenced the mastery decision.
--
-- Each element is @(tag, bounds)@. The returned list contains the tags of all
-- bounds that contributed to the classification:
-- - Veto path: the tag of the bound that triggered 'MasteryNotYet'
-- - Streak path: tags of all streak-contributing bounds + the streak-breaker (if any)
-- - Remaining path: the tag of the 'FromBoth' bound found by the floor scan
classifyWithReasoning :: [(a, AbilityBounds)] -> (MasteryStatus, [a])
classifyWithReasoning [] = (NotTried, [])
classifyWithReasoning tagged
  -- Negative check: latest direct ceiling vetoes if negative.
  | Just (latestCeiling, vetoTag) <- findLatestDirectCeilingT tagged
  , latestCeiling == WithSupport || latestCeiling == NotYet =
      (MasteryNotYet, [vetoTag])
  -- Positive check: count streak from noLowerThan values
  | otherwise =
      let (streakLen, hasIndiv, hasAssessed, streakTags) = countConstrainedStreakT tagged
       in case () of
            _
              | (streakLen :: Int) >= 2, hasIndiv, hasAssessed -> (StreakTwoAssessed, streakTags)
              | streakLen >= 2, hasIndiv -> (StreakTwoPlus, streakTags)
              | streakLen >= 1 -> (OneSuccess, streakTags)
              | otherwise -> classifyRemainingT tagged
  where
    -- Tagged variant of findLatestDirectCeiling: returns (ceiling, tag)
    findLatestDirectCeilingT [] = Nothing
    findLatestDirectCeilingT ((tag, FromBoth _ c _) : _) = Just (c, tag)
    findLatestDirectCeilingT ((tag, FromBelow c) : rest)
      | c == WithSupport || c == NotYet = Just (c, tag)
      | otherwise = findLatestDirectCeilingT rest
    findLatestDirectCeilingT ((_, b) : rest) = case abilityFloor b of
      Just SelfReliant -> Nothing
      Just SelfReliantWithSillyMistakes -> Nothing
      _ -> findLatestDirectCeilingT rest

    -- Tagged variant of countConstrainedStreak: also collects tags
    countConstrainedStreakT = go 0 False False []
      where
        go !n !indiv !assessed !tags [] = (n, indiv, assessed, tags)
        go !n !indiv !assessed !tags ((tag, b) : rest) = case abilityFloor b of
          Just SelfReliant ->
            go (n + 1) (indiv || boundsHasIndividual b) (assessed || boundsHasAssessmentActivity b) (tag : tags) rest
          Just SelfReliantWithSillyMistakes -> go n indiv assessed tags rest -- skip, don't break
          Nothing -> case b of
            FromBelow c
              | c == WithSupport || c == NotYet -> (n, indiv, assessed, tag : tags) -- breaks streak, include breaker
            _ -> go n indiv assessed tags rest
          _ -> case b of
            FromBoth {} -> (n, indiv, assessed, tag : tags) -- direct negative: breaks streak, include breaker
            _ -> go n indiv assessed tags rest

    -- Tagged variant of classifyRemaining
    classifyRemainingT :: [(a, AbilityBounds)] -> (MasteryStatus, [a])
    classifyRemainingT bs = case findLatestDirectFloorT bs of
      Just (SelfReliant, tag) -> (OneSuccess, [tag])
      Just (WithSupport, tag) -> (MasteryNotYet, [tag])
      Just (NotYet, tag) -> (MasteryNotYet, [tag])
      Just (SelfReliantWithSillyMistakes, tag) -> (OnlySillyMistakes, [tag])
      Nothing ->
        if any (hasDirectObs . snd) bs
          then (OnlySillyMistakes, [tag' | (tag', FromBoth {}) <- bs])
          else (NotTried, [])

    hasDirectObs :: AbilityBounds -> Bool
    hasDirectObs (FromBoth {}) = True
    hasDirectObs _ = False

    -- Tagged variant of findLatestDirectFloor
    findLatestDirectFloorT :: [(a, AbilityBounds)] -> Maybe (Ability, a)
    findLatestDirectFloorT [] = Nothing
    findLatestDirectFloorT ((tag, b) : rest) = case b of
      FromBoth {} -> case abilityFloor b of
        Just SelfReliantWithSillyMistakes -> findLatestDirectFloorT rest
        Just flr -> Just (flr, tag)
        Nothing -> findLatestDirectFloorT rest
      _ -> findLatestDirectFloorT rest

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
getConstrainedObservations doc userId clId =
  map snd $ getConstrainedObservationsTagged doc userId clId

-- | Like 'getConstrainedObservations' but pairs each bound with its source 'EvidenceId'.
getConstrainedObservationsTagged :: Document -> UserId -> CompetenceLevelId -> [(EvidenceId, AbilityBounds)]
getConstrainedObservationsTagged doc userId (compId, targetLevel) =
  let userEvidences = QEvidence.userEvidencesDesc doc userId
      grouped = QEvidence.groupByLessonDay userEvidences
   in mapMaybe (groupToBoundsTagged compId targetLevel) grouped
  where
    -- From a lesson group (sorted by reliability desc), pick the first
    -- evidence that produces bounds for this competence, tagged with its id.
    groupToBoundsTagged cId lvl evs =
      listToMaybe $ mapMaybe (evidenceToBoundsTagged cId lvl) evs

    evidenceToBoundsTagged cId lvl ev =
      fmap (ev.id,) $ evidenceToBounds cId lvl ev

-- | Compute AbilityBounds for a single evidence at a target competence/level.
-- Returns Nothing if the evidence has no observations for this competence.
-- Delegates to 'observationBounds' after extracting relevant observations.
evidenceToBounds :: CompetenceId -> Level -> Evidence -> Maybe AbilityBounds
evidenceToBounds compId targetLevel ev =
  let obs =
        [ LevelObservation lvl o.ability o.socialForm ev.activityType
        | o <- Ix.toList ev.observations
        , let (cId, lvl) = o.competenceLevelId
        , cId == compId
        ]
   in observationBounds targetLevel obs

-- | Get mastery status for one (user, competence-level)
--
-- Uses cross-level constrained observations for classification.
getUserMastery :: Document -> UserId -> CompetenceLevelId -> MasteryStatus
getUserMastery doc userId compLevelId =
  classifyMasteryConstrained $ getConstrainedObservations doc userId compLevelId

-- | Get mastery status with the evidence IDs that influenced the decision.
getUserMasteryWithReasoning :: Document -> UserId -> CompetenceLevelId -> (MasteryStatus, [EvidenceId])
getUserMasteryWithReasoning doc userId compLevelId =
  classifyWithReasoning $ getConstrainedObservationsTagged doc userId compLevelId

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
      (streakLen, hasIndiv, _hasAssessed) = countFloorStreak bounds
   in if n >= 2
        then streakLen >= n && hasIndiv
        else streakLen >= n
  where
    countFloorStreak = go 0 False False
    go !cnt !indiv !assessed [] = (cnt, indiv, assessed)
    go !cnt !indiv !assessed (b : rest) = case abilityFloor b of
      Just SelfReliant ->
        go (cnt + 1) (indiv || boundsHasIndividual b) (assessed || boundsHasAssessmentActivity b) rest
      Just SelfReliantWithSillyMistakes -> go cnt indiv assessed rest
      Nothing -> go cnt indiv assessed rest
      _ -> case b of
        FromBoth {} -> (cnt, indiv, assessed) -- direct negative: breaks streak
        _ -> go cnt indiv assessed rest -- indirect negative: skip

-- | Predicate: is user "proficient" (streak >= 2 with at least one Individual)?
isCurrentlyProficient :: Document -> UserId -> CompetenceLevelId -> Bool
isCurrentlyProficient = hasSuccessStreak 2
