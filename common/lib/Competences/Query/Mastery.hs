-- | Mastery classification queries
-- Pure functions for determining student mastery status at competence levels
-- Designed for reuse across analytics views and conditional task assignment
module Competences.Query.Mastery
  ( -- * Mastery Classification
    MasteryStatus (..)
  , classifyMastery

    -- * Streak Calculation
  , SuccessStreak (..)
  , calculateStreak

    -- * Document Queries
  , getUserMastery
  , getClassMasteryStats
  , getClassMasteryWithStudents

    -- * Low-level helpers (for conditional task predicates)
  , getObservationsForLevel
  , hasSuccessStreak
  , isCurrentlyProficient
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.User (User (..), UserId, isStudent)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Time (Day)
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

-- | Detailed streak information for complex condition expressions
data SuccessStreak = SuccessStreak
  { streakLength :: !Int
  -- ^ Current consecutive SelfReliant count (SillyMistakes ignored, not broken)
  , totalSuccesses :: !Int
  -- ^ All-time SelfReliant count
  , latestAbility :: !(Maybe Ability)
  -- ^ Most recent observation (including SillyMistakes)
  }
  deriving (Eq, Show, Generic)

-- | Classify mastery from a list of abilities (sorted newest-first)
--
-- Algorithm:
-- 1. If empty → NotTried
-- 2. Find latest "real" attempt (first non-SillyMistakes):
--    - If none found but has SillyMistakes → OnlySillyMistakes
--    - If latest real is WithSupport/Ability.NotYet → MasteryNotYet
-- 3. Latest real is SelfReliant:
--    - Count consecutive SelfReliant (skip SillyMistakes gaps)
--    - If count >= 2 → StreakTwoPlus
--    - If count == 1 → OneSuccess
classifyMastery :: [Ability] -> MasteryStatus
classifyMastery abilities = case abilities of
  [] -> NotTried
  _ ->
    let streak = calculateStreak abilities
     in case findLatestReal abilities of
          Nothing ->
            -- Only SillyMistakes exist
            OnlySillyMistakes
          Just WithSupport -> MasteryNotYet
          Just NotYet -> MasteryNotYet
          Just SelfReliant ->
            if streak.streakLength >= 2
              then StreakTwoPlus
              else OneSuccess
          Just SelfReliantWithSillyMistakes ->
            -- This shouldn't happen since we skip SillyMistakes in findLatestReal
            -- But if it does, treat as OnlySillyMistakes
            OnlySillyMistakes

-- | Find the latest "real" attempt (skipping SillyMistakes)
findLatestReal :: [Ability] -> Maybe Ability
findLatestReal = go
  where
    go [] = Nothing
    go (SelfReliantWithSillyMistakes : rest) = go rest
    go (a : _) = Just a

-- | Calculate detailed streak information
--
-- Counts consecutive SelfReliant observations from the start,
-- where SelfReliantWithSillyMistakes are skipped (don't break the streak,
-- but don't count toward it either).
calculateStreak :: [Ability] -> SuccessStreak
calculateStreak abilities =
  SuccessStreak
    { streakLength = countConsecutiveSuccesses abilities
    , totalSuccesses = length $ filter (== SelfReliant) abilities
    , latestAbility = case abilities of
        [] -> Nothing
        (a : _) -> Just a
    }

-- | Count consecutive SelfReliant from the start, skipping SillyMistakes
countConsecutiveSuccesses :: [Ability] -> Int
countConsecutiveSuccesses = go 0
  where
    go !n [] = n
    go !n (SelfReliant : rest) = go (n + 1) rest
    go !n (SelfReliantWithSillyMistakes : rest) = go n rest -- Skip, don't break
    go !n _ = n -- WithSupport or NotYet breaks the streak

-- ============================================================================
-- Document Queries
-- ============================================================================

-- | Get observations for (user, competence-level) sorted by date descending
--
-- Extracts the Ability from each observation, ordered newest-first
getObservationsForLevel :: Document -> UserId -> CompetenceLevelId -> [Ability]
getObservationsForLevel doc userId compLevelId =
  let -- Get evidences for this user, sorted by date descending (newest first)
      userEvidences = Ix.toDescList (Proxy @Day) $ doc.evidences Ix.@= userId
      -- Extract observations matching the competence level
      extractAbilities ev =
        [ obs.ability
        | obs <- Ix.toList ev.observations
        , obs.competenceLevelId == compLevelId
        ]
   in concatMap extractAbilities userEvidences

-- | Get mastery status for one (user, competence-level)
getUserMastery :: Document -> UserId -> CompetenceLevelId -> MasteryStatus
getUserMastery doc userId compLevelId =
  classifyMastery $ getObservationsForLevel doc userId compLevelId

-- | Get class-wide mastery statistics for one competence-level
--
-- Returns a map from MasteryStatus to count of students in that category
getClassMasteryStats :: Document -> CompetenceLevelId -> Map MasteryStatus Int
getClassMasteryStats doc compLevelId =
  let students = filter isStudent $ Ix.toList doc.users
      statuses = map (\u -> getUserMastery doc u.id compLevelId) students
   in foldl' countStatus Map.empty statuses
  where
    countStatus acc status = Map.insertWith (+) status 1 acc

-- | Get class-wide mastery with student lists for one competence-level
--
-- Returns a map from MasteryStatus to list of students in that category,
-- sorted alphabetically by student name.
getClassMasteryWithStudents :: Document -> CompetenceLevelId -> Map MasteryStatus [User]
getClassMasteryWithStudents doc compLevelId =
  let students = filter isStudent $ Ix.toList doc.users
      pairs = map (\u -> (getUserMastery doc u.id compLevelId, u)) students
   in Map.map (sortOn (.name)) $ foldl' groupStudent Map.empty pairs
  where
    groupStudent acc (status, user) = Map.insertWith (++) status [user] acc

-- ============================================================================
-- Predicates for Conditional Task Assignment
-- ============================================================================

-- | Predicate: does user have a success streak >= n for this level?
hasSuccessStreak :: Int -> Document -> UserId -> CompetenceLevelId -> Bool
hasSuccessStreak n doc userId compLevelId =
  let abilities = getObservationsForLevel doc userId compLevelId
      streak = calculateStreak abilities
   in streak.streakLength >= n

-- | Predicate: is user "proficient" (streak >= 2)?
isCurrentlyProficient :: Document -> UserId -> CompetenceLevelId -> Bool
isCurrentlyProficient = hasSuccessStreak 2
