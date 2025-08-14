module Competences.Document.Verdict
  ( AutoVerdict (..)
  , autoVerdict
  )
where

import Competences.Document.Competence (Competence (..), Level (..), levels)
import Competences.Document.Evidence
  ( Ability (..)
  , Evidence (..)
  , EvidenceIxs
  , SocialForm (..)
  , evidenceLevel
  )
import Competences.Document.User (User (..))
import Data.IxSet.Typed qualified as Ix
import Data.List (group)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | Given a list of evidences, produces a final result.
data AutoVerdict
  = -- | When we consider all evidences in reverse chronological order at the
    -- given level or below, up to before the first one where `Ability` is `NotYet`,
    -- and then filter those `Evidence`s by the respective level, the sequence has
    -- at least two entries with `Ability` being `SelfReliant` on two different days
    -- and at least one entry has the `SocialForm` `Individual`.
    Competent
  | -- | Like `Competent`, but we consider the initial sequence up to the second
    -- entry where the `Ability` is `NotYet`.
    ProbablyCompetent
  | -- | When there are at least three evidences at the given level, but none
    -- of the criteria above are satisfied, we consider the student to be
    -- `NotYetCompetent`.
    NotYetCompetent
  | -- | When none of the criteria above are satisfied, we consider the
    -- assessment to be `Inconclusive`.
    Inconclusive
  deriving (Eq, Ord, Show)

autoVerdict :: Ix.IxSet EvidenceIxs Evidence -> User -> Competence -> [(Level, AutoVerdict)]
autoVerdict s u c =
  let s' = Ix.toDescList (Proxy @Day) $ s Ix.@= c.id Ix.@= u.id
      atLevel l = l `Map.lookup` c.levelDescriptions >> pure (l, autoVerdict' s' l)
   in promoteVerdicts $ mapMaybe atLevel levels

-- | Given all evidences of a given user and competence, in reverse chronological
-- order, determines the `AutoVerdict` at a given level.
autoVerdict' :: [Evidence] -> Level -> AutoVerdict
autoVerdict' evs l
  | individuallySelfReliant competentSequence = Competent
  | individuallySelfReliant probablyCompetentSequence = ProbablyCompetent
  | length (filter ((== l) . evidenceLevel) evs) >= 3 = NotYetCompetent
  | otherwise = Inconclusive
  where
    filterWithSelect :: ([Evidence] -> [Evidence]) -> [Evidence] -> [Evidence]
    filterWithSelect select = filter ((== l) . evidenceLevel) . select . filter ((<= l) . evidenceLevel)
    competentSequence = filterWithSelect (takeWhile (\e -> e.ability /= NotYet)) evs
    probablyCompetentSequence = filterWithSelect (takeWhileButDropFirstFail (\e -> e.ability /= NotYet)) evs
    individuallySelfReliant s =
      any (\e -> e.socialForm == Individual && e.ability == SelfReliant) s
        && length (group $ map (.date) $ filter (\e -> e.ability == SelfReliant) s) >= 2

takeWhileButDropFirstFail :: (a -> Bool) -> [a] -> [a]
takeWhileButDropFirstFail p (x : xs)
  | p x = x : takeWhileButDropFirstFail p xs
  | otherwise = takeWhile p xs
takeWhileButDropFirstFail _ [] = []

-- | Promotes `AutoVerdict` at lower levels given the verdicts at higher levels.
promoteVerdicts :: [(Level, AutoVerdict)] -> [(Level, AutoVerdict)]
promoteVerdicts ((l0, v0) : (l1, v1) : rest) =
  (l0, promoteVerdict v0 v1) : promoteVerdicts ((l1, v1) : rest)
promoteVerdicts rest = rest

-- | Promotes `Verdict` at a lower level given the verdict at a higher level.
--
-- If a student is `Competent` or `ProbabyCompetent` at a higher level, but there
-- is not enough evidence, they have that level of competence at the lower level,
-- we assume they are at least as competent as they are at the higher level.
promoteVerdict :: AutoVerdict -> AutoVerdict -> AutoVerdict
promoteVerdict _ Competent = Competent
promoteVerdict Competent _ = Competent
promoteVerdict _ ProbablyCompetent = ProbablyCompetent
promoteVerdict lower _ = lower
