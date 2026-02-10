module Competences.HouseCup.Scoring
  ( ScoreTable
  , StudentScore (..)
  , computePoints
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationRecord (..), ParticipationType (..))
import Competences.Document.User (UserId)
import Competences.HouseCup.Config (ResolvedConfig (..))
import Data.Text (Text)

type ScoreTable = [(Text, Integer)]

-- | Per-student score breakdown.
data StudentScore = StudentScore
  { participation :: !Integer
  , mastery :: !Integer
  , tasks :: !Integer
  }
  deriving (Eq, Show)

totalScore :: StudentScore -> Integer
totalScore s = s.participation + s.mastery + s.tasks

-- | Compute points for one student across all three rules.
studentPoints :: Document -> Document -> UserId -> StudentScore
studentPoints docBefore docAfter userId =
  StudentScore
    { participation = participationPoints docBefore docAfter userId
    , mastery = 0 -- Phase 3
    , tasks = 0 -- Phase 4
    }

-- | Rule 1: Score new participation records.
-- Records present in docAfter but absent in docBefore earn points.
participationPoints :: Document -> Document -> UserId -> Integer
participationPoints docBefore docAfter userId =
  let afterRecords = Ix.toList $ docAfter.participationRecords Ix.@= userId
      newRecords = filter (\r -> Ix.null $ docBefore.participationRecords Ix.@= r.id) afterRecords
   in sum $ map scoreRecord newRecords

scoreRecord :: ParticipationRecord -> Integer
scoreRecord r = case (r.participationType, r.level) of
  (Participation, ParticipationLevel1) -> 2
  (Participation, ParticipationLevel2) -> 5
  (Collaboration, ParticipationLevel1) -> 3
  (Collaboration, ParticipationLevel2) -> 7
  (PoorWorkEthic, ParticipationLevel1) -> -5
  (PoorWorkEthic, ParticipationLevel2) -> -15

-- | Compute house points by diffing two document states.
computePoints :: ResolvedConfig -> Document -> Document -> ScoreTable
computePoints (ResolvedConfig houses) docBefore docAfter =
  let maxSize = maximum $ map (length . snd) houses
   in map (aggregateHouse maxSize) houses
  where
    aggregateHouse :: Int -> (Text, [UserId]) -> (Text, Integer)
    aggregateHouse maxSize (name, userIds) =
      let rawTotal = sum [totalScore (studentPoints docBefore docAfter uid) | uid <- userIds]
          houseSize = length userIds
          scaled = rawTotal * fromIntegral maxSize `div` fromIntegral houseSize
       in (name, scaled)
