module Competences.HouseCup.Scoring
  ( ScoreTable
  , StudentScore (..)
  , computePoints
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Competence (competenceLevelIdsOf)
import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationRecord (..), ParticipationType (..))
import Competences.Document.User (UserId)
import Competences.HouseCup.Config (ResolvedConfig (..))
import Competences.Query.Mastery (MasteryStatus (..), getUserMastery)
import Competences.Query.TaskStatus (TaskCompletionStatus (..), taskCompletionStatus)
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
    , mastery = masteryPoints docBefore docAfter userId
    , tasks = taskPoints docBefore docAfter userId
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

-- | Rule 2: Score mastery progression across all competence-levels.
-- Each step up in mastery tier earns +2 points. Downward changes ignored.
masteryPoints :: Document -> Document -> UserId -> Integer
masteryPoints docBefore docAfter userId =
  let allCompLevelIds = concatMap competenceLevelIdsOf $ Ix.toList docAfter.competences
   in sum $ map scoreMasteryDelta allCompLevelIds
  where
    scoreMasteryDelta clId =
      let tierBefore = masteryTier $ getUserMastery docBefore userId clId
          tierAfter = masteryTier $ getUserMastery docAfter userId clId
       in max 0 (tierAfter - tierBefore) * 2

-- | Map MasteryStatus to a numeric tier for scoring.
-- None=0, Erste Erfolge=1, Streak=2, Überprüft=3
masteryTier :: MasteryStatus -> Integer
masteryTier NotTried = 0
masteryTier MasteryNotYet = 0
masteryTier OnlySillyMistakes = 0
masteryTier OneSuccess = 1
masteryTier StreakTwoPlus = 2
masteryTier StreakTwoAssessed = 3

-- | Rule 3: Score newly completed tasks.
-- Any task going from not-done to done earns +1.
taskPoints :: Document -> Document -> UserId -> Integer
taskPoints docBefore docAfter userId =
  let allTasks = Ix.toList docAfter.tasks
   in fromIntegral $ length $ filter becameDone allTasks
  where
    becameDone task =
      let before = taskCompletionStatus docBefore userId task
          after = taskCompletionStatus docAfter userId task
       in isDone after && not (isDone before)
    isDone (TaskDone _) = True
    isDone _ = False

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
