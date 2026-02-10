module Competences.HouseCup.Scoring
  ( ScoreTable
  , StudentScore (..)
  , computePoints
  )
where

import Competences.Document (Document (..))
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
studentPoints _docBefore _docAfter _userId =
  StudentScore
    { participation = 0 -- Phase 2
    , mastery = 0 -- Phase 3
    , tasks = 0 -- Phase 4
    }

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
