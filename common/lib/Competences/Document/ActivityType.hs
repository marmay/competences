module Competences.Document.ActivityType
  ( ActivityType (..)
  , activityTypes
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)

-- | Different kinds of activities during which a student can demonstrate
-- they are competent.
data ActivityType
  = Conversation
    -- ^ A conversation with a teacher.
  | Exam
    -- ^ A written or oral exam.
  | SchoolExercise
    -- ^ Exercising in school.
  | HomeExercise
    -- ^ Home exercise.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON ActivityType

instance ToJSON ActivityType

instance Binary ActivityType

activityTypes :: [ActivityType]
activityTypes = [minBound .. maxBound]
