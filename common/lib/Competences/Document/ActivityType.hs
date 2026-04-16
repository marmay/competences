{-# LANGUAGE CPP #-}

module Competences.Document.ActivityType
  ( ActivityType (..)
  , activityTypes
  , isAssessmentActivity
  , activityReliability
  )
where

#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
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
  | Correction
    -- ^ Correction of a previous assessment.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

#ifdef WITH_AESON
instance FromJSON ActivityType

instance ToJSON ActivityType
#endif

instance Binary ActivityType

activityTypes :: [ActivityType]
activityTypes = [minBound .. maxBound]

-- | Whether the activity type represents an assessment-like situation
-- (individual demonstration under exam or conversation conditions).
isAssessmentActivity :: ActivityType -> Bool
isAssessmentActivity Exam = True
isAssessmentActivity Conversation = True
isAssessmentActivity _ = False

-- | Reliability ranking for evidence aggregation.
-- Higher value = more reliable observation.
activityReliability :: ActivityType -> Int
activityReliability Conversation = 4
activityReliability Exam = 3
activityReliability SchoolExercise = 2
activityReliability HomeExercise = 1
activityReliability Correction = 1
