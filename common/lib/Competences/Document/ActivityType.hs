{-# LANGUAGE CPP #-}

module Competences.Document.ActivityType
  ( ActivityType (..)
  , activityTypes
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
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

#ifdef WITH_AESON
instance FromJSON ActivityType

instance ToJSON ActivityType
#endif

instance Binary ActivityType

activityTypes :: [ActivityType]
activityTypes = [minBound .. maxBound]
