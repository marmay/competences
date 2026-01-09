module Competences.Document.Grade
  ( Grade (..)
  , grades
  , exactGrades
  , gradeToText
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Austrian school grades 1-5 with optional in-between grades.
-- In-between grades are used for competence grid grades.
-- Final grades use only exact grades (Grade1-5).
data Grade
  = -- | Sehr gut (Excellent)
    Grade1
  | -- | Zwischen Sehr gut und Gut
    Grade1_2
  | -- | Gut (Good)
    Grade2
  | -- | Zwischen Gut und Befriedigend
    Grade2_3
  | -- | Befriedigend (Satisfactory)
    Grade3
  | -- | Zwischen Befriedigend und Genügend
    Grade3_4
  | -- | Genügend (Sufficient)
    Grade4
  | -- | Zwischen Genügend und Nicht genügend
    Grade4_5
  | -- | Nicht genügend (Insufficient/Fail)
    Grade5
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | All grades in order from best to worst.
grades :: [Grade]
grades = [Grade1, Grade1_2, Grade2, Grade2_3, Grade3, Grade3_4, Grade4, Grade4_5, Grade5]

-- | Only exact grades (1-5), no in-between grades.
-- Used for final grades.
exactGrades :: [Grade]
exactGrades = [Grade1, Grade2, Grade3, Grade4, Grade5]

-- | Human-readable text representation of a grade.
gradeToText :: Grade -> Text
gradeToText Grade1 = "1 (Sehr gut)"
gradeToText Grade1_2 = "1-2"
gradeToText Grade2 = "2 (Gut)"
gradeToText Grade2_3 = "2-3"
gradeToText Grade3 = "3 (Befriedigend)"
gradeToText Grade3_4 = "3-4"
gradeToText Grade4 = "4 (Genügend)"
gradeToText Grade4_5 = "4-5"
gradeToText Grade5 = "5 (Nicht genügend)"

instance FromJSON Grade

instance ToJSON Grade

instance Binary Grade
