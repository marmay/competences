{-# LANGUAGE CPP #-}

module Competences.Document.Absence
  ( AbsenceId
  , Absence (..)
  , AbsenceIxs
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import GHC.Generics (Generic)

type AbsenceId = Id Absence

-- | Per-student per-lesson absence record.
-- At most one per (lessonId, userId).
data Absence = Absence
  { id :: !AbsenceId
  , lessonId :: !LessonId
  , userId :: !UserId
  }
  deriving (Eq, Generic, Ord, Show)

type AbsenceIxs = '[AbsenceId, LessonId, UserId]

instance Ix.Indexable AbsenceIxs Absence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.lessonId))
      (Ix.ixFun $ singleton . (.userId))

#ifdef WITH_AESON
instance FromJSON Absence

instance ToJSON Absence
#endif

instance Binary Absence
