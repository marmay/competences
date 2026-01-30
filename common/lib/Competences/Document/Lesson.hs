module Competences.Document.Lesson
  ( -- * Lesson (unified entity)
    LessonId
  , Lesson (..)
  , LessonIxs
    -- * LessonPhase (plain data)
  , LessonPhase (..)
    -- * Enums
  , TeachingSocialForm (..)
  , ActionForm (..)
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id)
import Competences.Document.MesoPlan (MesoPlanId)
import Competences.Document.Order (Order, Orderable)
import Competences.Document.Resource (ResourceId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Maybe (maybeToList)
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- ============================================================================
-- Enums
-- ============================================================================

-- | Social form for lesson phases (how students are organized).
-- Different from Evidence SocialForm (Group/Individual) which describes
-- how competence demonstration is categorized.
data TeachingSocialForm
  = WholeClass
  | SmallGroups
  | PairWork
  | IndividualWork
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON TeachingSocialForm

instance ToJSON TeachingSocialForm

instance Binary TeachingSocialForm

-- | How the teacher acts during a lesson phase.
data ActionForm
  = -- | "darbietend" - teacher presents
    Presenting
  | -- | "zusammenwirkend" - teacher collaborates with students
    Collaborating
  | -- | "aufgebend" - teacher assigns work, supports as necessary
    Assigning
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON ActionForm

instance ToJSON ActionForm

instance Binary ActionForm

-- ============================================================================
-- LessonPhase (plain data, not an entity)
-- ============================================================================

-- | A phase within a lesson. Plain structured data — no ID, ordering
-- is implicit in list position. Patched atomically as a whole list.
data LessonPhase = LessonPhase
  { title :: !Text
  , socialForm :: !TeachingSocialForm
  , duration :: !Int
  -- ^ Duration in minutes
  , actionForm :: !ActionForm
  , notes :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON LessonPhase

instance ToJSON LessonPhase

instance Binary LessonPhase

-- ============================================================================
-- Lesson (unified entity: merged MesoPlanEntry + LessonPlan)
-- ============================================================================

type LessonId = Id Lesson

-- | Unified lesson entity. Combines meso-level planning (title, description,
-- competence levels) with lesson-level detail (date, assignments, resources,
-- phases, notes). Two view states (collapsed/expanded) but a single editor.
data Lesson = Lesson
  { id :: !LessonId
  , mesoPlanId :: !MesoPlanId
  , order :: !Order
  -- Meso-level fields (collapsed view):
  , title :: !Text
  , description :: !RichContent
  -- ^ Rich text
  , competenceLevels :: ![CompetenceLevelId]
  -- Lesson-level fields (expanded view):
  , date :: !(Maybe Day)
  , assignments :: ![AssignmentId]
  , resources :: ![ResourceId]
  , phases :: ![LessonPhase]
  , notes :: !RichContent
  -- ^ Rich text
  }
  deriving (Eq, Generic, Ord, Show)

type LessonIxs = '[LessonId, MesoPlanId, Order, Day]

instance Ix.Indexable LessonIxs Lesson where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.mesoPlanId))
      (Ix.ixFun $ singleton . (.order))
      (Ix.ixFun $ \l -> maybeToList l.date)

instance FromJSON Lesson

instance ToJSON Lesson

instance Binary Lesson

instance Orderable Lesson
