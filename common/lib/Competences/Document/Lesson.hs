{-# LANGUAGE CPP #-}

module Competences.Document.Lesson
  ( -- * Lesson (unified entity)
    LessonId
  , Lesson (..)
  , LessonIxs
    -- * LessonPhase (plain data)
  , LessonPhase (..)
    -- * LessonItem
  , LessonItem (..)
  , LessonItemContent (..)
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
import Competences.Document.Task (TaskId)
import Competences.Document.TeachingNote (TeachingNoteId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?), (.!=))
#endif
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

#ifdef WITH_AESON
instance FromJSON TeachingSocialForm

instance ToJSON TeachingSocialForm
#endif

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

#ifdef WITH_AESON
instance FromJSON ActionForm

instance ToJSON ActionForm
#endif

instance Binary ActionForm

-- ============================================================================
-- LessonItem (the student-facing content of a LessonPhase, plus
-- Lesson.supplementalItems)
-- ============================================================================

-- | What a 'LessonItem' points at: a resource, task, or assignment.
-- Variants are named with the @Phase@ prefix to avoid collision with
-- the legacy 'LessonNoteItem' constructors at use sites.
data LessonItemContent
  = PhaseResource !ResourceId
  | PhaseTask !TaskId
  | PhaseAssignment !AssignmentId
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON LessonItemContent

instance ToJSON LessonItemContent
#endif

instance Binary LessonItemContent

-- | A single entry in a phase's (or lesson's supplemental) item list.
-- @publish@ controls whether students see it; teacher-side views
-- always show the item.
data LessonItem = LessonItem
  { content :: !LessonItemContent
  , publish :: !Bool
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON LessonItem

instance ToJSON LessonItem
#endif

instance Binary LessonItem

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
  , items :: ![LessonItem]
  -- ^ Mixed list of resources, tasks and assignments attached to this
  -- phase. Per-item @publish@ flag controls student visibility.
  , privateNoteRef :: !(Maybe TeachingNoteId)
  -- ^ Optional reference to the teacher-only annotation for this phase.
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON LessonPhase where
  parseJSON = withObject "LessonPhase" $ \v ->
    LessonPhase
      <$> v .: "title"
      <*> v .: "socialForm"
      <*> v .: "duration"
      <*> v .: "actionForm"
      <*> v .:? "items" .!= []
      <*> v .:? "privateNoteRef" .!= Nothing

instance ToJSON LessonPhase
#endif

instance Binary LessonPhase

-- ============================================================================
-- Lesson (unified entity: merged MesoPlanEntry + LessonPlan)
-- ============================================================================

type LessonId = Id Lesson

-- | Unified lesson entity. Combines meso-level planning (title, description,
-- competence levels) with lesson-level detail (date, resources, phases, notes).
-- Assignments are linked via 'assignments' field.
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
  -- ^ Assignments linked to this lesson
  , phases :: ![LessonPhase]
  , supplementalItems :: ![LessonItem]
  -- ^ Items (resources / tasks / assignments) not tied to a phase.
  -- Rendered at the bottom of the student view as a supplemental block.
  , notesTitleOverride :: !(Maybe Text)
  -- ^ Override for the auto-derived student title (German UI:
  -- "Schulübung vom …"). @Nothing@ means use the auto default.
  , privateNoteRef :: !(Maybe TeachingNoteId)
  -- ^ Optional reference to the teacher-only annotation for the lesson.
  }
  deriving (Eq, Generic, Ord, Show)

type LessonIxs = '[LessonId, MesoPlanId, Order, Day, AssignmentId]

instance Ix.Indexable LessonIxs Lesson where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.mesoPlanId))
      (Ix.ixFun $ singleton . (.order))
      (Ix.ixFun $ \l -> maybeToList l.date)
      (Ix.ixFun (.assignments))

#ifdef WITH_AESON
instance FromJSON Lesson where
  parseJSON = withObject "Lesson" $ \v ->
    Lesson
      <$> v .: "id"
      <*> v .: "mesoPlanId"
      <*> v .: "order"
      <*> v .: "title"
      <*> v .:? "description" .!= mempty
      <*> v .: "competenceLevels"
      <*> v .: "date"
      <*> v .:? "assignments" .!= []
      <*> v .:? "phases" .!= []
      <*> v .:? "supplementalItems" .!= []
      <*> v .:? "notesTitleOverride" .!= Nothing
      <*> v .:? "privateNoteRef" .!= Nothing

instance ToJSON Lesson
#endif

instance Binary Lesson

instance Orderable Lesson
