module Competences.Document.LessonPlan
  ( -- * LessonPlan
    LessonPlanId
  , LessonPlan (..)
  , LessonPlanIxs
    -- * LessonPhase (plain data)
  , LessonPhase (..)
    -- * Enums
  , TeachingSocialForm (..)
  , ActionForm (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.MesoPlan (MesoPlanEntryId)
import Competences.Document.Resource (ResourceId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Maybe (maybeToList)
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

-- | A phase within a lesson plan. Plain structured data — no ID, ordering
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
-- LessonPlan
-- ============================================================================

type LessonPlanId = Id LessonPlan

-- | Detailed plan for a single lesson. Linked to a MesoPlanEntry
-- (at most one LessonPlan per entry). Single lock covers all sub-data.
data LessonPlan = LessonPlan
  { id :: !LessonPlanId
  , mesoPlanEntryId :: !MesoPlanEntryId
  , date :: !(Maybe Day)
  , assignments :: ![AssignmentId]
  -- ^ Linked existing assignments
  , resources :: ![ResourceId]
  -- ^ Linked resources (materials, exercises, etc.)
  , phases :: ![LessonPhase]
  -- ^ Ordered sequence of lesson phases
  , notes :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

type LessonPlanIxs = '[LessonPlanId, MesoPlanEntryId, Day]

instance Ix.Indexable LessonPlanIxs LessonPlan where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.mesoPlanEntryId))
      (Ix.ixFun $ \lp -> maybeToList lp.date)

instance FromJSON LessonPlan

instance ToJSON LessonPlan

instance Binary LessonPlan
