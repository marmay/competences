{-# LANGUAGE CPP #-}

-- | Structured intermediate representation for round-trip export/import
-- of teaching content across instances.
--
-- 'ExchangeDoc' is a flat package: six top-level lists, one per pool.
-- A single-entity export populates exactly one list; a lesson export
-- populates whichever lists carry the lesson's transitive references.
-- The wire format stays uniform regardless of what was originally
-- exported, so the import side never has to branch on shape.
module Competences.Exchange.Types
  ( ExchangeDoc (..)
  , emptyExchangeDoc
  , ExchangeAssignment (..)
  , ExchangeTask (..)
  , ExchangeSolution (..)
  , ExchangeAttachment (..)
  , ExchangeCompetenceRef (..)
  , ExchangeResource (..)
  , ExchangeResourceContent (..)
  , ExchangeLesson (..)
  , ExchangeLessonPhase (..)
  , ExchangeLessonItem (..)
  , ExchangeLessonItemKind (..)
  , ExchangeCompetenceGrid (..)
  , ExchangeCompetence (..)
  , ExchangeCompetenceLevelExample (..)
  , exchangeFormatVersion
  )
where

import Competences.Document.ActivityType (ActivityType)
import Competences.Document.Competence (Level)
import Competences.Document.Lesson (ActionForm, TeachingSocialForm)
import Competences.Document.Solution (SolutionType)
import Competences.Document.Task (TaskPurpose)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.!=), (.:), (.:?))
#endif

-- | Bump when the wire format changes in a way that breaks
-- backward-compatible parsing. Currently @1@.
exchangeFormatVersion :: Int
exchangeFormatVersion = 1

-- | Top-level exchange document. Every list defaults to empty so the
-- YAML can omit any pool the export doesn't touch.
data ExchangeDoc = ExchangeDoc
  { competenceGrids :: ![ExchangeCompetenceGrid]
  , tasks :: ![ExchangeTask]
  , draftTasks :: ![ExchangeTask]
  , assignments :: ![ExchangeAssignment]
  , draftAssignments :: ![ExchangeAssignment]
  , resources :: ![ExchangeResource]
  , lessons :: ![ExchangeLesson]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeDoc

#ifdef WITH_AESON
-- | All seven pools default to empty so a YAML file can omit any pool
-- it has no entries for (very common — a single-entity export only
-- populates one list).
instance FromJSON ExchangeDoc where
  parseJSON = withObject "ExchangeDoc" $ \o ->
    ExchangeDoc
      <$> o .:? "competenceGrids" .!= []
      <*> o .:? "tasks" .!= []
      <*> o .:? "draftTasks" .!= []
      <*> o .:? "assignments" .!= []
      <*> o .:? "draftAssignments" .!= []
      <*> o .:? "resources" .!= []
      <*> o .:? "lessons" .!= []
instance ToJSON ExchangeDoc
#endif

emptyExchangeDoc :: ExchangeDoc
emptyExchangeDoc =
  ExchangeDoc
    { competenceGrids = []
    , tasks = []
    , draftTasks = []
    , assignments = []
    , draftAssignments = []
    , resources = []
    , lessons = []
    }

-- | Assignment payload. The pool ('assignments' vs 'draftAssignments'
-- list at the top level) decides whether this lands in published or
-- draft storage on import — there is no per-payload @isDraft@ flag.
data ExchangeAssignment = ExchangeAssignment
  { name :: !Text
  , replaces :: !(Maybe Text)
    -- ^ When set, the import treats this as a rename of the
    -- assignment formerly named @replaces@. Matcher tries this first,
    -- then falls back to matching by 'name'.
  , description :: !Text
  , assignmentDate :: !Day
  , activityType :: !ActivityType
  , groupSubmissionAllowed :: !Bool
  , taskRefs :: ![Text]
    -- ^ Identifiers of tasks belonging to this assignment. The
    -- corresponding bodies live in the top-level @tasks@ /
    -- @draftTasks@ list.
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeAssignment

#ifdef WITH_AESON
instance FromJSON ExchangeAssignment where
  parseJSON = withObject "ExchangeAssignment" $ \o ->
    ExchangeAssignment
      <$> o .: "name"
      <*> o .:? "replaces"
      <*> o .:? "description" .!= ""
      <*> o .: "assignmentDate"
      <*> o .: "activityType"
      <*> o .:? "groupSubmissionAllowed" .!= False
      <*> o .:? "taskRefs" .!= []
instance ToJSON ExchangeAssignment
#endif

-- | Task payload. Solutions and attachments stay inline because they
-- only ever live with a single owning task.
data ExchangeTask = ExchangeTask
  { identifier :: !Text
  , replaces :: !(Maybe Text)
    -- ^ Optional rename: matcher tries this old identifier first.
  , title :: !Text
  , content :: !(Maybe Text)
  , purpose :: !TaskPurpose
  , primary :: ![ExchangeCompetenceRef]
  , secondary :: ![ExchangeCompetenceRef]
  , solutions :: ![ExchangeSolution]
  , attachments :: ![ExchangeAttachment]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeTask

#ifdef WITH_AESON
instance FromJSON ExchangeTask where
  parseJSON = withObject "ExchangeTask" $ \o ->
    ExchangeTask
      <$> o .: "identifier"
      <*> o .:? "replaces"
      <*> o .:? "title" .!= ""
      <*> o .:? "content"
      <*> o .: "purpose"
      <*> o .:? "primary" .!= []
      <*> o .:? "secondary" .!= []
      <*> o .:? "solutions" .!= []
      <*> o .:? "attachments" .!= []
instance ToJSON ExchangeTask
#endif

data ExchangeSolution = ExchangeSolution
  { solutionType :: !SolutionType
  , content :: !Text
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeSolution

#ifdef WITH_AESON
instance FromJSON ExchangeSolution
instance ToJSON ExchangeSolution
#endif

-- | Attachment metadata. Metadata-only by default — same-server
-- imports resolve the blob via the shared CAS by 'sha256'. A later
-- revision adds an optional embedded content field for cross-server
-- sharing.
data ExchangeAttachment = ExchangeAttachment
  { fileName :: !Text
  , mimeType :: !Text
  , sha256 :: !Text
  , bytes :: !Int64
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeAttachment

#ifdef WITH_AESON
instance FromJSON ExchangeAttachment
instance ToJSON ExchangeAttachment
#endif

-- | Competence reference by grid title + competence description +
-- level. The importing instance matches these against its own
-- competence grids; unmatched references are surfaced in the preview.
data ExchangeCompetenceRef = ExchangeCompetenceRef
  { grid :: !Text
  , description :: !Text
  , level :: !Level
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeCompetenceRef

#ifdef WITH_AESON
instance FromJSON ExchangeCompetenceRef
instance ToJSON ExchangeCompetenceRef
#endif

data ExchangeResource = ExchangeResource
  { identifier :: !Text
  , replaces :: !(Maybe Text)
    -- ^ Optional rename: matcher tries this old identifier first.
  , content :: !ExchangeResourceContent
  , competenceLevels :: ![ExchangeCompetenceRef]
  , attachments :: ![ExchangeAttachment]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeResource

#ifdef WITH_AESON
instance FromJSON ExchangeResource where
  parseJSON = withObject "ExchangeResource" $ \o ->
    ExchangeResource
      <$> o .: "identifier"
      <*> o .:? "replaces"
      <*> o .: "content"
      <*> o .:? "competenceLevels" .!= []
      <*> o .:? "attachments" .!= []
instance ToJSON ExchangeResource
#endif

-- | Mirrors 'Competences.Document.Resource.ResourceContent' on the
-- exchange wire — the four resource flavours teachers can produce.
-- @ExFileContent@ carries attachment metadata only; cross-server
-- imports surface the file as missing until embedded-content support
-- lands.
data ExchangeResourceContent
  = ExInlineContent !Text
  | ExWebLink !Text !Text
  | ExVideoLink !Text !Text
  | ExFileContent !ExchangeAttachment
  deriving (Eq, Generic, Show)

instance Binary ExchangeResourceContent

#ifdef WITH_AESON
instance FromJSON ExchangeResourceContent
instance ToJSON ExchangeResourceContent
#endif

-- | Lesson payload. References siblings by name (assignments) or
-- identifier (tasks/resources); their bodies live in the top-level
-- pools when the export needed to ship them along.
data ExchangeLesson = ExchangeLesson
  { title :: !Text
  , replaces :: !(Maybe Text)
    -- ^ Optional rename: matcher tries this old title first.
  , description :: !Text
  , date :: !(Maybe Day)
  , competences :: ![ExchangeCompetenceRef]
  , phases :: ![ExchangeLessonPhase]
  , supplementalItems :: ![ExchangeLessonItem]
  , notesTitleOverride :: !(Maybe Text)
  , assignmentRefs :: ![Text]
    -- ^ Top-level assignment names linked from this lesson, in order.
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeLesson

#ifdef WITH_AESON
instance FromJSON ExchangeLesson where
  parseJSON = withObject "ExchangeLesson" $ \o ->
    ExchangeLesson
      <$> o .: "title"
      <*> o .:? "replaces"
      <*> o .:? "description" .!= ""
      <*> o .:? "date"
      <*> o .:? "competences" .!= []
      <*> o .:? "phases" .!= []
      <*> o .:? "supplementalItems" .!= []
      <*> o .:? "notesTitleOverride"
      <*> o .:? "assignmentRefs" .!= []
instance ToJSON ExchangeLesson
#endif

-- | A single phase within a lesson.
data ExchangeLessonPhase = ExchangeLessonPhase
  { title :: !Text
  , socialForm :: !TeachingSocialForm
  , duration :: !Int
  , actionForm :: !ActionForm
  , items :: ![ExchangeLessonItem]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeLessonPhase

#ifdef WITH_AESON
instance FromJSON ExchangeLessonPhase where
  parseJSON = withObject "ExchangeLessonPhase" $ \o ->
    ExchangeLessonPhase
      <$> o .:? "title" .!= ""
      <*> o .: "socialForm"
      <*> o .: "duration"
      <*> o .: "actionForm"
      <*> o .:? "items" .!= []
instance ToJSON ExchangeLessonPhase
#endif

-- | A reference inside a phase or the supplemental list. The @ref@
-- string keys the matching top-level list: assignment @name@, task
-- @identifier@, or resource @identifier@.
data ExchangeLessonItem = ExchangeLessonItem
  { kind :: !ExchangeLessonItemKind
  , ref :: !Text
  , publish :: !Bool
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeLessonItem

#ifdef WITH_AESON
instance FromJSON ExchangeLessonItem
instance ToJSON ExchangeLessonItem
#endif

data ExchangeLessonItemKind
  = ItemAssignment
  | ItemTask
  | ItemResource
  deriving (Eq, Generic, Show)

instance Binary ExchangeLessonItemKind

#ifdef WITH_AESON
instance FromJSON ExchangeLessonItemKind
instance ToJSON ExchangeLessonItemKind
#endif

-- | Competence grid payload. Carries its competences inline (a
-- competence has no meaning outside its grid).
data ExchangeCompetenceGrid = ExchangeCompetenceGrid
  { title :: !Text
  , replaces :: !(Maybe Text)
    -- ^ Optional rename: matcher tries this old title first.
  , description :: !Text
  , competences :: ![ExchangeCompetence]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeCompetenceGrid

#ifdef WITH_AESON
instance FromJSON ExchangeCompetenceGrid where
  parseJSON = withObject "ExchangeCompetenceGrid" $ \o ->
    ExchangeCompetenceGrid
      <$> o .: "title"
      <*> o .:? "replaces"
      <*> o .:? "description" .!= ""
      <*> o .:? "competences" .!= []
instance ToJSON ExchangeCompetenceGrid
#endif

-- | Competence within a grid. Matched by 'description' (or 'replaces'
-- when supplied) within the parent grid.
data ExchangeCompetence = ExchangeCompetence
  { description :: !Text
  , replaces :: !(Maybe Text)
    -- ^ Optional rename: matcher tries this old description first.
  , levels :: !(Map Level Text)
    -- ^ Per-level human-readable description. Empty descriptions and
    -- absent levels mean the same thing — no description at this
    -- level.
  , examples :: !(Map Level [ExchangeCompetenceLevelExample])
    -- ^ Per-level concrete examples. On import each listed level is
    -- replaced wholesale (replace-all): a level absent from this map
    -- is left untouched. Order is the list position.
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeCompetence

#ifdef WITH_AESON
instance FromJSON ExchangeCompetence where
  parseJSON = withObject "ExchangeCompetence" $ \o ->
    ExchangeCompetence
      <$> o .: "description"
      <*> o .:? "replaces"
      <*> o .:? "levels" .!= mempty
      <*> o .:? "examples" .!= mempty
instance ToJSON ExchangeCompetence
#endif

-- | A single concrete example for a competence level. Lives nested
-- under 'ExchangeCompetence.examples' keyed by 'Level'; its position
-- in the list is its order.
data ExchangeCompetenceLevelExample = ExchangeCompetenceLevelExample
  { content :: !Text
  , attachments :: ![ExchangeAttachment]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeCompetenceLevelExample

#ifdef WITH_AESON
instance FromJSON ExchangeCompetenceLevelExample where
  parseJSON = withObject "ExchangeCompetenceLevelExample" $ \o ->
    ExchangeCompetenceLevelExample
      <$> o .: "content"
      <*> o .:? "attachments" .!= []
instance ToJSON ExchangeCompetenceLevelExample
#endif
