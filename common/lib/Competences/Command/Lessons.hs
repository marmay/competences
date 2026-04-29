{-# LANGUAGE CPP #-}

module Competences.Command.Lessons
  ( LessonsCommand (..)
  , LessonPatch (..)
  , handleLessonsCommand
  , deleteLessonChildren
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkGroupOrderedEntityCommandContext
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson (Lesson (..), LessonId, LessonItem, LessonPhase)
import Competences.Document.TeachingNote (TeachingNoteId)
import Competences.Document.Order (OrderPosition, Reorder, explainReorderError, reorder)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (.~), (^.))

-- | Patch for modifying a Lesson (covers all fields)
data LessonPatch = LessonPatch
  { title :: !(Change Text)
  , description :: !(Change RichContent)
  , competenceLevels :: !(Change [CompetenceLevelId])
  , date :: !(Change (Maybe Day))
  , assignments :: !(Change [AssignmentId])
  , phases :: !(Change [LessonPhase])
  , supplementalItems :: !(Change [LessonItem])
  , notesTitleOverride :: !(Change (Maybe Text))
  , privateNoteRef :: !(Change (Maybe TeachingNoteId))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Lessons context
data LessonsCommand
  = OnLessons !(EntityCommand Lesson LessonPatch)
  | ReorderLesson !(OrderPosition Lesson) !(Reorder Lesson)
  deriving (Eq, Generic, Show)

instance Binary LessonPatch
#ifdef WITH_AESON
instance FromJSON LessonPatch
instance ToJSON LessonPatch
#endif

instance Binary LessonsCommand
#ifdef WITH_AESON
instance FromJSON LessonsCommand
instance ToJSON LessonsCommand
#endif

-- Default instance
instance Default LessonPatch where
  def =
    LessonPatch
      { title = Nothing
      , description = Nothing
      , competenceLevels = Nothing
      , date = Nothing
      , assignments = Nothing
      , phases = Nothing
      , supplementalItems = Nothing
      , notesTitleOverride = Nothing
      , privateNoteRef = Nothing
      }

-- | Apply a patch to a Lesson
applyLessonPatch :: Lesson -> LessonPatch -> Either Text Lesson
applyLessonPatch lesson patch =
  inContext "Lesson" lesson $
    patchField' @"title" patch
      >=> patchField' @"description" patch
      >=> patchField' @"competenceLevels" patch
      >=> patchField' @"date" patch
      >=> patchField' @"assignments" patch
      >=> patchField' @"phases" patch
      >=> patchField' @"supplementalItems" patch
      >=> patchField' @"notesTitleOverride" patch
      >=> patchField' @"privateNoteRef" patch

-- | Delete children of a Lesson (ParticipationRecords)
deleteLessonChildren :: LessonId -> Document -> Either Text Document
deleteLessonChildren lessonId doc =
  let prs = IxSet.toList $ doc.participationRecords Ix.@= lessonId
   in Right $ doc & #participationRecords %~ \rs -> foldr IxSet.delete rs prs

-- | Handle a Lessons context command
handleLessonsCommand :: CommandContext -> LessonsCommand -> Document -> UpdateResult
handleLessonsCommand cmdCtx cmd d = case cmd of
  OnLessons c -> case c of
    Delete lessonId -> do
      -- Cascade: delete participation records for this lesson
      d' <- deleteLessonChildren lessonId d
      lesson <- lessonContext.fetch lessonId d'
      (d'', _) <- lessonContext.delete lessonId d'
      pure (d'', lessonContext.affectedUsers lesson d)
    _ -> interpretEntityCommand lessonContext cmdCtx c d
  ReorderLesson p t ->
    case reorder p t d.lessons (.mesoPlanId) of
      Left err -> Left $ explainReorderError err
      Right lessons' -> Right (d & (#lessons .~ lessons'), AudienceAll)
  where
    lessonContext =
      mkGroupOrderedEntityCommandContext
        #lessons
        #id
        LessonLock
        (^. #mesoPlanId)
        applyLessonPatch
        (\_ _ -> AudienceAll)
