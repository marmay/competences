module Competences.Command.Lessons
  ( LessonsCommand (..)
  , LessonPatch (..)
  , handleLessonsCommand
  , deleteLessonChildren
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkGroupOrderedEntityCommandContext
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson (Lesson (..), LessonId, LessonPhase)
import Competences.Document.Order (OrderPosition, Reorder, explainReorderError, reorder)
import Competences.Document.Resource (ResourceId)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (.~), (^.))

-- | Patch for modifying a Lesson (covers all fields)
data LessonPatch = LessonPatch
  { title :: !(Change Text)
  , description :: !(Change Text)
  , competenceLevels :: !(Change [CompetenceLevelId])
  , date :: !(Change (Maybe Day))
  , assignments :: !(Change [AssignmentId])
  , resources :: !(Change [ResourceId])
  , phases :: !(Change [LessonPhase])
  , notes :: !(Change Text)
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Lessons context
data LessonsCommand
  = OnLessons !(EntityCommand Lesson LessonPatch)
  | ReorderLesson !(OrderPosition Lesson) !(Reorder Lesson)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON LessonPatch
instance ToJSON LessonPatch
instance Binary LessonPatch

instance FromJSON LessonsCommand
instance ToJSON LessonsCommand
instance Binary LessonsCommand

-- Default instance
instance Default LessonPatch where
  def =
    LessonPatch
      { title = Nothing
      , description = Nothing
      , competenceLevels = Nothing
      , date = Nothing
      , assignments = Nothing
      , resources = Nothing
      , phases = Nothing
      , notes = Nothing
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
      >=> patchField' @"resources" patch
      >=> patchField' @"phases" patch
      >=> patchField' @"notes" patch

-- | Delete children of a Lesson (ParticipationRecords)
deleteLessonChildren :: LessonId -> Document -> Either Text Document
deleteLessonChildren lessonId doc =
  let prs = IxSet.toList $ doc.participationRecords Ix.@= lessonId
   in Right $ doc & #participationRecords %~ \rs -> foldr IxSet.delete rs prs

-- | Handle a Lessons context command
handleLessonsCommand :: UserId -> LessonsCommand -> Document -> UpdateResult
handleLessonsCommand userId cmd d = case cmd of
  OnLessons c -> case c of
    Delete lessonId -> do
      -- Cascade: delete participation records for this lesson
      d' <- deleteLessonChildren lessonId d
      lesson <- lessonContext.fetch lessonId d'
      (d'', _) <- lessonContext.delete lessonId d'
      pure (d'', lessonContext.affectedUsers lesson d)
    _ -> interpretEntityCommand lessonContext userId c d
  ReorderLesson p t ->
    case reorder p t d.lessons (.mesoPlanId) of
      Left err -> Left $ explainReorderError err
      Right lessons' -> Right (d & (#lessons .~ lessons'), allUsers d)
  where
    lessonContext =
      mkGroupOrderedEntityCommandContext
        #lessons
        #id
        LessonLock
        (^. #mesoPlanId)
        applyLessonPatch
        (\_ d' -> allUsers d')
    allUsers d' = AffectedUsers $ map (.id) $ IxSet.toList $ d' ^. #users
