{-# LANGUAGE CPP #-}

module Competences.Command.LessonNotes
  ( LessonNotesCommand (..)
  , LessonNotesPatch (..)
  , handleLessonNotesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Lesson (LessonId)
import Competences.Document.LessonNotes (LessonNotes (..))
import Competences.Document.Resource (ResourceId)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Patch for modifying a LessonNotes entry
data LessonNotesPatch = LessonNotesPatch
  { date :: !(Change Day)
  , lessonId :: !(Change (Maybe LessonId))
  , title :: !(Change Text)
  , resources :: !(Change [ResourceId])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the LessonNotes context
newtype LessonNotesCommand = OnLessonNotes (EntityCommand LessonNotes LessonNotesPatch)
  deriving (Eq, Generic, Show)

instance Binary LessonNotesPatch

#ifdef WITH_AESON
instance FromJSON LessonNotesPatch

instance ToJSON LessonNotesPatch
#endif

instance Binary LessonNotesCommand

#ifdef WITH_AESON
instance FromJSON LessonNotesCommand

instance ToJSON LessonNotesCommand
#endif

-- Default instance
instance Default LessonNotesPatch where
  def =
    LessonNotesPatch
      { date = Nothing
      , lessonId = Nothing
      , title = Nothing
      , resources = Nothing
      }

-- | Apply a patch to a LessonNotes entry
applyLessonNotesPatch :: LessonNotes -> LessonNotesPatch -> Either Text LessonNotes
applyLessonNotesPatch ln patch =
  inContext "LessonNotes" ln $
    patchField' @"date" patch
      >=> patchField' @"lessonId" patch
      >=> patchField' @"title" patch
      >=> patchField' @"resources" patch

-- | Handle a LessonNotes context command
handleLessonNotesCommand :: UserId -> LessonNotesCommand -> Document -> UpdateResult
handleLessonNotesCommand userId (OnLessonNotes c) d =
  interpretEntityCommand lessonNotesContext userId c d
  where
    lessonNotesContext =
      mkEntityCommandContext
        #lessonNotes
        #id
        LessonNotesLock
        applyLessonNotesPatch
        affectedUsersForLessonNotes

    -- All users can see lesson notes
    affectedUsersForLessonNotes :: LessonNotes -> Document -> AffectedUsers
    affectedUsersForLessonNotes _ d' =
      AffectedUsers $ map (.id) $ IxSet.toList $ d' ^. #users

