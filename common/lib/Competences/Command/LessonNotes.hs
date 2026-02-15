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
import Competences.Document.LessonNotes (LessonNoteItem (..), LessonNotes (..))
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:?), (.!=))
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
  , items :: !(Change [LessonNoteItem])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the LessonNotes context
newtype LessonNotesCommand = OnLessonNotes (EntityCommand LessonNotes LessonNotesPatch)
  deriving (Eq, Generic, Show)

instance Binary LessonNotesPatch

#ifdef WITH_AESON
instance ToJSON LessonNotesPatch

instance FromJSON LessonNotesPatch where
  parseJSON = withObject "LessonNotesPatch" $ \v -> do
    d <- v .:? "date" .!= Nothing
    l <- v .:? "lessonId" .!= Nothing
    t <- v .:? "title" .!= Nothing
    -- Try "items" first, fall back to "resources" (wrapping as LessonResource)
    mi <- v .:? "items"
    mr <- v .:? "resources"
    let is = case mi of
          Just i -> i
          Nothing -> case mr of
            Just (Just (old, new)) -> Just (map LessonResource old, map LessonResource new)
            _ -> Nothing
    pure LessonNotesPatch
      { date = d
      , lessonId = l
      , title = t
      , items = is
      }
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
      , items = Nothing
      }

-- | Apply a patch to a LessonNotes entry
applyLessonNotesPatch :: LessonNotes -> LessonNotesPatch -> Either Text LessonNotes
applyLessonNotesPatch ln patch =
  inContext "LessonNotes" ln $
    patchField' @"date" patch
      >=> patchField' @"lessonId" patch
      >=> patchField' @"title" patch
      >=> patchField' @"items" patch

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
