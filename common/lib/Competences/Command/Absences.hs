{-# LANGUAGE CPP #-}

module Competences.Command.Absences
  ( AbsencesCommand (..)
  , AbsencePatch (..)
  , handleAbsencesCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (CommandContext (..), EntityCommand (..), UpdateResult)
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkEntityCommandContext
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..))
import Competences.Document.Absence (Absence (..))
import Control.Monad (unless)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Optics.Core ((&), (%~))

-- | Patch for modifying an Absence (no editable fields)
data AbsencePatch = AbsencePatch
  deriving (Eq, Generic, Show)

-- | Commands for the Absences context
data AbsencesCommand
  = OnAbsences !(EntityCommand Absence AbsencePatch)
  deriving (Eq, Generic, Show)

instance Binary AbsencePatch
#ifdef WITH_AESON
instance FromJSON AbsencePatch
instance ToJSON AbsencePatch
#endif

instance Binary AbsencesCommand
#ifdef WITH_AESON
instance FromJSON AbsencesCommand
instance ToJSON AbsencesCommand
#endif

instance Default AbsencePatch where
  def = AbsencePatch

-- | Apply a patch to an Absence (no-op, no editable fields)
applyAbsencePatch :: Absence -> AbsencePatch -> Either text Absence
applyAbsencePatch a AbsencePatch = Right a

-- | Handle an Absences context command
handleAbsencesCommand :: CommandContext -> AbsencesCommand -> Document -> UpdateResult
handleAbsencesCommand cmdCtx (OnAbsences c) d = case c of
  Create a -> do
    -- Uniqueness: at most one per (lessonId, userId)
    let existing = d.absences Ix.@= a.lessonId Ix.@= a.userId
    unless (Ix.null existing) $
      Left "An Absence already exists for this Lesson and User"
    d' <- ctx.create a d
    -- Clear participation records for this (lesson, user)
    let prsToDelete = d'.participationRecords Ix.@= a.lessonId Ix.@= a.userId
        d'' = d' & #participationRecords %~ \prs -> foldr Ix.delete prs (Ix.toList prsToDelete)
    pure (d'', ctx.affectedUsers a d'')
  _ -> interpretEntityCommand ctx cmdCtx c d
  where
    ctx =
      mkEntityCommandContext
        #absences
        #id
        AbsenceLock
        applyAbsencePatch
        (\a _ -> AudienceTeachersAnd [a.userId])
