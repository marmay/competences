{-# LANGUAGE CPP #-}

module Competences.Command.ParticipationRecords
  ( ParticipationRecordsCommand (..)
  , ParticipationRecordPatch (..)
  , handleParticipationRecordsCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkEntityCommandContext
  , doLock
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..))
import Competences.Document.ParticipationRecord (ParticipationRecord (..))
import Control.Monad (unless)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Patch for modifying a ParticipationRecord
data ParticipationRecordPatch = ParticipationRecordPatch
  { remark :: !(Change (Maybe Text))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the ParticipationRecords context
data ParticipationRecordsCommand
  = OnParticipationRecords !(EntityCommand ParticipationRecord ParticipationRecordPatch)
  deriving (Eq, Generic, Show)

instance Binary ParticipationRecordPatch
#ifdef WITH_AESON
instance FromJSON ParticipationRecordPatch
instance ToJSON ParticipationRecordPatch
#endif

instance Binary ParticipationRecordsCommand
#ifdef WITH_AESON
instance FromJSON ParticipationRecordsCommand
instance ToJSON ParticipationRecordsCommand
#endif

-- Default instance
instance Default ParticipationRecordPatch where
  def = ParticipationRecordPatch {remark = Nothing}

-- | Apply a patch to a ParticipationRecord
applyParticipationRecordPatch :: ParticipationRecord -> ParticipationRecordPatch -> Either Text ParticipationRecord
applyParticipationRecordPatch pr patch =
  inContext "ParticipationRecord" pr $
    patchField' @"remark" patch

-- | Handle a ParticipationRecords context command
handleParticipationRecordsCommand :: CommandContext -> ParticipationRecordsCommand -> Document -> UpdateResult
handleParticipationRecordsCommand cmdCtx (OnParticipationRecords c) d = case c of
  Create pr -> do
    -- Uniqueness: at most one per (lessonId, userId, participationType)
    let existing = d.participationRecords Ix.@= pr.lessonId Ix.@= pr.userId Ix.@= pr.participationType
    unless (Ix.null existing) $
      Left "A ParticipationRecord already exists for this Lesson, User, and ParticipationType"
    d' <- ctx.create pr d
    pure (d', ctx.affectedUsers pr d')
  CreateAndLock pr -> do
    let existing = d.participationRecords Ix.@= pr.lessonId Ix.@= pr.userId Ix.@= pr.participationType
    unless (Ix.null existing) $
      Left "A ParticipationRecord already exists for this Lesson, User, and ParticipationType"
    d' <- ctx.create pr d
    d'' <- doLock cmdCtx (ctx.lock (ctx.getId pr)) d'
    pure (d'', ctx.affectedUsers pr d'')
  _ -> interpretEntityCommand ctx cmdCtx c d
  where
    ctx =
      mkEntityCommandContext
        #participationRecords
        #id
        ParticipationRecordLock
        applyParticipationRecordPatch
        (\pr _ -> AudienceTeachersAnd [pr.userId])
