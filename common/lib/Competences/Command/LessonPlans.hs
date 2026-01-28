module Competences.Command.LessonPlans
  ( LessonPlansCommand (..)
  , LessonPlanPatch (..)
  , handleLessonPlansCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkEntityCommandContext
  , doLock
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.LessonPlan (LessonPlan (..), LessonPhase)
import Competences.Document.Resource (ResourceId)
import Competences.Document.User (UserId)
import Control.Monad ((>=>), unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (^.))

-- | Patch for modifying a LessonPlan
data LessonPlanPatch = LessonPlanPatch
  { date :: !(Change (Maybe Day))
  , assignments :: !(Change [AssignmentId])
  , resources :: !(Change [ResourceId])
  , phases :: !(Change [LessonPhase])
  , notes :: !(Change Text)
  }
  deriving (Eq, Generic, Show)

-- | Commands for the LessonPlans context
data LessonPlansCommand
  = OnLessonPlans !(EntityCommand LessonPlan LessonPlanPatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON LessonPlanPatch
instance ToJSON LessonPlanPatch
instance Binary LessonPlanPatch

instance FromJSON LessonPlansCommand
instance ToJSON LessonPlansCommand
instance Binary LessonPlansCommand

-- Default instance
instance Default LessonPlanPatch where
  def =
    LessonPlanPatch
      { date = Nothing
      , assignments = Nothing
      , resources = Nothing
      , phases = Nothing
      , notes = Nothing
      }

-- | Apply a patch to a LessonPlan
applyLessonPlanPatch :: LessonPlan -> LessonPlanPatch -> Either Text LessonPlan
applyLessonPlanPatch lp patch =
  inContext "LessonPlan" lp $
    patchField' @"date" patch
      >=> patchField' @"assignments" patch
      >=> patchField' @"resources" patch
      >=> patchField' @"phases" patch
      >=> patchField' @"notes" patch

-- | Handle a LessonPlans context command
handleLessonPlansCommand :: UserId -> LessonPlansCommand -> Document -> UpdateResult
handleLessonPlansCommand userId (OnLessonPlans c) d = case c of
  Create lp -> do
    -- Uniqueness: at most one LessonPlan per MesoPlanEntry
    unless (Ix.null $ d.lessonPlans Ix.@= lp.mesoPlanEntryId) $
      Left "A LessonPlan already exists for this MesoPlanEntry"
    (,allUsers d) <$> ctx.create lp d
  CreateAndLock lp -> do
    unless (Ix.null $ d.lessonPlans Ix.@= lp.mesoPlanEntryId) $
      Left "A LessonPlan already exists for this MesoPlanEntry"
    d' <- ctx.create lp d
    d'' <- doLock userId (ctx.lock (ctx.getId lp)) d'
    pure (d'', allUsers d)
  Delete lpId -> do
    -- Cascade: delete participation records for this lesson plan
    let prs = IxSet.toList $ d.participationRecords Ix.@= lpId
    let d' = d & #participationRecords %~ \rs -> foldr IxSet.delete rs prs
    (d'', lp) <- ctx.delete lpId d'
    pure (d'', ctx.affectedUsers lp d)
  Modify i modCmd ->
    interpretEntityCommand ctx userId (Modify i modCmd) d
  where
    ctx =
      mkEntityCommandContext
        #lessonPlans
        #id
        LessonPlanLock
        applyLessonPlanPatch
        (\_ d' -> allUsers d')
    allUsers d' = AffectedUsers $ map (.id) $ IxSet.toList $ d' ^. #users
