{-# LANGUAGE CPP #-}

module Competences.Command.MesoPlans
  ( MesoPlansCommand (..)
  , MesoPlanPatch (..)
  , handleMesoPlansCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkEntityCommandContext
  )
import Competences.Command.Lessons (deleteLessonChildren)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lesson (..), Lock (..))
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanId)
import Control.Monad (foldM, (>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~))

-- | Patch for modifying a MesoPlan
data MesoPlanPatch = MesoPlanPatch
  { title :: !(Change Text)
  , dateFrom :: !(Change (Maybe Day))
  , dateTo :: !(Change (Maybe Day))
  }
  deriving (Eq, Generic, Show)

-- | Commands for the MesoPlans context
data MesoPlansCommand
  = OnMesoPlans !(EntityCommand MesoPlan MesoPlanPatch)
  deriving (Eq, Generic, Show)

instance Binary MesoPlanPatch
#ifdef WITH_AESON
instance FromJSON MesoPlanPatch
instance ToJSON MesoPlanPatch
#endif

instance Binary MesoPlansCommand
#ifdef WITH_AESON
instance FromJSON MesoPlansCommand
instance ToJSON MesoPlansCommand
#endif

-- Default instances
instance Default MesoPlanPatch where
  def = MesoPlanPatch {title = Nothing, dateFrom = Nothing, dateTo = Nothing}

-- | Apply a patch to a MesoPlan
applyMesoPlanPatch :: MesoPlan -> MesoPlanPatch -> Either Text MesoPlan
applyMesoPlanPatch plan patch =
  inContext "MesoPlan" plan $
    patchField' @"title" patch
      >=> patchField' @"dateFrom" patch
      >=> patchField' @"dateTo" patch

-- | Delete a MesoPlan and all its children (cascading delete)
-- Deletes: Lessons -> their ParticipationRecords
deleteMesoPlanCascading :: MesoPlanId -> Document -> Either Text (Document, MesoPlan)
deleteMesoPlanCascading planId doc = do
  plan <- case Ix.getOne (doc.mesoPlans Ix.@= planId) of
    Nothing -> Left "MesoPlan not found"
    Just p -> Right p
  let lessons = IxSet.toList $ doc.lessons Ix.@= planId
  -- For each lesson, delete its ParticipationRecords
  doc' <- foldM (\d l -> deleteLessonChildren l.id d) doc lessons
  -- Delete all lessons
  let doc'' = doc' & #lessons %~ \ls -> foldr IxSet.delete ls lessons
  -- Delete the plan itself
  let doc''' = doc'' & #mesoPlans %~ IxSet.delete plan
  pure (doc''', plan)

-- | Handle a MesoPlans context command
handleMesoPlansCommand :: CommandContext -> MesoPlansCommand -> Document -> UpdateResult
handleMesoPlansCommand cmdCtx cmd d = case cmd of
  OnMesoPlans c -> case c of
    Delete planId -> do
      (d', plan) <- deleteMesoPlanCascading planId d
      pure (d', mesoPlanContext.affectedUsers plan d)
    _ -> interpretEntityCommand mesoPlanContext cmdCtx c d
  where
    mesoPlanContext =
      mkEntityCommandContext
        #mesoPlans
        #id
        MesoPlanLock
        applyMesoPlanPatch
        (\_ _ -> AudienceTeachers)
