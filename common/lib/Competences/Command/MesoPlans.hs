module Competences.Command.MesoPlans
  ( MesoPlansCommand (..)
  , MesoPlanPatch (..)
  , MesoPlanEntryPatch (..)
  , handleMesoPlansCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( EntityCommandContext (..)
  , interpretEntityCommand
  , mkEntityCommandContext
  , mkGroupOrderedEntityCommandContext
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.LessonPlan (LessonPlan (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanEntry (..), MesoPlanEntryId, MesoPlanId)
import Competences.Document.Order (OrderPosition, Reorder, explainReorderError, reorder)
import Competences.Document.User (UserId)
import Control.Monad (foldM, (>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~), (.~), (^.))

-- | Patch for modifying a MesoPlan
data MesoPlanPatch = MesoPlanPatch
  { title :: !(Change Text)
  , competenceGridId :: !(Change CompetenceGridId)
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying a MesoPlanEntry
data MesoPlanEntryPatch = MesoPlanEntryPatch
  { title :: !(Change Text)
  , description :: !(Change Text)
  , competenceLevels :: !(Change [CompetenceLevelId])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the MesoPlans context
data MesoPlansCommand
  = OnMesoPlans !(EntityCommand MesoPlan MesoPlanPatch)
  | OnMesoPlanEntries !(EntityCommand MesoPlanEntry MesoPlanEntryPatch)
  | ReorderMesoPlanEntry !(OrderPosition MesoPlanEntry) !(Reorder MesoPlanEntry)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON MesoPlanPatch
instance ToJSON MesoPlanPatch
instance Binary MesoPlanPatch

instance FromJSON MesoPlanEntryPatch
instance ToJSON MesoPlanEntryPatch
instance Binary MesoPlanEntryPatch

instance FromJSON MesoPlansCommand
instance ToJSON MesoPlansCommand
instance Binary MesoPlansCommand

-- Default instances
instance Default MesoPlanPatch where
  def = MesoPlanPatch {title = Nothing, competenceGridId = Nothing}

instance Default MesoPlanEntryPatch where
  def = MesoPlanEntryPatch {title = Nothing, description = Nothing, competenceLevels = Nothing}

-- | Apply a patch to a MesoPlan
applyMesoPlanPatch :: MesoPlan -> MesoPlanPatch -> Either Text MesoPlan
applyMesoPlanPatch plan patch =
  inContext "MesoPlan" plan $
    patchField' @"title" patch
      >=> patchField' @"competenceGridId" patch

-- | Apply a patch to a MesoPlanEntry
applyMesoPlanEntryPatch :: MesoPlanEntry -> MesoPlanEntryPatch -> Either Text MesoPlanEntry
applyMesoPlanEntryPatch entry patch =
  inContext "MesoPlanEntry" entry $
    patchField' @"title" patch
      >=> patchField' @"description" patch
      >=> patchField' @"competenceLevels" patch

-- | Delete a MesoPlan and all its children (cascading delete)
-- Deletes: MesoPlanEntries -> their LessonPlans -> their ParticipationRecords
deleteMesoPlanCascading :: MesoPlanId -> Document -> Either Text (Document, MesoPlan)
deleteMesoPlanCascading planId doc = do
  plan <- case Ix.getOne (doc.mesoPlans Ix.@= planId) of
    Nothing -> Left "MesoPlan not found"
    Just p -> Right p
  let entries = IxSet.toList $ doc.mesoPlanEntries Ix.@= planId
  -- For each entry, delete its LessonPlan + ParticipationRecords
  doc' <- foldM (\d e -> deleteMesoPlanEntryChildren e.id d) doc entries
  -- Delete all entries
  let doc'' = doc' & #mesoPlanEntries %~ \es -> foldr IxSet.delete es entries
  -- Delete the plan itself
  let doc''' = doc'' & #mesoPlans %~ IxSet.delete plan
  pure (doc''', plan)

-- | Delete children of a MesoPlanEntry (LessonPlan + ParticipationRecords)
deleteMesoPlanEntryChildren :: MesoPlanEntryId -> Document -> Either Text Document
deleteMesoPlanEntryChildren entryId doc =
  case Ix.getOne (doc.lessonPlans Ix.@= entryId) of
    Nothing -> Right doc -- No lesson plan for this entry
    Just lp ->
      -- Delete participation records for this lesson plan
      let prs = IxSet.toList $ doc.participationRecords Ix.@= lp.id
          doc' = doc & #participationRecords %~ \rs -> foldr IxSet.delete rs prs
       in -- Delete the lesson plan
          Right $ doc' & #lessonPlans %~ IxSet.delete lp

-- | Handle a MesoPlans context command
handleMesoPlansCommand :: UserId -> MesoPlansCommand -> Document -> UpdateResult
handleMesoPlansCommand userId cmd d = case cmd of
  OnMesoPlans c -> case c of
    Delete planId -> do
      (d', plan) <- deleteMesoPlanCascading planId d
      pure (d', mesoPlanContext.affectedUsers plan d)
    _ -> interpretEntityCommand mesoPlanContext userId c d
  OnMesoPlanEntries c -> case c of
    Delete entryId -> do
      -- Delete children first, then delete the entry
      d' <- deleteMesoPlanEntryChildren entryId d
      entry <- mesoPlanEntryContext.fetch entryId d'
      (d'', _) <- mesoPlanEntryContext.delete entryId d'
      pure (d'', mesoPlanEntryContext.affectedUsers entry d)
    _ -> interpretEntityCommand mesoPlanEntryContext userId c d
  ReorderMesoPlanEntry p t ->
    case reorder p t d.mesoPlanEntries (.mesoPlanId) of
      Left err -> Left $ explainReorderError err
      Right entries' -> Right (d & (#mesoPlanEntries .~ entries'), allUsers d)
  where
    mesoPlanContext =
      mkEntityCommandContext
        #mesoPlans
        #id
        MesoPlanLock
        applyMesoPlanPatch
        (\_ d' -> allUsers d')
    mesoPlanEntryContext =
      mkGroupOrderedEntityCommandContext
        #mesoPlanEntries
        #id
        MesoPlanEntryLock
        (^. #mesoPlanId)
        applyMesoPlanEntryPatch
        (\_ d' -> allUsers d')
    allUsers d' = AffectedUsers $ map (.id) $ IxSet.toList $ d' ^. #users
