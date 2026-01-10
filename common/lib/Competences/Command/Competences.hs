module Competences.Command.Competences
  ( CompetencesCommand (..)
  , CompetenceGridPatch (..)
  , CompetencePatch (..)
  , LevelInfoPatch (..)
  , handleCompetencesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( interpretEntityCommand
  , mkGroupOrderedEntityCommandContext
  , mkOrderedEntityCommandContext
  )
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Competence (Competence (..), Level, LevelInfo (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Order (OrderPosition, Reorder, explainReorderError, reorder)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as Ix
import Control.Monad (foldM, when, (>=>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (^.))

-- | Patch for modifying a CompetenceGrid (only editable fields)
data CompetenceGridPatch = CompetenceGridPatch
  { title :: !(Change Text)
    -- ^ Change title from old to new value
  , description :: !(Change Text)
    -- ^ Change description from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying individual level fields
data LevelInfoPatch = LevelInfoPatch
  { description :: !(Change Text)
    -- ^ Change description from old to new value
  , locked :: !(Change Bool)
    -- ^ Change locked status from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying a Competence (only editable fields)
data CompetencePatch = CompetencePatch
  { description :: !(Change Text)
    -- ^ Change description from old to new value
  , levels :: !(Map Level LevelInfoPatch)
    -- ^ Map of level-specific patches
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Competences context (CompetenceGrids and Competences)
data CompetencesCommand
  = OnCompetenceGrids !(EntityCommand CompetenceGrid CompetenceGridPatch)
  | OnCompetences !(EntityCommand Competence CompetencePatch)
  | ReorderCompetence !(OrderPosition Competence) !(Reorder Competence)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON CompetenceGridPatch
instance ToJSON CompetenceGridPatch
instance Binary CompetenceGridPatch

instance FromJSON LevelInfoPatch
instance ToJSON LevelInfoPatch
instance Binary LevelInfoPatch

instance FromJSON CompetencePatch
instance ToJSON CompetencePatch
instance Binary CompetencePatch

instance FromJSON CompetencesCommand
instance ToJSON CompetencesCommand
instance Binary CompetencesCommand

-- Default instances
instance Default CompetenceGridPatch where
  def = CompetenceGridPatch {title = Nothing, description = Nothing}

instance Default LevelInfoPatch where
  def = LevelInfoPatch {description = Nothing, locked = Nothing}

instance Default CompetencePatch where
  def = CompetencePatch {description = Nothing, levels = Map.empty}

-- | Apply a patch to a CompetenceGrid, checking for conflicts
applyCompetenceGridPatch :: CompetenceGrid -> CompetenceGridPatch -> Either Text CompetenceGrid
applyCompetenceGridPatch grid patch =
  inContext "CompetenceGrid" grid $
    patchField' @"title" patch
      >=> patchField' @"description" patch

-- | Apply a patch to a Competence, checking for conflicts
applyCompetencePatch :: Competence -> CompetencePatch -> Either Text Competence
applyCompetencePatch competence patch =
  inContext "Competence" competence $
      patchField' @"description" patch
        >=> applyLevelChanges patch.levels
        >=> pure . cleanupLevels
  where
    -- Apply level-specific patches
    applyLevelChanges :: Map Level LevelInfoPatch -> Competence -> Either Text Competence
    applyLevelChanges changes c = do
      newLevels <- foldM applyLevelPatch c.levels (Map.toList changes)
      pure $ c & #levels .~ newLevels

    applyLevelPatch :: Map Level LevelInfo -> (Level, LevelInfoPatch) -> Either Text (Map Level LevelInfo)
    applyLevelPatch currentMap (level, levelPatch) = do
      let current = Map.findWithDefault (LevelInfo T.empty False) level currentMap
      -- Apply description change
      newDesc <- case levelPatch.description of
        Nothing -> Right current.description
        Just (before, after) -> do
          when (current.description /= before) $
            Left $ "levels[" <> T.pack (show level) <> "].description: conflict detected"
          Right after
      -- Apply locked change
      newLocked <- case levelPatch.locked of
        Nothing -> Right current.locked
        Just (before, after) -> do
          when (current.locked /= before) $
            Left $ "levels[" <> T.pack (show level) <> "].locked: conflict detected"
          Right after
      Right $ Map.insert level (LevelInfo newDesc newLocked) currentMap

    -- Remove entries where description is empty and not locked (invariant enforcement)
    cleanupLevels :: Competence -> Competence
    cleanupLevels c =
      c & #levels .~ Map.filter (\info -> not (T.null info.description) || info.locked) c.levels

-- | Handle a Competences context command
handleCompetencesCommand :: UserId -> CompetencesCommand -> Document -> UpdateResult
handleCompetencesCommand userId cmd d = case cmd of
  OnCompetenceGrids c ->
    interpretEntityCommand competenceGridContext userId c d
  OnCompetences c ->
    interpretEntityCommand competenceContext userId c d
  ReorderCompetence p t -> do
    case reorder p t d.competences (.competenceGridId) of
      Left err -> Left $ explainReorderError err
      Right c' -> Right (d & (#competences .~ c'), allUsers d)
  where
    competenceGridContext =
      mkOrderedEntityCommandContext
        #competenceGrids
        #id
        CompetenceGridLock
        applyCompetenceGridPatch
        (\_ d' -> allUsers d')
    competenceContext =
      mkGroupOrderedEntityCommandContext
        #competences
        #id
        CompetenceLock
        (^. #competenceGridId)
        applyCompetencePatch
        (\_ d' -> allUsers d')
    allUsers d' = AffectedUsers $ map (.id) $ Ix.toList $ d' ^. #users
