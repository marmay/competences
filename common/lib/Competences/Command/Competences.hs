module Competences.Command.Competences
  ( CompetencesCommand (..)
  , CompetenceGridPatch (..)
  , CompetencePatch (..)
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
import Competences.Document.Competence (Competence (..), Level)
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

-- | Patch for modifying a Competence (only editable fields)
data CompetencePatch = CompetencePatch
  { description :: !(Change Text)
    -- ^ Change description from old to new value
  , levelDescriptions :: !(Map Level (Change Text))
    -- ^ Map of level-specific description changes
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

instance FromJSON CompetencePatch
instance ToJSON CompetencePatch
instance Binary CompetencePatch

instance FromJSON CompetencesCommand
instance ToJSON CompetencesCommand
instance Binary CompetencesCommand

-- Default instances
instance Default CompetenceGridPatch where
  def = CompetenceGridPatch {title = Nothing, description = Nothing}

instance Default CompetencePatch where
  def = CompetencePatch {description = Nothing, levelDescriptions = Map.empty}

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
        >=> applyLevelDescriptionsChanges patch.levelDescriptions
  where
    applyLevelDescriptionsChanges :: Map Level (Change Text) -> Competence -> Either Text Competence
    applyLevelDescriptionsChanges changes c = do
      newLevelDescs <- foldM applyLevelChange c.levelDescriptions (Map.toList changes)
      pure $ c & #levelDescriptions .~ newLevelDescs

    applyLevelChange :: Map Level Text -> (Level, Change Text) -> Either Text (Map Level Text)
    applyLevelChange currentMap (level, change) =
      case change of
        Nothing -> Right currentMap
        Just (before, after) -> do
          let current = Map.findWithDefault T.empty level currentMap
          when (current /= before) $
            Left $ "levelDescriptions[" <> T.pack (show level) <> "]: conflict detected"
          Right $ Map.insert level after currentMap

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
