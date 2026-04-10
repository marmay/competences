{-# LANGUAGE CPP #-}

module Competences.Command.CompetenceLevelExamples
  ( CompetenceLevelExamplesCommand (..)
  , CompetenceLevelExamplePatch (..)
  , handleCompetenceLevelExamplesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret
  ( interpretEntityCommand
  , mkGroupOrderedEntityCommandContext
  )
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.CompetenceLevelExample (CompetenceLevelExample (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Order (OrderPosition, Reorder, explainReorderError, reorder)
import Competences.Document.Session (SessionId)
import Competences.Document.User (UserId)
import Competences.TaskContent.RichContent (RichContent)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (^.))

-- | Patch for modifying a CompetenceLevelExample
data CompetenceLevelExamplePatch = CompetenceLevelExamplePatch
  { content :: !(Change RichContent)
  , attachments :: !(Change [FileRef])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the CompetenceLevelExamples context
data CompetenceLevelExamplesCommand
  = OnCompetenceLevelExamples !(EntityCommand CompetenceLevelExample CompetenceLevelExamplePatch)
  | ReorderCompetenceLevelExample !(OrderPosition CompetenceLevelExample) !(Reorder CompetenceLevelExample)
  deriving (Eq, Generic, Show)

instance Binary CompetenceLevelExamplePatch

#ifdef WITH_AESON
instance FromJSON CompetenceLevelExamplePatch

instance ToJSON CompetenceLevelExamplePatch
#endif

instance Binary CompetenceLevelExamplesCommand

#ifdef WITH_AESON
instance FromJSON CompetenceLevelExamplesCommand

instance ToJSON CompetenceLevelExamplesCommand
#endif

instance Default CompetenceLevelExamplePatch where
  def =
    CompetenceLevelExamplePatch
      { content = Nothing
      , attachments = Nothing
      }

-- | Apply a patch to a CompetenceLevelExample, checking for conflicts
applyCompetenceLevelExamplePatch :: CompetenceLevelExample -> CompetenceLevelExamplePatch -> Either Text CompetenceLevelExample
applyCompetenceLevelExamplePatch example patch =
  inContext "CompetenceLevelExample" example $
    patchField' @"content" patch
      >=> patchField' @"attachments" patch

-- | Handle a CompetenceLevelExamples context command
handleCompetenceLevelExamplesCommand :: UserId -> SessionId -> CompetenceLevelExamplesCommand -> Document -> UpdateResult
handleCompetenceLevelExamplesCommand userId sid cmd d = case cmd of
  OnCompetenceLevelExamples c ->
    interpretEntityCommand exampleContext userId sid c d
  ReorderCompetenceLevelExample p t -> do
    case reorder p t d.competenceLevelExamples competenceLevelIdOf of
      Left err -> Left $ explainReorderError err
      Right c' -> Right (d & #competenceLevelExamples .~ c', allUsers d)
  where
    exampleContext =
      mkGroupOrderedEntityCommandContext
        #competenceLevelExamples
        #id
        CompetenceLevelExampleLock
        competenceLevelIdOf
        applyCompetenceLevelExamplePatch
        (\_ d' -> allUsers d')

    allUsers :: Document -> AffectedUsers
    allUsers d' = AffectedUsers $ map (.id) $ Ix.toList $ d' ^. #users

-- | Extract the CompetenceLevelId (group key) from a CompetenceLevelExample
competenceLevelIdOf :: CompetenceLevelExample -> CompetenceLevelId
competenceLevelIdOf e = (e.competenceId, e.level)
