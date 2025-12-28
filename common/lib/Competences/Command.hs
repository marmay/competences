module Competences.Command
  ( Command (..)
  , CommandId
  , handleCommand
  , module Competences.Command.Common
  , module Competences.Command.Competences
  , module Competences.Command.Users
  , module Competences.Command.Evidences
  )
where

import Competences.Command.Common (AffectedUsers (..), UpdateResult, EntityCommand(..), ModifyCommand(..))
import Competences.Command.Competences (CompetencesCommand (..), CompetenceGridPatch (..), CompetencePatch (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), User (..))
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Top-level command type wrapping all context commands
data Command
  = SetDocument !Document
  | Competences !CompetencesCommand
  | Users !UsersCommand
  | Evidences !EvidencesCommand
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance FromJSON Command

instance ToJSON Command

-- | Handle a command and return the updated document with affected users
handleCommand :: UserId -> Command -> Document -> UpdateResult
handleCommand userId cmd d = case cmd of
  SetDocument newDoc ->
    -- Replace entire document, all users affected
    let allUserIds = map (.id) $ Ix.toList $ newDoc ^. #users
     in Right (newDoc, AffectedUsers allUserIds)
  Competences c -> handleCompetencesCommand userId c d
  Users c -> handleUsersCommand userId c d
  Evidences c -> handleEvidencesCommand userId c d
