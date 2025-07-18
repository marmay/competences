module Competences.Command
  ( Command (..)
  , CommandId
  , handleCommand
  )
where

import Competences.Command.ChangeField (lockField, releaseField)
import Competences.Model (Model(..))
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.Competence (Competence(..), CompetenceId)
import Competences.Model.Evidence (Evidence(..), EvidenceId)
import Competences.Model.Id (Id)
import Competences.Model.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.IxSet.Typed as Ix
import Optics.Core (Lens', (^.), (&), (%~))
import Competences.Command.Common (UpdateResult, AffectedUsers (..))
import Control.Monad (unless)

data Command
  = LockField !ChangableField !UserId !Text
  | ReleaseField !ChangableField !(Maybe Text)
  | AddCompetence !Competence
  | RemoveCompetence !CompetenceId
  | AddEvidence !Evidence
  | RemoveEvidence !EvidenceId
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance FromJSON Command

instance ToJSON Command

handleCommand :: Model -> Command -> UpdateResult
handleCommand model event = case event of
  LockField f u t -> lockField model f u t
  ReleaseField f t -> releaseField model f t
  AddCompetence competence -> insertNew model #competences competence (.id) (const AllUsers)
  RemoveCompetence competenceId -> removeExisting model #competences competenceId (const AllUsers)
  AddEvidence evidence -> insertNew model #evidences evidence (.id) evidenceAffectedUsers
  RemoveEvidence evidenceId -> removeExisting model #evidences evidenceId evidenceAffectedUsers

  where
    evidenceAffectedUsers :: Evidence -> AffectedUsers
    evidenceAffectedUsers e = AllTeachersAndSpecificStudents [e.userId]

insertNew
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Model -> Lens' Model (Ix.IxSet ixs a) -> a -> (a -> ix) -> (a -> AffectedUsers) -> UpdateResult
insertNew model lens newItem p affectedUsers = do
  unless (Ix.null $ (model ^. lens) Ix.@= p newItem) $
    Left "entity with that index already exists."
  pure (model & lens %~ Ix.insert newItem, affectedUsers newItem)

removeExisting
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Model -> Lens' Model (Ix.IxSet ixs a) -> ix -> (a -> AffectedUsers) -> UpdateResult
removeExisting model lens ix affectedUsers = do
  case Ix.getOne $ (model ^. lens) Ix.@= ix of
    Just a -> pure (model & lens %~ Ix.deleteIx ix, affectedUsers a)
    Nothing -> Left "entity with that index does not exist."
