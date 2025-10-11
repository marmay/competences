module Competences.Command
  ( Command (..)
  , CommandId
  , handleCommand
  )
where

import Competences.Command.ChangeField (FieldEncoding, changeField, lockField, releaseField)
import Competences.Command.Common (AffectedUsers (..), UpdateResult)
import Competences.Document (Document (..), User (..))
import Competences.Document.ChangableField (ChangableField)
import Competences.Document.Competence (Competence (..), CompetenceId)
import Competences.Document.Evidence (Evidence (..), EvidenceId)
import Competences.Document.Id (Id)
import Competences.Document.Order
  ( OrderPosition
  , Reorder
  , explainReorderError
  , orderedDelete
  , orderedInsert
  , reorder
  )
import Competences.Document.User (UserId)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Extra (mapLeft)
import Data.IxSet.Typed qualified as Ix
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Optics.Core (Lens', (%~), (&), (.~), (^.))

data ReleaseCommand a
  = OnlyRelease
  | ReleaseWithChange !a
  | Delete
  deriving (Eq, Generic, Show)

data Command
  = LockField !ChangableField !UserId !FieldEncoding
  | ReleaseField !ChangableField !UserId !(Maybe FieldEncoding)
  | ChangeField !ChangableField !UserId !FieldEncoding !FieldEncoding
  | AddCompetence !Competence
  | RemoveCompetence !CompetenceId
  | ReorderCompetence !(OrderPosition Competence) !(Reorder Competence)
  | AddEvidence !Evidence
  | LockEvidence !EvidenceId
  | ReleaseEvidence !(ReleaseCommand Evidence)
  | AddUser !User
  | RemoveUser !UserId
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance FromJSON Command

instance ToJSON Command

handleCommand :: Command -> Document -> UpdateResult
handleCommand cmd model = case cmd of
  LockField f u t -> lockField model f u t
  ReleaseField f u t -> releaseField model f u t
  ChangeField f u t t' -> changeField model f u t t'
  AddCompetence competence -> do
    competences' <- orderedInsert competence (model ^. #competences)
    pure (model & #competences .~ competences', AllUsers)
  RemoveCompetence competenceId -> do
    competences' <- orderedDelete competenceId (model ^. #competences)
    pure (model & #competences .~ competences', AllUsers)
  ReorderCompetence pos reordering ->
    fmap (,AllUsers) $
      fmap (\cs -> model & #competences .~ cs) $
        mapLeft explainReorderError $
          reorder pos reordering (model ^. #competences)
  AddEvidence evidence -> insertNew model #evidences evidence (.id) evidenceAffectedUsers
  RemoveEvidence evidenceId -> removeExisting model #evidences evidenceId evidenceAffectedUsers
  AddUser user -> do
    insertNew model #users user (.id) userAffectedUsers
  RemoveUser userId -> do
    removeExisting model #users userId userAffectedUsers
  where
    evidenceAffectedUsers e = AllTeachersAndSpecificStudents (Set.toList e.userIds)
    userAffectedUsers u = AllTeachersAndSpecificStudents [u.id]

insertNew
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Document
  -> Lens' Document (Ix.IxSet ixs a)
  -> a
  -> (a -> ix)
  -> (a -> AffectedUsers)
  -> UpdateResult
insertNew model lens newItem p affectedUsers = do
  unless (Ix.null $ (model ^. lens) Ix.@= p newItem) $
    Left "entity with that index already exists."
  pure (model & lens %~ Ix.insert newItem, affectedUsers newItem)

removeExisting
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Document -> Lens' Document (Ix.IxSet ixs a) -> ix -> (a -> AffectedUsers) -> UpdateResult
removeExisting model lens ix affectedUsers = do
  case Ix.getOne $ (model ^. lens) Ix.@= ix of
    Just a -> pure (model & lens %~ Ix.deleteIx ix, affectedUsers a)
    Nothing -> Left "entity with that index does not exist."
