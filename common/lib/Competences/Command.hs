module Competences.Command
  ( Command (..)
  , CommandId
  , handleCommand
  )
where

import Competences.Command.ChangeField (lockField, releaseField)
import Competences.Command.Common (AffectedUsers (..), UpdateResult)
import Competences.Document (Document (..))
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
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens', (%~), (&), (.~), (^.))

data Command
  = LockField !ChangableField !UserId !Text
  | ReleaseField !ChangableField !(Maybe Text)
  | AddCompetence !Competence
  | RemoveCompetence !CompetenceId
  | ReorderCompetence !(OrderPosition Competence) !(Reorder Competence)
  | AddEvidence !Evidence
  | RemoveEvidence !EvidenceId
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance FromJSON Command

instance ToJSON Command

handleCommand :: Command -> Document -> UpdateResult
handleCommand cmd model = case cmd of
  LockField f u t -> lockField model f u t
  ReleaseField f t -> releaseField model f t
  AddCompetence competence -> do
    competences' <- orderedInsert competence (model ^. #competences)
    pure $ (model & #competences .~ competences', AllUsers)
  RemoveCompetence competenceId -> do
    competences' <- orderedDelete competenceId (model ^. #competences)
    pure $ (model & #competences .~ competences', AllUsers)
  ReorderCompetence pos reordering ->
    fmap (,AllUsers) $
      fmap (\cs -> model & #competences .~ cs) $
        mapLeft explainReorderError $
          reorder pos reordering (model ^. #competences)
  AddEvidence evidence -> insertNew model #evidences evidence (.id) evidenceAffectedUsers
  RemoveEvidence evidenceId -> removeExisting model #evidences evidenceId evidenceAffectedUsers
  where
    evidenceAffectedUsers :: Evidence -> AffectedUsers
    evidenceAffectedUsers e = AllTeachersAndSpecificStudents [e.userId]

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
