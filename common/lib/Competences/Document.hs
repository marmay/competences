{-# LANGUAGE TypeFamilies #-}

module Competences.Document
  ( Document (..)
  , emptyDocument
  , projectDocument
  , module Competences.Document.Lock
  , module Competences.Document.Competence
  , module Competences.Document.CompetenceGrid
  , module Competences.Document.Evidence
  , module Competences.Document.Order
  , module Competences.Document.Resource
  , module Competences.Document.Task
  , module Competences.Document.Assignment
  , module Competences.Document.User
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Competence
  ( Competence (..)
  , CompetenceId
  , CompetenceIxs
  , Level (..)
  , levels
  )
import Competences.Document.CompetenceGrid
  ( CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceGridIxs
  , emptyCompetenceGrid
  )
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentIxs)
import Competences.Document.Evidence (Evidence (..), EvidenceId, EvidenceIxs)
import Competences.Document.Lock (Lock (..))
import Competences.Document.Order (Order, orderAt, orderMax, orderMin, ordered)
import Competences.Document.Resource (Resource (..), ResourceId, ResourceIxs)
import Competences.Document.Task (Task (..), TaskId, TaskIxs, TaskGroup (..), TaskGroupId, TaskGroupIxs, TaskType (..))
import Competences.Document.User (User (..), UserId, UserIxs, UserRole (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Map qualified as M
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (^.))

data Document = Document
  { competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , resources :: !(Ix.IxSet ResourceIxs Resource)
  , locks :: !(M.Map Lock UserId)
  , users :: !(Ix.IxSet UserIxs User)
  , tasks :: !(Ix.IxSet TaskIxs Task)
  , taskGroups :: !(Ix.IxSet TaskGroupIxs TaskGroup)
  , assignments :: !(Ix.IxSet AssignmentIxs Assignment)
  }
  deriving (Eq, Generic, Show)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v ->
    Document
      <$> v .: "competenceGrids"
      <*> fmap Ix.fromList (v .: "competences")
      <*> fmap Ix.fromList (v .: "evidences")
      <*> fmap Ix.fromList (v .: "resources")
      <*> fmap M.fromList (v .: "locks")
      <*> fmap Ix.fromList (v .: "users")
      <*> fmap Ix.fromList (v .: "tasks")
      <*> fmap Ix.fromList (v .: "taskGroups")
      <*> fmap Ix.fromList (v .: "assignments")

instance ToJSON Document where
  toJSON d =
    object
      [ "competenceGrids" .= d.competenceGrids
      , "competences" .= Ix.toList d.competences
      , "evidences" .= Ix.toList d.evidences
      , "resources" .= Ix.toList d.resources
      , "locks" .= M.toList d.locks
      , "users" .= Ix.toList d.users
      , "tasks" .= Ix.toList d.tasks
      , "taskGroups" .= Ix.toList d.taskGroups
      , "assignments" .= Ix.toList d.assignments
      ]

emptyDocument :: Document
emptyDocument =
  Document
    { competenceGrids = Ix.empty
    , competences = Ix.empty
    , evidences = Ix.empty
    , resources = Ix.empty
    , locks = M.empty
    , users = Ix.empty
    , tasks = Ix.empty
    , taskGroups = Ix.empty
    , assignments = Ix.empty
    }


-- | Project document based on user identity for access control
-- Teachers see full document, students see filtered view
projectDocument :: User -> Document -> Document
projectDocument user doc
  | user.role == Teacher = doc  -- Teachers see everything
  | otherwise =
      -- Students see filtered view based on their identity
      doc
        & #users .~ Ix.fromList [user]  -- Only their own user
        & #evidences .~ (doc.evidences Ix.@= user.id)  -- Only evidences about them (via UserId index)
        & #assignments .~ (doc.assignments Ix.@= user.id)  -- Only assignments assigned to them (via UserId index)
        & #locks .~ M.filterWithKey isLockVisible (doc ^. #locks)  -- Only locks on entities they can see
        -- competenceGrids, competences, resources, tasks, taskGroups: students see all (public materials)
        -- TODO: May need to filter tasks/taskGroups in the future (e.g., hide exam questions before exam date)
  where
    -- Student can see locks on entities they have access to
    isLockVisible lock _ = case lock of
      UserLock uid -> uid == user.id  -- Only their own user
      EvidenceLock eid ->
        case Ix.getOne (Ix.getEQ eid (doc ^. #evidences)) of
          Just e -> user.id `elem` e.userIds  -- Only evidences about them
          Nothing -> False
      AssignmentLock aid ->
        case Ix.getOne (Ix.getEQ aid (doc ^. #assignments)) of
          Just a -> user.id `elem` a.studentIds  -- Only assignments assigned to them
          Nothing -> False
      _ -> True  -- Other locks (competence, grid, etc.) are visible (public materials)