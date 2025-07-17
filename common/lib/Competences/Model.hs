module Competences.Model
  ( Model (..)
  , UpdateResult(..)
  , emptyModel
  , updateModel
  )
where

import Competences.Event (ChangableField, Event (..))
import Competences.Model.Competence (Competence (..), CompetenceIxs)
import Competences.Model.CompetenceGrid (CompetenceGrid, emptyCompetenceGrid)
import Competences.Model.Evidence (Evidence (..), EvidenceIxs)
import Competences.Model.Resource (Resource, ResourceIxs)
import Competences.Model.User (User, UserId, UserIxs)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens', (%~), (&), (^.))

data Model = Model
  { competenceGrid :: !CompetenceGrid
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , resources :: !(Ix.IxSet ResourceIxs Resource)
  , lockedFields :: !(M.Map ChangableField UserId)
  , users :: !(Ix.IxSet UserIxs User)
  }
  deriving (Eq, Generic, Show)

emptyModel :: Model
emptyModel =
  Model
    { competenceGrid = emptyCompetenceGrid
    , competences = Ix.empty
    , evidences = Ix.empty
    , resources = Ix.empty
    , lockedFields = M.empty
    , users = Ix.empty
    }

data UpdateResult
  = UpdateSuccessful
  | UpdateFailed !Text
  deriving (Eq, Show)

updateModel :: Model -> Event -> (Model, UpdateResult)
updateModel model event = case event of
  FieldLocked f u -> lockField model f u
  FieldReleased f t -> releaseField model f t
  CompetenceAdded competence -> insertNew model #competences competence (.id)
  CompetenceRemoved competenceId -> removeExisting model #competences competenceId
  EvidenceAdded evidence -> insertNew model #evidences evidence (.id)
  EvidenceRemoved evidenceId -> removeExisting model #evidences evidenceId

insertNew
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Model -> Lens' Model (Ix.IxSet ixs a) -> a -> (a -> ix) -> (Model, UpdateResult)
insertNew model lens newItem p =
  let s = model ^. lens
   in if Ix.null $ s Ix.@= p newItem
        then (model & lens %~ Ix.insert newItem, UpdateSuccessful)
        else (model, UpdateFailed "already exists")

removeExisting
  :: forall a ix ixs
   . (Ix.Indexable ixs a, Ix.IsIndexOf ix ixs)
  => Model -> Lens' Model (Ix.IxSet ixs a) -> ix -> (Model, UpdateResult)
removeExisting model lens ix =
  let s = model ^. lens
   in if Ix.null $ s Ix.@= ix
        then (model, UpdateFailed "does not exist")
        else (model & lens %~ Ix.deleteIx ix, UpdateSuccessful)

lockField :: Model -> ChangableField -> UserId -> (Model, UpdateResult)
lockField model field userId =
  let lockedFields = model ^. #lockedFields
   in if M.member field lockedFields
        then (model, UpdateFailed "already locked")
        else (model & #lockedFields %~ M.insert field userId, UpdateSuccessful)

releaseField :: Model -> ChangableField -> Maybe Text -> (Model, UpdateResult)
releaseField model field _text =
  let lockedFields = model ^. #lockedFields
   in if M.member field lockedFields
        then (model & #lockedFields %~ M.delete field, UpdateFailed "changing fields not implemented")
        else (model, UpdateFailed "not locked")
