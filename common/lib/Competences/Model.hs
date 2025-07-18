module Competences.Model
  ( Model (..)
  , emptyModel
  , fieldATraversal
  )
where

import Competences.Model.ChangableField (ChangableField (..))
import Competences.Model.Competence (Competence (..), CompetenceIxs)
import Competences.Model.CompetenceGrid (CompetenceGrid (..), emptyCompetenceGrid)
import Competences.Model.Evidence (Evidence (..), EvidenceIxs)
import Competences.Model.Resource (Resource, ResourceIxs)
import Competences.Model.User (User, UserId, UserIxs)
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.AffineTraversal (AffineTraversal', atraversal)
import Optics.Core (castOptic, ix, (%%))

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

fieldATraversal :: ChangableField -> AffineTraversal' Model Text
fieldATraversal CompetenceGridTitle = castOptic #competenceGrid %% castOptic #title
fieldATraversal CompetenceGridDescription = castOptic #competenceGrid %% castOptic #description
fieldATraversal (CompetenceDescription competenceId) = castOptic #competences %% ixATraversal competenceId %% castOptic #description
fieldATraversal (CompetenceLevelDescription (competenceId, level)) = castOptic #competences %% ixATraversal competenceId %% castOptic #levelDescriptions %% ix level

ixATraversal
  :: forall ix a (ixs :: [Type])
   . (Ix.IsIndexOf ix ixs, Ix.Indexable ixs a) => ix -> AffineTraversal' (Ix.IxSet ixs a) a
ixATraversal ix' =
  let get :: Ix.IxSet ixs a -> Either (Ix.IxSet ixs a) a
      get ixSet = maybe (Left ixSet) Right $ Ix.getOne $ ixSet Ix.@= ix'
      set :: Ix.IxSet ixs a -> a -> Ix.IxSet ixs a
      set ixSet a = case Ix.getOne (ixSet Ix.@= ix') of
        Just _ -> Ix.insert a ixSet
        Nothing -> ixSet
   in atraversal get set
