module Competences.Document
  ( Document (..)
  , PartialChecksumId (..)
  , emptyDocument
  , fieldATraversal
  , updateChecksums
  , updateAllChecksums
  )
where

import Competences.Document.ChangableField (ChangableField (..))
import Competences.Document.Competence (Competence (..), CompetenceIxs)
import Competences.Document.CompetenceGrid (CompetenceGrid (..), emptyCompetenceGrid)
import Competences.Document.Evidence (Evidence (..), EvidenceIxs)
import Competences.Document.Resource (Resource, ResourceIxs)
import Competences.Document.User (User, UserId, UserIxs)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL (ByteString)
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.AffineTraversal (AffineTraversal', atraversal)
import Optics.Core (Lens', castOptic, ix, (%%), (%~), (&), (.~), (^.))

data Document = Document
  { competenceGrid :: !CompetenceGrid
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , resources :: !(Ix.IxSet ResourceIxs Resource)
  , lockedFields :: !(M.Map ChangableField UserId)
  , users :: !(Ix.IxSet UserIxs User)
  , partialChecksums :: !(M.Map PartialChecksumId ByteString)
  , overallChecksum :: !ByteString
  }
  deriving (Eq, Generic, Show)

emptyDocument :: Document
emptyDocument =
  updateAllChecksums $
    Document
      { competenceGrid = emptyCompetenceGrid
      , competences = Ix.empty
      , evidences = Ix.empty
      , resources = Ix.empty
      , lockedFields = M.empty
      , users = Ix.empty
      , partialChecksums = M.empty
      , overallChecksum = ""
      }

fieldATraversal :: ChangableField -> AffineTraversal' Document Text
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

data PartialChecksumId
  = PC_CompetenceGrid
  | PC_Competences
  | PC_Evidences
  | PC_LockedFields
  | PC_Users
  deriving (Bounded, Enum, Eq, Ord, Show)

updateChecksums :: Document -> [PartialChecksumId] -> Document
updateChecksums m pcs = updateOverallChecksum $ foldl' updatePartialChecksum m pcs

updateAllChecksums :: Document -> Document
updateAllChecksums m = updateChecksums m [minBound .. maxBound]

updateOverallChecksum :: Document -> Document
updateOverallChecksum m =
  let overallChecksum = hashlazy $ encode $ M.elems $ m ^. #partialChecksums
   in m & #overallChecksum .~ overallChecksum

updatePartialChecksum :: Document -> PartialChecksumId -> Document
updatePartialChecksum m c = updatePartialChecksum' (computePartialChecksum c)
  where
    updatePartialChecksum' v = m & (#partialChecksums %~ M.insert c v)
    computePartialChecksum PC_CompetenceGrid = computePartialChecksum' #competenceGrid encode
    computePartialChecksum PC_Competences = computePartialChecksum' #competences encodeIx
    computePartialChecksum PC_Evidences = computePartialChecksum' #evidences encodeIx
    computePartialChecksum PC_LockedFields = computePartialChecksum' #lockedFields encode
    computePartialChecksum PC_Users = computePartialChecksum' #users encodeIx
    computePartialChecksum' :: Lens' Document a -> (a -> BL.ByteString) -> ByteString
    computePartialChecksum' l enc = hashlazy $ enc $ m ^. l
    encodeIx :: forall ixs a. (Binary a) => Ix.IxSet ixs a -> BL.ByteString
    encodeIx = encode . Ix.toList
