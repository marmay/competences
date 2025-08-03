module Competences.Document
  ( Document (..)
  , PartialChecksumId (..)
  , emptyDocument
  , fieldATraversal
  , updateChecksums
  , updateAllChecksums
  , module Competences.Document.ChangableField
  , module Competences.Document.Competence
  , module Competences.Document.CompetenceGrid
  , module Competences.Document.Evidence
  , module Competences.Document.Order
  , module Competences.Document.Resource
  , module Competences.Document.User
  )
where

import Competences.Document.ChangableField (ChangableField (..))
import Competences.Document.Competence (Competence (..), CompetenceId, CompetenceIxs, Level (..))
import Competences.Document.CompetenceGrid
  ( CompetenceGrid (..)
  , CompetenceGridId
  , emptyCompetenceGrid
  )
import Competences.Document.Evidence (Evidence (..), EvidenceId, EvidenceIxs)
import Competences.Document.Order (Order, orderAt, orderMax, orderMin, ordered)
import Competences.Document.Resource (Resource (..), ResourceId, ResourceIxs)
import Competences.Document.User (User (..), UserId, UserIxs)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL (ByteString)
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Optics.AffineTraversal (AffineTraversal', atraversal)
import Optics.Core (Lens', castOptic, (%%), (%~), (&), (.~), (^.))

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

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v ->
    Document
      <$> v .: "competenceGrid"
      <*> (fmap Ix.fromList $ v .: "competences")
      <*> (fmap Ix.fromList $ v .: "evidences")
      <*> (fmap Ix.fromList $ v .: "resources")
      <*> (fmap M.fromList $ v .: "lockedFields")
      <*> (fmap Ix.fromList $ v .: "users")
      <*> (fmap (M.fromList . map (\(k, v') -> (k, Base64.decodeLenient (encodeUtf8 v')))) $ v .: "partialChecksums")
      <*> (fmap (Base64.decodeLenient . encodeUtf8) $ v .: "overallChecksum")

instance ToJSON Document where
  toJSON d =
    object
      [ "competenceGrid" .= d.competenceGrid
      , "competences" .= Ix.toList d.competences
      , "evidences" .= Ix.toList d.evidences
      , "resources" .= Ix.toList d.resources
      , "lockedFields" .= M.toList d.lockedFields
      , "users" .= Ix.toList d.users
      , "partialChecksums" .= map (\(k, v') -> (k, decodeUtf8 (Base64.encode v'))) (M.toList d.partialChecksums)
      , "overallChecksum" .= decodeUtf8 (Base64.encode d.overallChecksum)
      ]

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
fieldATraversal (CompetenceLevelDescription (competenceId, level)) = castOptic #competences %% ixATraversal competenceId %% castOptic #levelDescriptions %% defaultedMapTraversal level ""

ixATraversal
  :: forall ix a (ixs :: [Type])
   . (Ix.IsIndexOf ix ixs, Ix.Indexable ixs a) => ix -> AffineTraversal' (Ix.IxSet ixs a) a
ixATraversal ix' =
  let get :: Ix.IxSet ixs a -> Either (Ix.IxSet ixs a) a
      get ixSet = maybe (Left ixSet) Right $ Ix.getOne $ ixSet Ix.@= ix'
      set :: Ix.IxSet ixs a -> a -> Ix.IxSet ixs a
      set ixSet a = case Ix.getOne (ixSet Ix.@= ix') of
        Just _ -> Ix.insert a $ Ix.deleteIx ix' ixSet
        Nothing -> ixSet
   in atraversal get set

-- | Behaves as if the underlying map always contains the key; if it does not, it is assumed
-- to be equal to the default value passed as 2nd argument.
defaultedMapTraversal :: forall k v. (Ord k) => k -> v -> AffineTraversal' (M.Map k v) v
defaultedMapTraversal k v =
  let get :: M.Map k v -> Either (M.Map k v) v
      get m = maybe (Right v) Right $ M.lookup k m
      set :: M.Map k v -> v -> M.Map k v
      set m a = M.insert k a m
   in atraversal get set

data PartialChecksumId
  = PC_CompetenceGrid
  | PC_Competences
  | PC_Evidences
  | PC_LockedFields
  | PC_Users
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PartialChecksumId

instance ToJSON PartialChecksumId

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
