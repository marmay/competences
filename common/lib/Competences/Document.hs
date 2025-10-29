{-# LANGUAGE TypeFamilies #-}

module Competences.Document
  ( Document (..)
  , PartialChecksumId (..)
  , emptyDocument
  , updateChecksums
  , updateAllChecksums
  , module Competences.Document.Lock
  , module Competences.Document.Competence
  , module Competences.Document.CompetenceGrid
  , module Competences.Document.Evidence
  , module Competences.Document.Order
  , module Competences.Document.Resource
  , module Competences.Document.User
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Lock(Lock(..))
import Competences.Document.Competence (Competence (..), CompetenceId, CompetenceIxs, Level (..), levels)
import Competences.Document.CompetenceGrid
  ( CompetenceGrid (..)
  , CompetenceGridId
  , emptyCompetenceGrid, CompetenceGridIxs
  )
import Competences.Document.Evidence (Evidence (..), EvidenceId, EvidenceIxs)
import Competences.Document.Order (Order, orderAt, orderMax, orderMin, ordered)
import Competences.Document.Resource (Resource (..), ResourceId, ResourceIxs)
import Competences.Document.User (User (..), UserRole(..), UserId, UserIxs)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL (ByteString)
import Data.Map qualified as M
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Optics.Core
  ( An_AffineTraversal
  , Index
  , IxValue
  , Ixed (..)
  , Lens'
  , at
  , castOptic
  , ix
  , non
  , (%)
  , (%%)
  , (%~)
  , (&)
  , (.~)
  , (^.)
  )

data Document = Document
  { competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , resources :: !(Ix.IxSet ResourceIxs Resource)
  , locks :: !(M.Map Lock UserId)
  , users :: !(Ix.IxSet UserIxs User)
  , partialChecksums :: !(M.Map PartialChecksumId ByteString)
  , overallChecksum :: !ByteString
  }
  deriving (Eq, Generic, Show)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v ->
    Document
      <$> v .: "competenceGrids"
      <*> fmap Ix.fromList (v .: "competences")
      <*> fmap Ix.fromList (v .: "evidences")
      <*> fmap Ix.fromList (v .: "resources")
      <*> fmap M.fromList (v .: "lockedFields")
      <*> fmap Ix.fromList (v .: "users")
      <*> fmap
        (M.fromList . map (\(k, v') -> (k, Base64.decodeLenient (encodeUtf8 v'))))
        (v .: "partialChecksums")
      <*> fmap (Base64.decodeLenient . encodeUtf8) (v .: "overallChecksum")

instance ToJSON Document where
  toJSON d =
    object
      [ "competenceGrids" .= d.competenceGrids
      , "competences" .= Ix.toList d.competences
      , "evidences" .= Ix.toList d.evidences
      , "resources" .= Ix.toList d.resources
      , "locks" .= M.toList d.locks
      , "users" .= Ix.toList d.users
      , "partialChecksums"
          .= map (\(k, v') -> (k, decodeUtf8 (Base64.encode v'))) (M.toList d.partialChecksums)
      , "overallChecksum" .= decodeUtf8 (Base64.encode d.overallChecksum)
      ]

emptyDocument :: Document
emptyDocument =
  updateAllChecksums $
    Document
      { competenceGrids = Ix.empty
      , competences = Ix.empty
      , evidences = Ix.empty
      , resources = Ix.empty
      , locks = M.empty
      , users = Ix.empty
      , partialChecksums = M.empty
      , overallChecksum = ""
      }

data PartialChecksumId
  = PC_CompetenceGrid
  | PC_Competences
  | PC_Evidences
  | PC_Locks
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
    updatePartialChecksum' v = m & #partialChecksums %~ M.insert c v
    computePartialChecksum PC_CompetenceGrid = computePartialChecksum' #competenceGrids encodeIx
    computePartialChecksum PC_Competences = computePartialChecksum' #competences encodeIx
    computePartialChecksum PC_Evidences = computePartialChecksum' #evidences encodeIx
    computePartialChecksum PC_Locks = computePartialChecksum' #locks encode
    computePartialChecksum PC_Users = computePartialChecksum' #users encodeIx
    computePartialChecksum' :: Lens' Document a -> (a -> BL.ByteString) -> ByteString
    computePartialChecksum' l enc = hashlazy $ enc $ m ^. l
    encodeIx :: forall ixs a. (Binary a) => Ix.IxSet ixs a -> BL.ByteString
    encodeIx = encode . Ix.toList
