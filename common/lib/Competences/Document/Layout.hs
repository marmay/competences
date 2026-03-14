{-# LANGUAGE CPP #-}

module Competences.Document.Layout
  ( Layout (..)
  , LayoutId
  , LayoutIxs
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.Layout.Settings
  ( ContentPreset
  , ContentSettings
  , PrintSettings
  )
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Layout ID
type LayoutId = Id Layout

-- | Layout indexes
type LayoutIxs = '[LayoutId, AssignmentId]

-- | A saved print layout for an assignment
data Layout = Layout
  { id :: !LayoutId
  , assignmentId :: !AssignmentId
  , preset :: !ContentPreset
  , printSettings :: !PrintSettings
  , contentSettings :: !ContentSettings
  , createdAt :: !UTCTime
  }
  deriving (Eq, Generic, Ord, Show)

instance Ix.Indexable LayoutIxs Layout where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.assignmentId))

#ifdef WITH_AESON
instance FromJSON Layout

instance ToJSON Layout
#endif

instance Binary Layout
