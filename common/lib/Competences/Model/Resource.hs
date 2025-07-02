module Competences.Model.Resource
  ( ResourceId(..)
  , Resource(..)
  )
where

import Data.Text (Text)
import Data.UUID (UUID)

newtype ResourceId = ResourceId UUID
  deriving (Eq, Show)

data Resource = Resource
  { id :: !ResourceId
  , name :: !Text
  }
  deriving (Eq, Show)
