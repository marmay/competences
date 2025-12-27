module Competences.Command.Users
  ( UsersCommand (..)
  , UserPatch (..)
  )
where

import Competences.Command.Common (Change, EntityCommand)
import Competences.Document.User (Office365Id, User, UserRole)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Patch for modifying a User (only editable fields)
data UserPatch = UserPatch
  { name :: !(Change Text)
    -- ^ Change name from old to new value
  , role :: !(Change UserRole)
    -- ^ Change role from old to new value
  , office365Id :: !(Change (Maybe Office365Id))
    -- ^ Change office365Id from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Users context
data UsersCommand
  = OnUsers !(EntityCommand User UserPatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON UserPatch
instance ToJSON UserPatch
instance Binary UserPatch

instance FromJSON UsersCommand
instance ToJSON UsersCommand
instance Binary UsersCommand
