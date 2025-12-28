module Competences.Command.Users
  ( UsersCommand (..)
  , UserPatch (..)
  , handleUsersCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Data.Default (Default (..))
import Competences.Document (Document (..), Lock (..))
import Competences.Document.User (Office365Id, User (..), UserId, UserRole)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((^.))
import Control.Monad ((>=>))

-- | Patch for modifying a User (only editable fields)
data UserPatch = UserPatch
  { name :: !(Change Text)
    -- ^ Change name from old to new value
  , role :: !(Change UserRole)
    -- ^ Change role from old to new value
  , office365Id :: !(Change Office365Id)
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

-- Default instances
instance Default UserPatch where
  def = UserPatch {name = Nothing, role = Nothing, office365Id = Nothing}

-- | Apply a patch to a User, checking for conflicts
applyUserPatch :: User -> UserPatch -> Either Text User
applyUserPatch user patch =
  inContext "User" user $
    patchField' @"name" patch
      >=> patchField' @"role" patch
      >=> patchField' @"office365Id" patch

-- | Handle a Users context command
handleUsersCommand :: UserId -> UsersCommand -> Document -> UpdateResult
handleUsersCommand userId (OnUsers c) = interpretEntityCommand userContext userId c
  where
    userContext =
      mkEntityCommandContext
        #users
        #id
        UserLock
        applyUserPatch
        (\_ d' -> allUsers d')
    allUsers d' = AffectedUsers $ map (.id) $ Ix.toList $ d' ^. #users
