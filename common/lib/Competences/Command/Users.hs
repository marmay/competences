{-# LANGUAGE CPP #-}

module Competences.Command.Users
  ( UsersCommand (..)
  , UserPatch (..)
  , handleUsersCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Data.Default (Default (..))
import Competences.Document (Document (..), Lock (..))
import Competences.Document.User (Office365Id, User (..), UserRole)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
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

instance Binary UserPatch
#ifdef WITH_AESON
instance FromJSON UserPatch
instance ToJSON UserPatch
#endif

instance Binary UsersCommand
#ifdef WITH_AESON
instance FromJSON UsersCommand
instance ToJSON UsersCommand
#endif

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
handleUsersCommand :: CommandContext -> UsersCommand -> Document -> UpdateResult
handleUsersCommand cmdCtx (OnUsers c) = interpretEntityCommand userContext cmdCtx c
  where
    userContext =
      mkEntityCommandContext
        #users
        #id
        UserLock
        applyUserPatch
        (\_ _ -> AudienceAll)
