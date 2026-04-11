{-# LANGUAGE CPP #-}

module Competences.Command.Resources
  ( ResourcesCommand (..)
  , ResourcePatch (..)
  , handleResourcesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Resource (Resource (..), ResourceContent, ResourceIdentifier)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Patch for modifying a Resource
data ResourcePatch = ResourcePatch
  { identifier :: !(Change ResourceIdentifier)
  , competenceLevels :: !(Change [CompetenceLevelId])
  , content :: !(Change ResourceContent)
  , attachments :: !(Change [FileRef])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Resources context
newtype ResourcesCommand = OnResources (EntityCommand Resource ResourcePatch)
  deriving (Eq, Generic, Show)

instance Binary ResourcePatch

#ifdef WITH_AESON
instance FromJSON ResourcePatch

instance ToJSON ResourcePatch
#endif

instance Binary ResourcesCommand

#ifdef WITH_AESON
instance FromJSON ResourcesCommand

instance ToJSON ResourcesCommand
#endif

-- Default instance
instance Default ResourcePatch where
  def =
    ResourcePatch
      { identifier = Nothing
      , competenceLevels = Nothing
      , content = Nothing
      , attachments = Nothing
      }

-- | Validate that a Resource has at least one competence level
validateResource :: Resource -> Either Text Resource
validateResource resource
  | null resource.competenceLevels = Left "Resource must have at least one competence level"
  | otherwise = Right resource

-- | Apply a patch to a Resource, checking for conflicts and validating invariants
applyResourcePatch :: Resource -> ResourcePatch -> Either Text Resource
applyResourcePatch resource patch =
  inContext "Resource" resource $
    patchField' @"identifier" patch
      >=> patchField' @"competenceLevels" patch
      >=> patchField' @"content" patch
      >=> patchField' @"attachments" patch
      >=> validateResource

-- | Handle a Resources context command
handleResourcesCommand :: UserId -> ResourcesCommand -> Document -> UpdateResult
handleResourcesCommand userId (OnResources c) d =
  case c of
    -- Validate new resources before creating
    Create r -> validateResource r >>= \_ -> interpretEntityCommand resourceContext userId c d
    CreateAndLock _r _ _ -> interpretEntityCommand resourceContext userId c d
    -- Other operations use applyPatch which already validates
    _ -> interpretEntityCommand resourceContext userId c d
  where
    resourceContext =
      mkEntityCommandContext
        #resources
        #id
        ResourceLock
        applyResourcePatch
        affectedUsersForResource

    -- All users can see resources
    affectedUsersForResource :: Resource -> Document -> AffectedUsers
    affectedUsersForResource _ d' =
      AffectedUsers $ map (.id) $ IxSet.toList $ d' ^. #users
