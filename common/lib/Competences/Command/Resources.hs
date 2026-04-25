{-# LANGUAGE CPP #-}

module Competences.Command.Resources
  ( ResourcesCommand (..)
  , ResourcePatch (..)
  , handleResourcesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, CommandContext (..), EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Resource (Resource (..), ResourceContent, ResourceIdentifier)
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

-- | Apply a patch to a Resource.
applyResourcePatch :: Resource -> ResourcePatch -> Either Text Resource
applyResourcePatch resource patch =
  inContext "Resource" resource $
    patchField' @"identifier" patch
      >=> patchField' @"competenceLevels" patch
      >=> patchField' @"content" patch
      >=> patchField' @"attachments" patch

-- | Handle a Resources context command
handleResourcesCommand :: CommandContext -> ResourcesCommand -> Document -> UpdateResult
handleResourcesCommand cmdCtx (OnResources c) d =
  interpretEntityCommand resourceContext cmdCtx c d
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
