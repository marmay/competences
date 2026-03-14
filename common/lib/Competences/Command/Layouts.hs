{-# LANGUAGE CPP #-}

module Competences.Command.Layouts
  ( LayoutsCommand (..)
  , LayoutPatch (..)
  , handleLayoutsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Layout (Layout (..))
import Competences.Document.Layout.Settings (ContentPreset, ContentSettings, PrintSettings)
import Competences.Document.User (UserId, UserRole (..))
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.Text (Text)
import Data.IxSet.Typed qualified as IxSet
import GHC.Generics (Generic)

-- | Patch for modifying a Layout
data LayoutPatch = LayoutPatch
  { preset :: !(Change ContentPreset)
  , printSettings :: !(Change PrintSettings)
  , contentSettings :: !(Change ContentSettings)
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Layouts context
newtype LayoutsCommand = OnLayouts (EntityCommand Layout LayoutPatch)
  deriving (Eq, Generic, Show)

instance Binary LayoutPatch

#ifdef WITH_AESON
instance FromJSON LayoutPatch

instance ToJSON LayoutPatch
#endif

instance Binary LayoutsCommand

#ifdef WITH_AESON
instance FromJSON LayoutsCommand

instance ToJSON LayoutsCommand
#endif

instance Default LayoutPatch where
  def =
    LayoutPatch
      { preset = Nothing
      , printSettings = Nothing
      , contentSettings = Nothing
      }

-- | Apply a patch to a Layout
applyLayoutPatch :: Layout -> LayoutPatch -> Either Text Layout
applyLayoutPatch layout patch =
  inContext "Layout" layout $
    patchField' @"preset" patch
      >=> patchField' @"printSettings" patch
      >=> patchField' @"contentSettings" patch

-- | Handle a Layouts context command
handleLayoutsCommand :: UserId -> LayoutsCommand -> Document -> UpdateResult
handleLayoutsCommand userId (OnLayouts c) =
  interpretEntityCommand layoutContext userId c
  where
    layoutContext =
      mkEntityCommandContext
        #layouts
        #id
        LayoutLock
        applyLayoutPatch
        affectedUsersForLayout

    -- Only teachers see layouts, so affect all teachers
    affectedUsersForLayout :: Layout -> Document -> AffectedUsers
    affectedUsersForLayout _ d =
      AffectedUsers $ map (.id) $ filter (\u -> u.role == Teacher) $ IxSet.toList d.users
