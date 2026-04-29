{-# LANGUAGE CPP #-}

module Competences.Command.Layouts
  ( LayoutsCommand (..)
  , LayoutPatch (..)
  , handleLayoutsCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..))
import Competences.Document.Layout (Layout (..))
import Competences.Document.Layout.Settings (ContentPreset, ContentSettings, PrintSettings)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.Text (Text)
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
handleLayoutsCommand :: CommandContext -> LayoutsCommand -> Document -> UpdateResult
handleLayoutsCommand cmdCtx (OnLayouts c) =
  interpretEntityCommand layoutContext cmdCtx c
  where
    layoutContext =
      mkEntityCommandContext
        #layouts
        #id
        LayoutLock
        applyLayoutPatch
        affectedUsersForLayout

    -- Only teachers see layouts
    affectedUsersForLayout :: Layout -> Document -> CommandAudience
    affectedUsersForLayout _ _ = AudienceTeachers
