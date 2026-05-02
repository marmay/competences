{-# LANGUAGE CPP #-}

module Competences.Command.Solutions
  ( SolutionsCommand (..)
  , SolutionPatch (..)
  , handleSolutionsCommand
  )
where

import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (Change, CommandContext (..), EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..))
import Competences.Document.FileRef (FileRef)
import Competences.Document.Solution (Solution (..), SolutionType)
import Control.Monad ((>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON, withObject, (.:?))
import Data.Aeson qualified as Aeson
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Patch for modifying a Solution
data SolutionPatch = SolutionPatch
  { solutionType :: !(Change SolutionType)
  , content :: !(Change RichContent)
  , files :: !(Change [FileRef])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Solutions context
newtype SolutionsCommand = OnSolutions (EntityCommand Solution SolutionPatch)
  deriving (Eq, Generic, Show)

instance Binary SolutionPatch

#ifdef WITH_AESON
-- Hand-written to keep `files` optional in commands recorded before the
-- field existed.
instance FromJSON SolutionPatch where
  parseJSON = withObject "SolutionPatch" $ \v ->
    SolutionPatch
      <$> v .:? "solutionType"
      <*> v .:? "content"
      <*> v .:? "files"

instance ToJSON SolutionPatch where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
#endif

instance Binary SolutionsCommand

#ifdef WITH_AESON
instance FromJSON SolutionsCommand

instance ToJSON SolutionsCommand
#endif

-- Default instance
instance Default SolutionPatch where
  def =
    SolutionPatch
      { solutionType = Nothing
      , content = Nothing
      , files = Nothing
      }

-- | Apply a patch to a Solution, checking for conflicts
applySolutionPatch :: Solution -> SolutionPatch -> Either Text Solution
applySolutionPatch solution patch =
  inContext "Solution" solution $
    patchField' @"solutionType" patch
      >=> patchField' @"content" patch
      >=> patchField' @"files" patch

-- | Handle a Solutions context command
handleSolutionsCommand :: CommandContext -> SolutionsCommand -> Document -> UpdateResult
handleSolutionsCommand cmdCtx (OnSolutions c) =
  interpretEntityCommand solutionContext cmdCtx c
  where
    solutionContext =
      mkEntityCommandContext
        #solutions
        #id
        SolutionLock
        applySolutionPatch
        affectedUsersForSolution

    -- All users can see solutions
    affectedUsersForSolution :: Solution -> Document -> CommandAudience
    affectedUsersForSolution _ _ = AudienceAll
