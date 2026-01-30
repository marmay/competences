module Competences.Command.Solutions
  ( SolutionsCommand (..)
  , SolutionPatch (..)
  , handleSolutionsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Solution (Solution (..), SolutionType)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Patch for modifying a Solution
data SolutionPatch = SolutionPatch
  { solutionType :: !(Change SolutionType)
  , content :: !(Change RichContent)
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Solutions context
newtype SolutionsCommand = OnSolutions (EntityCommand Solution SolutionPatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON SolutionPatch

instance ToJSON SolutionPatch

instance Binary SolutionPatch

instance FromJSON SolutionsCommand

instance ToJSON SolutionsCommand

instance Binary SolutionsCommand

-- Default instance
instance Default SolutionPatch where
  def =
    SolutionPatch
      { solutionType = Nothing
      , content = Nothing
      }

-- | Apply a patch to a Solution, checking for conflicts
applySolutionPatch :: Solution -> SolutionPatch -> Either Text Solution
applySolutionPatch solution patch =
  inContext "Solution" solution $
    patchField' @"solutionType" patch
      >=> patchField' @"content" patch

-- | Handle a Solutions context command
handleSolutionsCommand :: UserId -> SolutionsCommand -> Document -> UpdateResult
handleSolutionsCommand userId (OnSolutions c) =
  interpretEntityCommand solutionContext userId c
  where
    solutionContext =
      mkEntityCommandContext
        #solutions
        #id
        SolutionLock
        applySolutionPatch
        affectedUsersForSolution

    -- All users can see solutions
    affectedUsersForSolution :: Solution -> Document -> AffectedUsers
    affectedUsersForSolution _ d =
      AffectedUsers $ map (.id) $ IxSet.toList $ d ^. #users
