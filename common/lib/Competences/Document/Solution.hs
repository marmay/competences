{-# LANGUAGE CPP #-}

module Competences.Document.Solution
  ( Solution (..)
  , SolutionId
  , SolutionIxs
  , SolutionType (..)
  , solutionTypes
  , mkSolution
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import Competences.TaskContent.RichContent (RichContent)
import GHC.Generics (Generic)

-- | Type of solution
data SolutionType = Hint | Results | Complete
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON SolutionType

instance ToJSON SolutionType
#endif

instance Binary SolutionType

-- | All solution types
solutionTypes :: [SolutionType]
solutionTypes = [minBound .. maxBound]

-- | Solution ID
type SolutionId = Id Solution

-- | Solution indexes
type SolutionIxs = '[SolutionId, TaskId, UserId, SolutionType]

-- | A solution to a task
data Solution = Solution
  { id :: !SolutionId
  , taskId :: !TaskId
  , userId :: !UserId
  , solutionType :: !SolutionType
  , content :: !RichContent
  }
  deriving (Eq, Generic, Ord, Show)

instance Ix.Indexable SolutionIxs Solution where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.taskId))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.solutionType))

#ifdef WITH_AESON
instance FromJSON Solution

instance ToJSON Solution
#endif

instance Binary Solution

-- | Create a new solution with defaults
mkSolution :: SolutionId -> TaskId -> UserId -> Solution
mkSolution sid tid uid =
  Solution
    { id = sid
    , taskId = tid
    , userId = uid
    , solutionType = Hint
    , content = mempty
    }
