module Competences.Document.Solution
  ( Solution (..)
  , SolutionId
  , SolutionIxs
  , SolutionType (..)
  , mkSolution
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Type of solution
data SolutionType = Hint | Results | Complete
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON SolutionType

instance ToJSON SolutionType

instance Binary SolutionType

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
  , content :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance Ix.Indexable SolutionIxs Solution where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.taskId))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.solutionType))

instance FromJSON Solution

instance ToJSON Solution

instance Binary Solution

-- | Create a new solution with defaults
mkSolution :: SolutionId -> TaskId -> UserId -> Solution
mkSolution sid tid uid =
  Solution
    { id = sid
    , taskId = tid
    , userId = uid
    , solutionType = Hint
    , content = ""
    }
