module Competences.API.CourseList () where

import Data.Text (Text)
import Data.UUID (UUID)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.User (User)
import qualified Data.Set as S
import Servant.API

newtype CourseId = CourseId UUID
  deriving (Eq, Ord, Show)

data CourseDto = CourseDto
  { id :: !CourseId
  , competenceGridId :: !CompetenceGridId
  , name :: !Text
  , description :: !Text
  , users :: !(S.Set User)
  }

type CourseAPI = "course" :> ("list" :> Get '[JSON] [CourseDto])
