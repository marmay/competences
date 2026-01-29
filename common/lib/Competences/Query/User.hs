-- | User queries on the Document.
-- Provides efficient index-based lookups instead of filter-based patterns.
module Competences.Query.User
  ( students
  , studentsSortedByName
  , teachers
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User, UserRole (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)

-- | All students in the document (via UserRole index).
students :: Document -> [User]
students doc = Ix.toList $ doc.users Ix.@= Student

-- | All students sorted by name (Text index, ascending).
studentsSortedByName :: Document -> [User]
studentsSortedByName doc = Ix.toAscList (Proxy @Text) $ doc.users Ix.@= Student

-- | All teachers in the document (via UserRole index).
teachers :: Document -> [User]
teachers doc = Ix.toList $ doc.users Ix.@= Teacher
