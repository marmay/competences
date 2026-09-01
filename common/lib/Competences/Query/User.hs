-- | User queries on the Document.
-- Provides efficient index-based lookups instead of filter-based patterns.
module Competences.Query.User
  ( -- * Single-entity lookup
    getUser
  , findUserByOffice365Id
  , findUserByEntraOid
    -- * Role-based queries
  , students
  , studentsSortedByName
  , teachers
    -- * All users
  , allUsersSortedByName
    -- * Multi-ID lookup
  , usersByIds
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User, UserIxs, UserId, UserRole (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Competences.Document.User (EntraOid, Office365Id)

-- | Lookup a user by primary key.
getUser :: Document -> UserId -> Maybe User
getUser doc userId = Ix.getOne $ doc.users Ix.@= userId

-- | Retrieves user by o365Id
findUserByOffice365Id :: Document -> Office365Id -> Maybe User
findUserByOffice365Id doc o365Id = Ix.getOne $ doc.users Ix.@= o365Id

-- | Retrieves user by bound Entra object id.
findUserByEntraOid :: Document -> EntraOid -> Maybe User
findUserByEntraOid doc oid = Ix.getOne $ doc.users Ix.@= oid

-- | All students in the document (via UserRole index).
students :: Document -> [User]
students doc = Ix.toList $ doc.users Ix.@= Student

-- | All students sorted by name (Text index, ascending).
studentsSortedByName :: Document -> [User]
studentsSortedByName doc = Ix.toAscList (Proxy @Text) $ doc.users Ix.@= Student

-- | All teachers in the document (via UserRole index).
teachers :: Document -> [User]
teachers doc = Ix.toList $ doc.users Ix.@= Teacher

-- | All users sorted by name (Text index, ascending).
allUsersSortedByName :: Document -> [User]
allUsersSortedByName doc = Ix.toAscList (Proxy @Text) doc.users

-- | Users matching a list of IDs (as IxSet for further filtering/sorting).
usersByIds :: Document -> [UserId] -> Ix.IxSet UserIxs User
usersByIds doc userIds = doc.users Ix.@+ userIds
