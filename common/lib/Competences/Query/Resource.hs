-- | Resource queries on the Document.
-- Provides reusable lookups for resources.
module Competences.Query.Resource
  ( -- * Single-entity lookup
    getResource
    -- * All resources
  , allResources
    -- * Multi-value queries
  , resourcesByLevels
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Resource, ResourceId)
import Competences.Document.Competence (CompetenceLevelId)

-- | Lookup a resource by primary key.
getResource :: Document -> ResourceId -> Maybe Resource
getResource doc resourceId = Ix.getOne $ doc.resources Ix.@= resourceId

-- | All resources in the document.
allResources :: Document -> [Resource]
allResources doc = Ix.toList doc.resources

-- | Resources associated with any of the given competence levels.
resourcesByLevels :: Document -> [CompetenceLevelId] -> [Resource]
resourcesByLevels doc levelIds = Ix.toList $ doc.resources Ix.@+ levelIds
