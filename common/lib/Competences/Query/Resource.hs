-- | Resource queries on the Document.
-- Provides reusable lookups for resources.
module Competences.Query.Resource
  ( allResources
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Resource)

-- | All resources in the document.
allResources :: Document -> [Resource]
allResources doc = Ix.toList doc.resources
