-- | Re-exports all SyncContext modules for backwards compatibility
-- and convenient imports.
module Competences.Frontend.SyncContext
  ( -- * Re-exports from SyncDocument
    module Competences.Frontend.SyncContext.SyncDocument
    -- * Re-exports from ProjectedSubscription
  , module Competences.Frontend.SyncContext.ProjectedSubscription
  )
where

import Competences.Frontend.SyncContext.ProjectedSubscription
import Competences.Frontend.SyncContext.SyncDocument
