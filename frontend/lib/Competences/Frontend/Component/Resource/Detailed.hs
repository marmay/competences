-- | Full Miso component wrapping the detailed resource Fragment.
-- Parents embedding the Fragment inline should use 'Resource.Detailed.Embed'.
module Competences.Frontend.Component.Resource.Detailed
  ( ResourceDetailedConfig (..)
  , ResourceDetailedSettings (..)
  , defaultResourceDetailedSettings
  , resourceDetailedComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Resource (..), User)
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Component.Resource.Detailed.Embed (renderResource, updateResourceDetailed)
import Competences.Frontend.Component.Resource.EditButton (resourceEditButton)
import Competences.Frontend.Fragment.Resource.Detailed qualified as V
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , isTeacher
  , subscribeWithProjection
  )
import Competences.Frontend.View.Layout qualified as Layout
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((.~))

-- ============================================================================
-- Configuration
-- ============================================================================

data ResourceDetailedConfig = ResourceDetailedConfig
  { resourceId :: !ResourceId
  , settings :: !ResourceDetailedSettings
  }

data ResourceDetailedSettings = ResourceDetailedSettings
  { startExpanded :: !Bool
  -- ^ Initial expansion state for collapsible contents
  , showAnnotations :: !Bool
  -- ^ Show teacher-only annotations (edit button)
  }
  deriving (Eq, Show)

defaultResourceDetailedSettings :: ResourceDetailedSettings
defaultResourceDetailedSettings =
  ResourceDetailedSettings {startExpanded = True, showAnnotations = True}

-- ============================================================================
-- Model & Actions
-- ============================================================================

newtype ResourceProjection = ResourceProjection
  { resource :: Maybe Resource
  }
  deriving (Eq, Generic, Show)

data Model = Model
  { projection :: !ResourceProjection
  , viewState :: !V.ResourceDetailedState
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange ResourceProjection)
  | ViewAction !V.ResourceDetailedAction
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

resourceDetailedComponent :: SyncContext -> ResourceDetailedConfig -> M.Component p Model Action
resourceDetailedComponent r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (resourceProjection cfg) ProjectionChanged]
    }
  where
    model =
      Model
        { projection = ResourceProjection {resource = Nothing}
        , viewState =
            V.initialResourceDetailedState
              [cfg.resourceId | cfg.settings.startExpanded]
        }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction a) = updateResourceDetailed #viewState r ViewAction a

    view' m = case m.projection.resource of
      Nothing -> Layout.empty
      Just res -> renderResource r m.viewState annotations ViewAction res

    annotations res
      | cfg.settings.showAnnotations, isTeacher r = [resourceEditButton r res]
      | otherwise = []

-- ============================================================================
-- Projection
-- ============================================================================

resourceProjection :: ResourceDetailedConfig -> Document -> Maybe User -> ResourceProjection
resourceProjection cfg doc _mUser =
  ResourceProjection {resource = Ix.getOne (doc.resources Ix.@= cfg.resourceId)}
