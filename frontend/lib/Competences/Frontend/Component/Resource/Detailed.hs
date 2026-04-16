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
import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), ResourcesCommand (..))
import Competences.Document (Document (..), Lock (..), Resource (..), User)
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.Component.Resource.Detailed.Embed (renderResource, updateResourceDetailed)
import Competences.Frontend.Fragment.Resource.Detailed qualified as V
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , isTeacher
  , subscribeWithProjection
  )
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.EntityMenu (entityMenu, menuPin, menuGoTo, menuSeparator)
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Layout qualified as Layout
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((.~))

data ResourceDetailedConfig = ResourceDetailedConfig
  { resourceId :: !ResourceId
  , settings :: !ResourceDetailedSettings
  }

data ResourceDetailedSettings = ResourceDetailedSettings
  { startExpanded :: !Bool
  , showAnnotations :: !Bool
  , enableGoTo :: !Bool
  , enableDelete :: !Bool
  }
  deriving (Eq, Show)

defaultResourceDetailedSettings :: ResourceDetailedSettings
defaultResourceDetailedSettings =
  ResourceDetailedSettings
    { startExpanded = True
    , showAnnotations = True
    , enableGoTo = True
    , enableDelete = False
    }

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
      Just res -> renderResource r m.viewState (annotations m) ViewAction res

    annotations m res
      | cfg.settings.showAnnotations, isTeacher r =
          [entityMenu m.viewState.menuOpen (ViewAction V.MenuToggle) (ViewAction V.MenuClose) $
            [ editButton r res.id
            , menuPin (ViewAction (V.MenuPin res))
            ]
            ++ [menuGoTo (ViewAction (V.MenuGoTo res.id)) | cfg.settings.enableGoTo]
            ++ [menuSeparator | cfg.settings.enableDelete]
            ++ [HoldButton.holdDeleteButtonSm (ViewAction . V.HoldDeleteEntity) m.viewState.holdDeleteEntity res.id | cfg.settings.enableDelete]
          ]
      | otherwise = []

    editButton :: SyncContext -> ResourceId -> M.View Model Action
    editButton r' rid =
      inlineComponent
        ("res-edit-btn-" <> ms (show rid))
        (lockButtonComponent r'
          (LockButtonConfig (ResourceLock rid) (Resources (OnResources (Modify rid Lock))) Button.IconTextS))

resourceProjection :: ResourceDetailedConfig -> Document -> Maybe User -> ResourceProjection
resourceProjection cfg doc _mUser =
  ResourceProjection {resource = Ix.getOne (doc.resources Ix.@= cfg.resourceId)}
