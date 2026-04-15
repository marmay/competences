-- | Standalone edit button for resources.
-- Polymorphic — can be used in any parent component's view.
module Competences.Frontend.Component.Resource.EditButton
  ( resourceEditButton
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), ResourcesCommand (..))
import Competences.Document (Lock (..), Resource (..))
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Miso qualified as M
import Miso.String (ms)

-- | LockButton for editing a resource. Opens the pin editor for the resource.
resourceEditButton :: SyncContext -> Resource -> M.View m a
resourceEditButton r res =
  inlineComponent
    ("resource-edit-btn-" <> ms (show res.id))
    ( lockButtonComponent
        r
        ( LockButtonConfig
            (ResourceLock res.id)
            (Resources (OnResources (Modify res.id Lock)))
            Button.IconOnlyS
        )
    )
