-- | Embedding helpers for the detailed resource view.
--
-- Depends only on the Fragment layer, so safe to import from any entity's
-- component module without creating cycles.
module Competences.Frontend.Component.Resource.Detailed.Embed
  ( updateResourceDetailed
  , renderResource
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), ResourcesCommand (..))
import Competences.Document (FileRef (..), Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Frontend.Component.FileGallery (fileGalleryComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Fragment.Resource.Detailed qualified as V
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext (..), modifySyncDocument, requestViewerPin, PinViewerRequest (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import Miso.Router qualified as M
import Miso.String (ms)
import Optics.Core (Lens', (%), (%~), (.~))

-- | Embeddable update: pass a lens at the parent's 'ResourceDetailedState'.
updateResourceDetailed
  :: Lens' model V.ResourceDetailedState
  -> SyncContext
  -> (V.ResourceDetailedAction -> action)
  -> V.ResourceDetailedAction
  -> M.Effect parent model action
updateResourceDetailed stateLens r lift = go
  where
    go (V.MenuEdit rid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Resources (OnResources (Modify rid Lock))
    go (V.MenuPin res) = do
      dismiss
      M.io_ $ requestViewerPin r (PinResourceViewer res)
    go (V.MenuGoTo rid) = do
      dismiss
      M.io_ $ M.pushURI (M.toURI (ManageResources (Just rid)))
    go (V.MenuDelete rid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Resources (OnResources (Delete rid))
    go (V.HoldDeleteEntity ha) =
      HoldButton.handleHoldAction'
        (stateLens % #holdDeleteEntity)
        (\rid -> modifySyncDocument r $ Resources (OnResources (Delete rid)))
        (lift . V.HoldDeleteEntity)
        ha
    go action = M.modify (stateLens %~ V.updateResourceDetailedPure action)

    dismiss = M.modify (stateLens % #menuOpen .~ Nothing)

renderResource
  :: SyncContext
  -> V.ResourceDetailedState
  -> (Resource -> [M.View m a])
  -> (V.ResourceDetailedAction -> a)
  -> Resource
  -> M.View m a
renderResource r state mkAnnotations liftAction res =
  let ResourceIdentifier identText = res.identifier
      displayName = if T.null identText then "(Unbenannt)" else identText
      expanded = Set.member res.id state.expandedResources
      toggle = liftAction (V.ToggleResource res.id)
      annotations = mkAnnotations res
   in case res.content of
        InlineContent rc
          | rc == mempty ->
              V.resourceStaticHeader
                (V.resourceContentIcon res.content)
                (ms displayName)
                annotations
          | otherwise ->
              V.resourceDisclosureView
                (V.resourceContentIcon res.content)
                toggle
                (ms displayName)
                annotations
                expanded
                (V.resourceContentView (renderRichTextWithFiles r.formulaCache r res.attachments rc))
        WebLink url title ->
          V.linkCardView
            (V.resourceContentIcon res.content)
            identText
            displayName
            url
            title
            annotations
        VideoLink url title ->
          V.linkCardView
            (V.resourceContentIcon res.content)
            identText
            displayName
            url
            title
            annotations
        FileContent fileRef ->
          V.resourceDisclosureView
            (V.resourceContentIcon res.content)
            toggle
            (ms displayName)
            annotations
            expanded
            (inlineComponent
                ("res-gallery-" <> ms (show fileRef.hash))
                (fileGalleryComponent r [fileRef]))
