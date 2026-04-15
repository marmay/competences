-- | Embedding helpers for the detailed resource view.
--
-- Depends only on the pure view layer, so it is safe to import from any
-- entity's component module without creating cycles.
module Competences.Frontend.Component.Resource.Detailed.Embed
  ( updateResourceDetailed
  , renderResource
  )
where

import Competences.Document (FileRef (..), Resource (..), ResourceContent (..), ResourceIdentifier (..))
import Competences.Frontend.Component.FileGallery (fileGalleryComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Fragment.Resource.Detailed qualified as V
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens', (%~))

-- | Embeddable update: pass a lens at the parent's 'ResourceDetailedState'.
updateResourceDetailed
  :: Lens' model V.ResourceDetailedState
  -> SyncContext
  -> (V.ResourceDetailedAction -> action)
  -> V.ResourceDetailedAction
  -> M.Effect parent model action
updateResourceDetailed stateLens _ _ action =
  M.modify (stateLens %~ V.updateResourceDetailedPure action)

-- | Render a single resource with its body in the shape determined by content type.
--
-- * 'InlineContent' with content — collapsible disclosure.
-- * 'InlineContent' empty — static header.
-- * 'WebLink' / 'VideoLink' — link card.
-- * 'FileContent' — collapsible disclosure containing the file gallery.
renderResource
  :: SyncContext
  -> V.ResourceDetailedState
  -> (Resource -> [M.View m a])
  -- ^ Per-resource annotations (edit button, badges, …)
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

