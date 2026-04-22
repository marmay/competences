-- | Bridge between SearchSelect and the EditorField system.
-- Provides 'searchSelectEditorField' (multi-select) and
-- 'searchSelectSingleEditorField' (single-select) which use SearchSelect as
-- editor and a read-only viewer showing comma-separated labels.
module Competences.Frontend.Component.Selector.SearchSelectEditorField
  ( searchSelectEditorField
  , searchSelectSingleEditorField
  , addableSearchSelectEditorField
  , AddAction (..)
  )
where

import Competences.Command (Command)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), currentValue, mkFieldLens, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Editor.Types (Action (..), Model)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), selectorTransformedLens)
import Competences.Frontend.Component.Selector.SearchSelect
  ( SearchSelectConfig (..)
  , searchSelectComponent
  , searchSelectViewerComponent
  )
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Default (Default)
import Data.Maybe (listToMaybe, maybeToList)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- | Create an EditorField using SearchSelect for the editor and a read-only
-- viewer that shows comma-separated labels.
--
-- @extractIds@ extracts the initial selected IDs from the entity value.
-- The @EntityPatchTransformedLens@ handles reading/writing through the
-- entity and patch types.
searchSelectEditorField
  :: (Eq a, Eq id, Ord id, Show a, Show id, Ord entity, Default patch)
  => SyncContext
  -> M.MisoString
  -> SearchSelectConfig a id
  -> (entity -> [id])
  -> EntityPatchTransformedLens entity patch [] a f' a'
  -> EditorField entity patch ef
searchSelectEditorField r k cfg extractIds eptl =
  selectorEditorFieldWithViewer
    k
    eptl
    (\e stl -> searchSelectViewerComponent r cfg (extractIds e) stl)
    (\e p stl -> searchSelectComponent r k cfg (extractIds (withPatchedField eptl e p)) stl)

-- | Single-select variant of 'searchSelectEditorField'.
--
-- Bridges between @Maybe@ on the entity\/patch side and @[]@ on the
-- SearchSelect side by adapting the 'EntityPatchTransformedLens' embed
-- function through 'listToMaybe'.
searchSelectSingleEditorField
  :: (Eq a, Eq id, Ord id, Show a, Show id, Ord entity, Default patch)
  => SyncContext
  -> M.MisoString
  -> SearchSelectConfig a id
  -> (entity -> Maybe id)
  -> EntityPatchTransformedLens entity patch Maybe a f' a'
  -> EditorField entity patch ef
-- | Return a copy of the entity with its field replaced by the patched
-- value (if the patch modifies this field) — lets 'extractIds' see the
-- in-progress selection state on remount after minimize/restore.
withPatchedField
  :: EntityPatchTransformedLens entity patch f b f' b'
  -> entity -> patch -> entity
withPatchedField eptl e p =
  e & eptl.viewLens .~ currentValue e p eptl.viewLens eptl.patchLens

searchSelectSingleEditorField r k cfg extractId eptl =
  selectorEditorFieldWithViewer
    k
    adaptedEptl
    (\e stl -> searchSelectViewerComponent r cfg (maybeToList $ extractId e) stl)
    (\e p stl -> searchSelectComponent r k cfg (maybeToList $ extractId (withPatchedField eptl e p)) stl)
  where
    -- Adapt from Maybe to []: keep viewLens/patchLens unchanged (they store
    -- Maybe on the model side), but change embed so that the [] coming from
    -- SearchSelect is collapsed back to Maybe before writing.
    adaptedEptl =
      EntityPatchTransformedLens
        { viewLens = eptl.viewLens
        , patchLens = eptl.patchLens
        , transform = eptl.transform
        , embed = eptl.embed . listToMaybe
        }

-- | Spec for one "Add" button rendered beside the selector.
data AddAction entity patch = AddAction
  { label :: !C.Label
  , icon :: !Icon.Icon
  , mkSpec :: !(entity -> patch -> IO (Command, patch -> patch))
  -- ^ Given the current entity + patch, produce an IO action that
  -- generates a fresh ID, yields the 'CreateAndLock' command to fire,
  -- and a patch mutator that appends the new entity's id/item to the
  -- selection field. Dispatched via 'SpawnChild' on the Editor.
  }

-- | A search-select editor field plus one or more Add buttons that
-- spawn a new entity and append it to the selection in a single click.
-- The spawned entity's editor pin opens via LockWatching with a
-- follow-up pointing at this editor, so closing the spawned pin brings
-- this one back to the front.
addableSearchSelectEditorField
  :: forall a id entity patch ef
   . (Eq a, Eq id, Ord id, Show a, Show id, Ord entity, Default patch)
  => SyncContext
  -> M.MisoString
  -> SearchSelectConfig a id
  -> (entity -> [id])
  -> EntityPatchTransformedLens entity patch [] a [] id
  -> [AddAction entity patch]
  -> EditorField entity patch ef
addableSearchSelectEditorField r k cfg extractIds eptl addActions =
  EditorField
    { viewer = \entity ->
        inlineComponent (k <> "-viewer")
          (searchSelectViewerComponent r cfg (extractIds entity) (mkStl entity))
    , editor = \_refocus entity patch ->
        MH.div_
          [class_ "space-y-2"]
          [ inlineComponent (k <> "-editor")
              (searchSelectComponent r k cfg (extractIds (withPatchedField eptl entity patch)) (mkStl entity))
          , MH.div_
              [class_ "flex gap-1 justify-end"]
              (map (renderAddBtn entity patch) addActions)
          ]
    }
  where
    mkFieldLens' = mkFieldLens eptl.viewLens eptl.patchLens
    mkStl entity = selectorTransformedLens eptl.transform eptl.embed (mkFieldLens' entity)

    renderAddBtn :: entity -> patch -> AddAction entity patch -> M.View (Model entity patch ef) (Action entity patch)
    renderAddBtn entity patch action =
      Button.ghostSm
        (Button.ButtonConfig
          (Button.IconText action.icon (C.translate' action.label))
          (Just (SpawnChild entity (action.mkSpec entity patch))))
