-- | Bridge between SearchSelect and the EditorField system.
-- Provides 'searchSelectEditorField' (multi-select) and
-- 'searchSelectSingleEditorField' (single-select) which use SearchSelect as
-- editor and a read-only viewer showing comma-separated labels.
module Competences.Frontend.Component.Selector.SearchSelectEditorField
  ( searchSelectEditorField
  , searchSelectSingleEditorField
  )
where

import Competences.Frontend.Component.Editor.EditorField (EditorField, currentValue, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
import Optics.Core ((&), (.~))
import Competences.Frontend.Component.Selector.SearchSelect
  ( SearchSelectConfig (..)
  , searchSelectComponent
  , searchSelectViewerComponent
  )
import Competences.Frontend.SyncContext (SyncContext)
import Data.Default (Default)
import Data.Maybe (listToMaybe, maybeToList)
import Miso qualified as M

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
