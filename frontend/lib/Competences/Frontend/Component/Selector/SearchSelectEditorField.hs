-- | Bridge between SearchSelect and the EditorField system.
-- Provides 'searchSelectEditorField' (multi-select) and
-- 'searchSelectSingleEditorField' (single-select) which use SearchSelect as
-- editor and a read-only viewer showing comma-separated labels.
module Competences.Frontend.Component.Selector.SearchSelectEditorField
  ( searchSelectEditorField
  , searchSelectSingleEditorField
  )
where

import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
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
    (\e stl -> searchSelectComponent r cfg (extractIds e) stl)

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
searchSelectSingleEditorField r k cfg extractId eptl =
  selectorEditorFieldWithViewer
    k
    adaptedEptl
    (\e stl -> searchSelectViewerComponent r cfg (maybeToList $ extractId e) stl)
    (\e stl -> searchSelectComponent r cfg (maybeToList $ extractId e) stl)
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
