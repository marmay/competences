-- | Bridge between SearchSelect and the EditorField system.
-- Provides 'searchSelectEditorField' which uses SearchSelect as editor
-- and a read-only viewer showing comma-separated labels.
module Competences.Frontend.Component.Selector.SearchSelectEditorField
  ( searchSelectEditorField
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
