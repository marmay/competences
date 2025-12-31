module Competences.Frontend.Component.Selector.MultiTaskSelector
  ( multiTaskEditorField
  )
where

import Competences.Document.Task (TaskId)
import Competences.Frontend.Component.Editor.EditorField (EditorField (..))
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..))
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core ((^.))

-- | Editor field for selecting multiple tasks
-- For now, this is a simplified read-only display
multiTaskEditorField
  :: SyncDocumentRef
  -> M.MisoString
  -> EntityPatchTransformedLens entity patch [] TaskId [] TaskId
  -> EditorField entity patch f
multiTaskEditorField _r _key lens =
  EditorField
    { viewer = \entity ->
        let taskIds = entity ^. lens.viewLens
         in viewTaskList taskIds
    , editor = \_ entity _ ->
        let taskIds = entity ^. lens.viewLens
         in viewTaskList taskIds
    }
  where
    viewTaskList taskIds =
      if null taskIds
        then M.div_ [] [M.text "No tasks"]
        else M.div_
          []
          [ M.text $ ms $ show (length taskIds) <> " tasks"
          ]
