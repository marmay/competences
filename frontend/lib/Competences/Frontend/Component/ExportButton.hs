module Competences.Frontend.Component.ExportButton
  ( exportButtonComponent
  )
where

import Competences.Frontend.Clipboard (copyToClipboard)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Control.Concurrent (forkIO, threadDelay)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((.~))

-- | Model for the export button component
newtype ExportModel = ExportModel
  { exportSuccess :: Bool
  }
  deriving (Eq, Generic, Show)

-- | Actions for the export button component
data ExportAction
  = -- | User clicked the export button
    RequestExport
  | -- | Perform export with the given text
    DoExport !Text
  | ClearExportSuccess
  deriving (Eq, Show)

-- | Reusable export button component
--
-- Takes a function to extract export text from the parent model.
-- The component manages its own success state and shows a checkmark
-- for 3 seconds after successful export.
--
-- Usage:
--
-- @
-- V.inlineComponent "export-btn"
--   (exportButtonComponent (\\m -> exportCompetenceGrid m.document grid))
-- @
exportButtonComponent
  :: (parent -> Text)
  -- ^ Function to get export text from parent model
  -> M.Component parent ExportModel ExportAction
exportButtonComponent extractText =
  M.component initialModel update view
  where
    initialModel = ExportModel {exportSuccess = False}

    -- When user clicks, use parent to fetch parent model and get export text
    update RequestExport =
      M.parent (DoExport . extractText) ClearExportSuccess
    update (DoExport text) = do
      M.modify $ #exportSuccess .~ True
      M.withSink $ \sink -> do
        copyToClipboard text
        _ <- forkIO $ do
          threadDelay 3000000 -- 3 seconds
          sink ClearExportSuccess
        pure ()
    update ClearExportSuccess =
      M.modify $ #exportSuccess .~ False

    view m
      | m.exportSuccess = Button.secondary $ Button.button Icon.IcnApply Button.Disabled
      | otherwise = Button.secondary $ Button.button (Icon.IcnExport, C.LblExport) RequestExport
