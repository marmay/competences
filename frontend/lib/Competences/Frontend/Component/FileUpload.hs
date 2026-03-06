-- | Reusable file upload component for Miso.
--
-- Follows the same self-contained component pattern as 'richContentEditorComponent':
-- manages its own upload state and bidirectionally binds a @[FileRef]@ field on
-- the parent model.
--
-- For resources (single file), the parent uses @listToMaybe files@.
-- For future assignment hand-ins, the full list is used.
module Competences.Frontend.Component.FileUpload
  ( fileUploadComponent
  , FileUploadModel
  , FileUploadAction
  , showFileSize
  )
where

import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Frontend.BinaryFFI (readFileFromInput)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Logging (logError)
import Competences.Frontend.SyncContext.SyncDocument (SyncContext, uploadFile)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.DSL (JSVal)
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)
import Optics.Core qualified as O

-- | Upload status of the component.
data UploadStatus
  = Idle
  | Uploading
  | Failed !Text
  deriving (Eq, Show, Generic)

-- | Internal model for 'fileUploadComponent'.
data FileUploadModel = FileUploadModel
  { files :: ![FileRef]
  , uploadStatus :: !UploadStatus
  }
  deriving (Eq, Show, Generic)

-- | Actions for 'fileUploadComponent'.
data FileUploadAction
  = -- | User selected a file in the browser input (carries DOM element ref)
    FileSelected !JSVal
  | -- | File was read from the browser
    FileRead !Text !Text !Int64 !BL.ByteString
  | -- | Upload completed (success or failure)
    UploadResult !(Either Text FileRef)
  | -- | Remove a file from the list by hash
    RemoveFile !SHA256Hash

instance Eq FileUploadAction where
  _ == _ = False

-- | A self-contained Miso component for uploading files.
--
-- Takes a 'SyncContext' for the upload API, initial file refs,
-- and a parent lens pointing to @[FileRef]@. The component manages
-- its own upload state and file list.
--
-- @
-- inlineComponent "file-upload"
--   (fileUploadComponent r [] #fileRefs)
-- @
fileUploadComponent
  :: SyncContext
  -> [FileRef]
  -> O.Lens' p [FileRef]
  -> M.Component p FileUploadModel FileUploadAction
fileUploadComponent syncCtx initialFiles parentLens =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #files]
    }
  where
    model =
      FileUploadModel
        { files = initialFiles
        , uploadStatus = Idle
        }

    update (FileSelected domRef) =
      M.io $ do
        result <- readFileFromInput domRef
        case result of
          Nothing -> pure (UploadResult (Left "No file selected"))
          Just (name, mime, size, contents) -> pure (FileRead name mime size contents)

    update (FileRead name mime _size contents) = do
      M.modify $ \m -> m{uploadStatus = Uploading}
      M.io $ do
        result <- uploadFile syncCtx name mime contents
        pure (UploadResult result)

    update (UploadResult (Right ref)) =
      M.modify $ \m -> m{files = m.files ++ [ref], uploadStatus = Idle}

    update (UploadResult (Left err)) = do
      M.io_ $ logError $ ms $ "File upload failed: " <> err
      M.modify $ \m -> m{uploadStatus = Failed err}

    update (RemoveFile hash) =
      M.modify $ \m -> m{files = filter (\f -> f.hash /= hash) m.files}

    view m =
      MH.div_ [class_ "space-y-3"]
        [ -- List of uploaded files
          if null m.files
            then MH.div_ [class_ "text-sm text-muted-foreground italic"]
                   [M.text $ C.translate' C.LblNoFileSelected]
            else MH.div_ [class_ "space-y-2"] (map viewFile m.files)
        , -- Upload area
          MH.div_ [class_ "flex items-center gap-3"]
            [ MH.input_
                [ MP.type_ "file"
                , class_ "text-sm file:mr-4 file:py-2 file:px-4 file:rounded-md file:border-0 file:text-sm file:font-medium file:bg-sky-50 file:text-sky-700 hover:file:bg-sky-100"
                , M.on "change" M.emptyDecoder $ \() domRef -> FileSelected domRef
                ]
            , case m.uploadStatus of
                Idle -> MH.span_ [] []
                Uploading ->
                  MH.span_ [class_ "text-sm text-muted-foreground animate-pulse"]
                    [M.text $ C.translate' C.LblUploading]
                Failed err ->
                  MH.span_ [class_ "text-sm text-red-600"]
                    [ M.text $ C.translate' C.LblFileUploadFailed
                    , M.text ": "
                    , M.text $ ms err
                    ]
            ]
        ]

    viewFile ref =
      MH.div_ [class_ "flex items-center gap-3 p-2 bg-stone-50 rounded-md border border-stone-200"]
        [ MH.div_ [class_ "flex-1 min-w-0"]
            [ MH.div_ [class_ "text-sm font-medium truncate"] [M.text $ ms ref.fileName]
            , Typography.small $ ms $
                ref.mimeType <> " (" <> showFileSize ref.fileSize <> ")"
            ]
        , Button.destructiveSm $ Button.button C.LblDelete (RemoveFile ref.hash)
        ]

-- | Format a file size in human-readable form.
showFileSize :: Int64 -> Text
showFileSize n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = T.pack (show (n `div` 1024)) <> " KB"
  | otherwise = T.pack (show (n `div` (1024 * 1024))) <> " MB"
