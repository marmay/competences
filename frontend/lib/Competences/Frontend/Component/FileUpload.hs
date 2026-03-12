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
import Competences.Document.Id (idToText)
import Competences.Frontend.BinaryFFI (readFileFromInput)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Logging (logError)
import Competences.Frontend.SyncContext.SyncDocument (SyncContext, nextId, requestUploadPermission, uploadFile)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tailwind (class_)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.DSL (JSVal)
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Optics.Core qualified as O

-- | Upload status of the component.
data UploadStatus
  = Idle
  | RequestingPermission
  | Uploading
  | Failed !Text
  deriving (Eq, Show, Generic)

-- | Internal model for 'fileUploadComponent'.
data FileUploadModel = FileUploadModel
  { files :: ![FileRef]
  , uploadStatus :: !UploadStatus
  , inputId :: !MisoString
  }
  deriving (Eq, Show, Generic)

-- | Actions for 'fileUploadComponent'.
data FileUploadAction
  = -- | User selected a file in the browser input (carries DOM element ref)
    FileSelected !JSVal
  | -- | File was read from the browser
    FileRead !Text !Text !Int64 !BL.ByteString
  | -- | Permission granted, proceed with upload (carries name, mime, contents)
    PermissionGranted !Text !Text !BL.ByteString
  | -- | Upload completed (success or failure)
    UploadResult !(Either Text FileRef)
  | -- | Remove a file from the list by hash
    RemoveFile !SHA256Hash
  | -- | Generate a unique DOM id for the hidden input
    GenerateInputId
  | -- | Set the generated input id
    SetInputId !MisoString

instance Eq FileUploadAction where
  _ == _ = False

-- | Columns for the file table.
data FileCol = ColName | ColSize | ColActions
  deriving (Eq, Show)

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
  -> Maybe MisoString
  -> [FileRef]
  -> O.Lens' p [FileRef]
  -> M.Component p FileUploadModel FileUploadAction
fileUploadComponent syncCtx mTitle initialFiles parentLens =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #files]
    , M.initialAction = Just GenerateInputId
    }
  where
    model =
      FileUploadModel
        { files = initialFiles
        , uploadStatus = Idle
        , inputId = ""
        }

    update GenerateInputId =
      M.io $ do
        newId <- nextId syncCtx
        pure $ SetInputId ("file-upload-" <> ms (idToText newId))

    update (SetInputId iid) =
      M.modify $ \m -> m{inputId = iid}

    update (FileSelected domRef) =
      M.io $ do
        result <- readFileFromInput domRef
        case result of
          Nothing -> pure (UploadResult (Left "No file selected"))
          Just (name, mime, size, contents) -> pure (FileRead name mime size contents)

    update (FileRead name mime size contents) = do
      M.modify $ \m -> m{uploadStatus = RequestingPermission}
      M.withSink $ \sink ->
        requestUploadPermission syncCtx name mime size $ \case
          Left reason -> sink (UploadResult (Left reason))
          Right () -> sink (PermissionGranted name mime contents)

    update (PermissionGranted name mime contents) = do
      M.modify $ \m -> m{uploadStatus = Uploading}
      M.withSink $ \sink ->
        uploadFile syncCtx name mime contents $ \result ->
          sink (UploadResult result)

    update (UploadResult (Right ref)) =
      M.modify $ \m -> m{files = m.files ++ [ref], uploadStatus = Idle}

    update (UploadResult (Left err)) = do
      M.io_ $ logError $ ms $ "File upload failed: " <> err
      M.modify $ \m -> m{uploadStatus = Failed err}

    update (RemoveFile hash) =
      M.modify $ \m -> m{files = filter (\f -> f.hash /= hash) m.files}

    view m =
      MH.div_ [class_ "space-y-3"]
        [ -- Upload button row: optional title + hidden input + styled label + status
          MH.div_ [class_ "flex items-center gap-3"]
            $ maybe [] (\t -> [MH.span_ [class_ "font-medium text-sm"] [M.text t]]) mTitle
            ++ [ MH.span_ [class_ "ml-auto flex items-center gap-3"]
                   [ MH.input_
                       [ MP.type_ "file"
                       , MP.id_ m.inputId
                       , class_ "hidden"
                       , M.on "change" M.emptyDecoder $ \() domRef -> FileSelected domRef
                       ]
                   , MH.label_
                       [ MP.for_ m.inputId
                       , class_ "btn btn-secondary btn-sm cursor-pointer"
                       ]
                       [M.text $ C.translate' C.LblUploadFile]
                   , case m.uploadStatus of
                       Idle -> MH.span_ [] []
                       RequestingPermission ->
                         MH.span_ [class_ "text-sm text-muted-foreground animate-pulse"]
                           [M.text $ C.translate' C.LblUploading]
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
        , -- File list: table or empty-state text
          if null m.files
            then MH.div_ [class_ "text-sm text-muted-foreground italic"]
                   [M.text $ C.translate' C.LblNoFileSelected]
            else Table.viewTable $ Table.defTable
                   { Table.columns = [ColName, ColSize, ColActions]
                   , Table.rows = m.files
                   , Table.columnSpec = fileColumnSpec
                   , Table.rowContents = Table.cellContents fileCell
                   }
        ]

    fileColumnSpec ColName = Table.TableColumnSpec Table.EqualWidthColumn (C.translate' C.LblFile)
    fileColumnSpec ColSize = Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblSize)
    fileColumnSpec ColActions = Table.TableColumnSpec Table.SingleActionColumn ""

    fileCell ref ColName =
      MH.div_ [class_ "px-3 py-2 truncate"]
        [MH.span_ [class_ "text-sm font-medium"] [M.text $ ms ref.fileName]]
    fileCell ref ColSize =
      MH.div_ [class_ "px-3 py-2 whitespace-nowrap"]
        [MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ ms $ showFileSize ref.fileSize]]
    fileCell ref ColActions =
      MH.div_ [class_ "px-3 py-2"]
        [Button.ghostSm $ Button.button Icon.IcnDelete (RemoveFile ref.hash)]

-- | Format a file size in human-readable form.
showFileSize :: Int64 -> Text
showFileSize n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = T.pack (show (n `div` 1024)) <> " KB"
  | otherwise = T.pack (show (n `div` (1024 * 1024))) <> " MB"
