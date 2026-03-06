-- | Self-contained Miso component for previewing files embedded in markdown.
--
-- Each @![](file:...)@ in the markdown mounts one of these.
-- Handles file loading via 'downloadFile' and dispatches to
-- type-specific renderers based on MIME type.
module Competences.Frontend.Component.FilePreview
  ( filePreviewComponent
  , FilePreviewModel
  , FilePreviewAction
  )
where

import Competences.Document.FileRef (FileRef (..))
import Competences.Frontend.FileCache (fileToDataUrl)
import Competences.Frontend.SyncContext.SyncDocument (SyncContext, downloadFile)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.Component.FileUpload (showFileSize)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)

-- | Model tracks file loading state and enlarge state.
data FilePreviewModel = FilePreviewModel
  { fileRef :: !FileRef
  , dataUrl :: !(Maybe Text)
  , enlarged :: !Bool
  }
  deriving (Eq, Show, Generic)

-- | Actions for the file preview component.
data FilePreviewAction
  = LoadFile
  | FileLoaded !Text
  | FileNotAvailable
  | ToggleEnlarged
  deriving (Eq, Show)

-- | Check if a MIME type is an image type.
isImageMime :: Text -> Bool
isImageMime mime =
  any (`T.isPrefixOf` mime)
    ["image/jpeg", "image/png", "image/gif", "image/webp", "image/svg"]

-- | A self-contained Miso component for previewing a single file.
filePreviewComponent :: SyncContext -> FileRef -> M.Component p FilePreviewModel FilePreviewAction
filePreviewComponent syncCtx ref =
  (M.component model update view)
    { M.initialAction = Just LoadFile
    }
  where
    model =
      FilePreviewModel
        { fileRef = ref
        , dataUrl = Nothing
        , enlarged = False
        }

    update LoadFile = M.io $ do
      mData <- downloadFile syncCtx ref.hash
      case mData of
        Just bs -> pure $ FileLoaded (fileToDataUrl ref.mimeType bs)
        Nothing -> pure FileNotAvailable

    update (FileLoaded url) =
      M.modify $ \m -> m{dataUrl = Just url}

    update FileNotAvailable =
      M.modify $ \m -> m{dataUrl = Just ""}

    update ToggleEnlarged =
      M.modify $ \m -> m{enlarged = not m.enlarged}

    view m = case m.dataUrl of
      Nothing -> viewLoading m.fileRef
      Just "" -> viewNotFound m.fileRef
      Just url
        | isImageMime m.fileRef.mimeType -> viewImage url m.fileRef m.enlarged
        | otherwise -> viewFileCard m.fileRef

    viewLoading fr =
      MH.div_ [class_ "inline-flex items-center gap-2 px-3 py-2 bg-stone-100 rounded-md animate-pulse"]
        [ MH.span_ [class_ "text-sm text-stone-500"] [M.text $ ms fr.fileName]
        ]

    viewNotFound fr =
      MH.div_ [class_ "inline-flex items-center gap-2 px-3 py-2 bg-red-50 rounded-md border border-red-200"]
        [ MH.span_ [class_ "text-sm text-red-500"]
            [M.text $ ms $ "Datei nicht gefunden: " <> fr.fileName]
        ]

    viewImage url fr isEnlarged =
      MH.div_ []
        [ MH.img_
            [ MP.src_ (ms url)
            , MP.alt_ (ms fr.fileName)
            , class_ "max-h-64 rounded-md cursor-pointer hover:opacity-90 transition-opacity"
            , MP.title_ "Klicken zum Vergrößern"
            , MH.onClick ToggleEnlarged
            ]
        , if isEnlarged
            then viewEnlargedModal url fr
            else MH.span_ [] []
        ]

    viewEnlargedModal url fr =
      MH.div_
        [ class_ "fixed inset-0 z-50 flex items-center justify-center bg-black/80 cursor-pointer"
        , MH.onClick ToggleEnlarged
        ]
        [ MH.img_
            [ MP.src_ (ms url)
            , MP.alt_ (ms fr.fileName)
            , class_ "max-w-[90vw] max-h-[90vh] object-contain rounded-lg shadow-2xl"
            ]
        ]

    viewFileCard fr =
      MH.div_ [class_ "inline-flex items-center gap-3 px-3 py-2 bg-stone-50 rounded-md border border-stone-200"]
        [ MH.div_ [class_ "flex-1 min-w-0"]
            [ MH.div_ [class_ "text-sm font-medium truncate"] [M.text $ ms fr.fileName]
            , MH.span_ [class_ "text-xs text-stone-500"]
                [M.text $ ms $ fr.mimeType <> " (" <> showFileSize fr.fileSize <> ")"]
            ]
        ]
