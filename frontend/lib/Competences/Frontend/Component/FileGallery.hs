-- | Reusable file gallery component.
--
-- Shows images in a navigable gallery with prev/next controls and a download
-- button. Non-image files appear in a popover (when mixed with images) or as
-- a plain file list (when no images are present). Empty file lists render nothing.
module Competences.Frontend.Component.FileGallery
  ( fileGalleryComponent
  , FileGalleryModel
  , FileGalleryAction
  , isImageMime
  )
where

import Competences.Document.FileRef (FileRef (..), SHA256Hash)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.FileCache (fileToDataUrl)
import Competences.Frontend.SyncContext (SyncContext (..), downloadFile)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)

-- | Check if a MIME type is an image type.
isImageMime :: Text -> Bool
isImageMime mime =
  any (`T.isPrefixOf` mime)
    ["image/jpeg", "image/png", "image/gif", "image/webp", "image/svg"]

-- ===========================================================================
-- Model & Actions
-- ===========================================================================

data FileGalleryModel = FileGalleryModel
  { imageFiles :: ![(FileRef, Maybe Text)]
  -- ^ Image files with their loaded data URL (Nothing = loading)
  , nonImageFiles :: ![FileRef]
  -- ^ Non-image files
  , currentImageIndex :: !Int
  , showFilePopover :: !Bool
  }
  deriving (Eq, Show, Generic)

data FileGalleryAction
  = LoadFiles
  | ImageLoaded !SHA256Hash !Text
  | ImageNotAvailable !SHA256Hash
  | PrevImage
  | NextImage
  | ToggleFilePopover
  deriving (Eq, Show)

-- ===========================================================================
-- Component
-- ===========================================================================

-- | A self-contained gallery component for a list of file references.
--
-- Behaviour:
--   * Has images → navigable image gallery; non-images in a popover.
--   * All non-images → plain file table.
--   * Empty list → renders nothing.
fileGalleryComponent
  :: SyncContext -> [FileRef]
  -> M.Component p FileGalleryModel FileGalleryAction
fileGalleryComponent r files =
  (M.component model update view')
    { M.initialAction = Just LoadFiles
    }
  where
    imgs = filter (isImageMime . (.mimeType)) files
    nonImgs = filter (not . isImageMime . (.mimeType)) files

    model =
      FileGalleryModel
        { imageFiles = map (, Nothing) imgs
        , nonImageFiles = nonImgs
        , currentImageIndex = 0
        , showFilePopover = False
        }

    update LoadFiles =
      mapM_ initiateDownload imgs

    update (ImageLoaded hash url) = M.modify $ \m ->
      m
        { imageFiles =
            [ if ref.hash == hash then (ref, Just url) else (ref, mUrl)
            | (ref, mUrl) <- m.imageFiles
            ]
        }

    update (ImageNotAvailable hash) = M.modify $ \m ->
      m
        { imageFiles =
            [ if ref.hash == hash then (ref, Just "") else (ref, mUrl)
            | (ref, mUrl) <- m.imageFiles
            ]
        }

    update PrevImage = M.modify $ \m ->
      let total = length m.imageFiles
          newIdx = if m.currentImageIndex > 0 then m.currentImageIndex - 1 else total - 1
       in m{currentImageIndex = newIdx}

    update NextImage = M.modify $ \m ->
      let total = length m.imageFiles
          newIdx = if m.currentImageIndex < total - 1 then m.currentImageIndex + 1 else 0
       in m{currentImageIndex = newIdx}

    update ToggleFilePopover = M.modify $ \m ->
      m{showFilePopover = not m.showFilePopover}

    initiateDownload ref = M.io $ do
      mData <- downloadFile r ref.hash
      case mData of
        Just bs -> pure $ ImageLoaded ref.hash (fileToDataUrl ref.mimeType bs)
        Nothing -> pure $ ImageNotAvailable ref.hash

    view' m
      | null m.imageFiles && null m.nonImageFiles =
          MH.span_ [] []
      | null m.imageFiles =
          viewFileList m.nonImageFiles
      | otherwise =
          Layout.vFlow
            mempty
            [ viewImageGallery m
            , viewGalleryBottomBar m
            ]

-- ---------------------------------------------------------------------------
-- Image Gallery Views
-- ---------------------------------------------------------------------------

viewImageGallery :: FileGalleryModel -> M.View m FileGalleryAction
viewImageGallery m =
  let idx = m.currentImageIndex
      mEntry = if idx < length m.imageFiles then Just (m.imageFiles !! idx) else Nothing
   in MH.div_
        [class_ "bg-stone-50 rounded-t-lg flex items-center justify-center min-h-48"]
        [ case mEntry of
            Nothing -> M.text ""
            Just (ref, Nothing) ->
              MH.div_
                [class_ "flex flex-col items-center gap-2 p-8 animate-pulse"]
                [MH.span_ [class_ "text-sm text-stone-500"] [M.text $ ms ref.fileName]]
            Just (_ref, Just "") ->
              MH.div_
                [class_ "p-8 text-red-500 text-sm"]
                [M.text "Datei nicht verfügbar"]
            Just (ref, Just url) ->
              MH.img_
                [ MP.src_ (ms url)
                , MP.alt_ (ms ref.fileName)
                , class_ "max-h-96 object-contain"
                ]
        ]

viewGalleryBottomBar :: FileGalleryModel -> M.View m FileGalleryAction
viewGalleryBottomBar m =
  let totalImages = length m.imageFiles
      hasNonImageFiles = not (null m.nonImageFiles)
      showNav = totalImages > 1
      idx = m.currentImageIndex
      mEntry = if idx < length m.imageFiles then Just (m.imageFiles !! idx) else Nothing
   in MH.div_
        [class_ "flex items-center justify-between px-3 py-2 bg-stone-100 rounded-b-lg border-t border-stone-200"]
        [ -- Left: navigation controls
          if showNav
            then
              Layout.hFlow
                (Layout.gapS <> Layout.crossCenter)
                [ MH.button_
                    [ class_ "p-1 rounded hover:bg-stone-200 transition-colors"
                    , MH.onClick PrevImage
                    ]
                    [Icon.iconS Icon.Small Icon.IcnArrowUp]
                , MH.span_
                    [class_ "text-sm text-muted-foreground font-medium tabular-nums"]
                    [M.text $ ms (show (m.currentImageIndex + 1)) <> "/" <> ms (show totalImages)]
                , MH.button_
                    [ class_ "p-1 rounded hover:bg-stone-200 transition-colors"
                    , MH.onClick NextImage
                    ]
                    [Icon.iconS Icon.Small Icon.IcnArrowDown]
                ]
            else MH.span_ [] []
        , -- Right: download button + file indicator
          Layout.hFlow
            (Layout.gapS <> Layout.crossCenter)
            [ viewDownloadButton mEntry
            , if hasNonImageFiles
                then viewFileIndicator m
                else M.text ""
            ]
        ]

-- | Download button for the current image (rendered as an <a> with download attribute).
viewDownloadButton :: Maybe (FileRef, Maybe Text) -> M.View m a
viewDownloadButton (Just (ref, Just url))
  | not (T.null url) =
      MH.a_
        [ MP.href_ (ms url)
        , M.textProp "download" (ms ref.fileName)
        , class_ "flex items-center gap-1 px-2 py-1 rounded text-sm text-muted-foreground hover:bg-stone-200 transition-colors"
        ]
        [Icon.iconS Icon.Small Icon.IcnExport]
viewDownloadButton _ = M.text ""

viewFileIndicator :: FileGalleryModel -> M.View m FileGalleryAction
viewFileIndicator m =
  let fileCount = length m.nonImageFiles
   in MH.div_
        [class_ "relative"]
        [ MH.button_
            [ class_ "flex items-center gap-1.5 px-2 py-1 rounded text-sm text-muted-foreground hover:bg-stone-200 transition-colors"
            , MH.onClick ToggleFilePopover
            ]
            [ M.text $ C.translate' (C.LblMoreFiles fileCount)
            , Icon.iconS Icon.Small Icon.IcnImport
            ]
        , if m.showFilePopover
            then viewFilePopover m.nonImageFiles
            else M.text ""
        ]

viewFilePopover :: [FileRef] -> M.View m a
viewFilePopover files =
  MH.div_
    [class_ "absolute bottom-full right-0 mb-1 w-64 bg-popover border border-border rounded-lg shadow-lg p-2 z-10"]
    (map viewPopoverFileItem files)

viewPopoverFileItem :: FileRef -> M.View m a
viewPopoverFileItem ref =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ MH.span_ [class_ "text-xs font-medium truncate flex-1"] [M.text $ ms ref.fileName]
    , MH.span_
        [class_ "text-xs text-muted-foreground flex-shrink-0"]
        [M.text $ ms $ showFileSize ref.fileSize]
    ]

-- ---------------------------------------------------------------------------
-- File List (non-image only)
-- ---------------------------------------------------------------------------

viewFileList :: [FileRef] -> M.View m a
viewFileList files =
  MH.div_
    [class_ "p-3"]
    [ Layout.vFlow
        Layout.gapS
        (map viewFileListItem files)
    ]

viewFileListItem :: FileRef -> M.View m a
viewFileListItem ref =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ Icon.iconS Icon.Small Icon.IcnImport
    , MH.span_ [class_ "text-sm font-medium truncate flex-1"] [M.text $ ms ref.fileName]
    , MH.span_
        [class_ "text-sm text-muted-foreground flex-shrink-0"]
        [M.text $ ms $ "(" <> showFileSize ref.fileSize <> ")"]
    ]
