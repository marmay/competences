-- | Reusable file gallery component.
--
-- Shows images in a navigable gallery with prev/next controls, click-to-enlarge
-- lightbox, and download overlays. Non-image files appear in a popover (when
-- mixed with images) or as a plain file list (when no images are present).
-- A toggle switches between gallery and table views. Empty file lists render nothing.
module Competences.Frontend.Component.FileGallery
  ( fileGalleryComponent
  , FileGalleryModel
  , FileGalleryAction
  , isImageMime
  )
where

import Competences.Document.FileRef (FileRef (..), SHA256Hash)
import Competences.Frontend.BinaryFFI (triggerDownload)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.FileCache (fileToDataUrl)
import Competences.Frontend.SyncContext (SyncContext (..), downloadFile)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.ByteString.Lazy qualified as BL
import Data.Set (Set)
import Data.Set qualified as Set
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

data ViewMode = GalleryView | TableView
  deriving (Eq, Show, Generic)

data FileGalleryModel = FileGalleryModel
  { imageFiles :: ![(FileRef, Maybe Text)]
  -- ^ Image files with their loaded data URL (Nothing = loading)
  , nonImageFiles :: ![FileRef]
  -- ^ Non-image files
  , currentImageIndex :: !Int
  , showFilePopover :: !Bool
  , enlarged :: !Bool
  -- ^ Lightbox open?
  , viewMode :: !ViewMode
  -- ^ Gallery vs table view
  , downloadingFiles :: !(Set SHA256Hash)
  -- ^ Files currently being downloaded (for spinner in table view)
  }
  deriving (Eq, Show, Generic)

data FileGalleryAction
  = LoadFiles
  | ImageLoaded !SHA256Hash !Text
  | ImageNotAvailable !SHA256Hash
  | PrevImage
  | NextImage
  | ToggleFilePopover
  | ToggleEnlarged
  | SetViewMode !ViewMode
  | DownloadNonImageFile !FileRef
  | DownloadComplete !SHA256Hash
  deriving (Eq, Show)

-- ===========================================================================
-- Component
-- ===========================================================================

-- | A self-contained gallery component for a list of file references.
--
-- Behaviour:
--   * Has images → navigable image gallery (or table view); non-images in popover.
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
        , enlarged = False
        , viewMode = GalleryView
        , downloadingFiles = Set.empty
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

    update ToggleEnlarged = M.modify $ \m ->
      m{enlarged = not m.enlarged}

    update (SetViewMode mode) = M.modify $ \m ->
      m{viewMode = mode}

    update (DownloadNonImageFile ref) = do
      M.modify $ \m -> m{downloadingFiles = Set.insert ref.hash m.downloadingFiles}
      M.io $ do
        mData <- downloadFile r ref.hash
        case mData of
          Just bs -> do
            triggerDownload (BL.toStrict bs) ref.mimeType ref.fileName
            pure $ DownloadComplete ref.hash
          Nothing ->
            pure $ DownloadComplete ref.hash

    update (DownloadComplete hash) = M.modify $ \m ->
      m{downloadingFiles = Set.delete hash m.downloadingFiles}

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
          MH.div_
            [class_ "flex flex-col w-full h-full"]
            [ case m.viewMode of
                GalleryView -> viewImageGallery m
                TableView -> viewFileTable m
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
        [class_ "bg-stone-50 rounded-t-lg flex flex-1 min-h-0 items-center justify-center p-2 relative group"]
        [ case mEntry of
            Nothing -> M.text ""
            Just (ref, Nothing) ->
              MH.div_
                [class_ "flex flex-col items-center gap-2 animate-pulse"]
                [MH.span_ [class_ "text-sm text-stone-500"] [M.text $ ms ref.fileName]]
            Just (_ref, Just "") ->
              MH.div_
                [class_ "text-red-500 text-sm"]
                [M.text "Datei nicht verfügbar"]
            Just (ref, Just url) ->
              MH.div_ [class_ "relative max-h-full max-w-full"]
                [ MH.img_
                    [ MP.src_ (ms url)
                    , MP.alt_ (ms ref.fileName)
                    , class_ "max-h-full max-w-full object-contain cursor-pointer"
                    , MH.onClick ToggleEnlarged
                    ]
                , MH.a_
                    [ MP.href_ (ms url)
                    , M.textProp "download" (ms ref.fileName)
                    , class_ "absolute bottom-2 right-2 p-1.5 rounded-md bg-black/50 text-white opacity-0 group-hover:opacity-100 transition-opacity hover:bg-black/70"
                    , MP.title_ "Herunterladen"
                    ]
                    [Icon.iconS Icon.Small Icon.IcnImport]
                ]
        , if m.enlarged
            then viewEnlargedModal mEntry
            else MH.span_ [] []
        ]

-- | Lightbox modal for enlarged image viewing.
viewEnlargedModal :: Maybe (FileRef, Maybe Text) -> M.View m FileGalleryAction
viewEnlargedModal (Just (ref, Just url))
  | not (T.null url) =
      MH.div_
        [ class_ "fixed inset-0 z-50 flex items-center justify-center bg-black/80 cursor-pointer"
        , MH.onClick ToggleEnlarged
        ]
        [ MH.img_
            [ MP.src_ (ms url)
            , MP.alt_ (ms ref.fileName)
            , class_ "max-w-[90vw] max-h-[90vh] object-contain rounded-lg shadow-2xl"
            ]
        , MH.a_
            [ MP.href_ (ms url)
            , M.textProp "download" (ms ref.fileName)
            , class_ "absolute top-4 right-4 flex items-center gap-2 px-3 py-2 rounded-lg bg-black/60 text-white hover:bg-black/80 transition-colors"
            , MP.title_ "Herunterladen"
            ]
            [ Icon.iconS Icon.Small Icon.IcnImport
            , MH.span_ [class_ "text-sm"] [M.text $ ms ref.fileName]
            ]
        ]
viewEnlargedModal _ = MH.span_ [] []

-- ---------------------------------------------------------------------------
-- Bottom Bar
-- ---------------------------------------------------------------------------

viewGalleryBottomBar :: FileGalleryModel -> M.View m FileGalleryAction
viewGalleryBottomBar m =
  let totalImages = length m.imageFiles
      hasNonImageFiles = not (null m.nonImageFiles)
      showNav = totalImages > 1 && m.viewMode == GalleryView
   in MH.div_
        [class_ "flex items-center px-3 py-1.5 bg-stone-100 rounded-b-lg border-t border-stone-200"]
        [ -- Left: spacer (balances right side for centering)
          MH.div_ [class_ "flex-1"] []
        , -- Center: navigation controls (gallery mode only)
          if showNav
            then
              SL.indexedNav
                (Just PrevImage)
                (m.currentImageIndex + 1)
                totalImages
                (Just NextImage)
            else MH.span_ [] []
        , -- Right: view toggle + file indicator
          MH.div_
            [class_ "flex-1 flex justify-end"]
            [ Layout.hFlow
                (Layout.gapS <> Layout.crossCenter)
                [ viewModeToggle m.viewMode
                , if hasNonImageFiles
                    then viewFileIndicator m
                    else M.text ""
                ]
            ]
        ]

-- | Toggle button to switch between gallery and table view.
viewModeToggle :: ViewMode -> M.View m FileGalleryAction
viewModeToggle current =
  let (targetMode, icon, title) = case current of
        GalleryView -> (TableView, Icon.IcnMenu, "Tabellenansicht")
        TableView -> (GalleryView, Icon.IcnEvidence, "Galerieansicht")
   in MH.button_
        [ class_ "p-1 rounded text-muted-foreground hover:bg-stone-200 transition-colors"
        , MH.onClick (SetViewMode targetMode)
        , MP.title_ title
        ]
        [Icon.iconS Icon.Small icon]

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
-- Table View (all files)
-- ---------------------------------------------------------------------------

viewFileTable :: FileGalleryModel -> M.View m FileGalleryAction
viewFileTable m =
  MH.div_
    [class_ "bg-stone-50 rounded-t-lg divide-y divide-stone-200 flex-1 min-h-0 overflow-y-auto"]
    ( map viewImageFileRow m.imageFiles
        ++ map (viewNonImageFileRow m.downloadingFiles) m.nonImageFiles
    )

-- | Row for an image file in table view — download via <a> since data URL is loaded.
viewImageFileRow :: (FileRef, Maybe Text) -> M.View m a
viewImageFileRow (ref, mUrl) =
  MH.div_
    [class_ "flex items-center gap-2 px-3 py-2"]
    [ Icon.iconS Icon.Small Icon.IcnEvidence
    , MH.span_ [class_ "text-sm font-medium truncate flex-1"] [M.text $ ms ref.fileName]
    , MH.span_
        [class_ "text-sm text-muted-foreground flex-shrink-0"]
        [M.text $ ms $ showFileSize ref.fileSize]
    , case mUrl of
        Just url | not (T.null url) ->
          MH.a_
            [ MP.href_ (ms url)
            , M.textProp "download" (ms ref.fileName)
            , class_ "p-1 rounded text-muted-foreground hover:bg-stone-200 transition-colors"
            , MP.title_ "Herunterladen"
            ]
            [Icon.iconS Icon.Small Icon.IcnImport]
        _ ->
          MH.span_ [class_ "p-1 text-stone-300"] [Icon.iconS Icon.Small Icon.IcnImport]
    ]

-- | Row for a non-image file — on-demand download via WebSocket + triggerDownload.
viewNonImageFileRow :: Set SHA256Hash -> FileRef -> M.View m FileGalleryAction
viewNonImageFileRow downloading ref =
  let isDownloading = Set.member ref.hash downloading
   in MH.div_
        [class_ "flex items-center gap-2 px-3 py-2"]
        [ Icon.iconS Icon.Small Icon.IcnImport
        , MH.span_ [class_ "text-sm font-medium truncate flex-1"] [M.text $ ms ref.fileName]
        , MH.span_
            [class_ "text-sm text-muted-foreground flex-shrink-0"]
            [M.text $ ms $ showFileSize ref.fileSize]
        , if isDownloading
            then
              MH.span_
                [class_ "p-1 text-stone-400 animate-spin"]
                [Icon.iconS Icon.Small Icon.IcnProgress]
            else
              MH.button_
                [ class_ "p-1 rounded text-muted-foreground hover:bg-stone-200 transition-colors"
                , MH.onClick (DownloadNonImageFile ref)
                , MP.title_ "Herunterladen"
                ]
                [Icon.iconS Icon.Small Icon.IcnImport]
        ]

-- ---------------------------------------------------------------------------
-- File List (non-image only, no toggle)
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
