-- | Self-contained Miso component for previewing submissions.
--
-- Architecture:
--   submissionPreviewPanel (mounts container)
--   └─ Container component (holds selectedId)
--      ├─ CustomSelect component (derives options, pushes selectedId via binding)
--      └─ Detail component (keyed by selectedId, loads files on init)
--
-- When the user picks a different submission in the CustomSelect, the container's
-- selectedId changes via binding, which changes the detail component's key,
-- causing Miso to remount it → fresh file loading.
module Competences.Frontend.Component.SubmissionPreview
  ( submissionPreviewPanel
  , SubmissionPreviewModel
  , SubmissionPreviewAction
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (AssignmentId, Document (..), User (..))
import Competences.Document.FileRef (FileRef (..), SHA256Hash)
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionKind (..))
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.Component.Selector.Common (selectorTransformedLens)
import Competences.Frontend.Component.Selector.CustomSelect
  ( CustomSelectConfig (..)
  , customSelectComponent
  )
import Competences.Frontend.FileCache (fileToDataUrl)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , downloadFile
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)
import Optics.Core ((.~), (&))

-- | Check if a MIME type is an image type.
isImageMime :: Text -> Bool
isImageMime mime =
  any (`T.isPrefixOf` mime)
    ["image/jpeg", "image/png", "image/gif", "image/webp", "image/svg"]

-- ===========================================================================
-- Shared view helpers
-- ===========================================================================

kindToBadge :: SubmissionKind -> M.View m a
kindToBadge (DigitalSubmission _) = Badge.primary (Badge.badgeLabel C.LblAbgegeben)
kindToBadge (NonDigitalSubmission _) = Badge.secondary (Badge.badgeLabel C.LblGemacht)
kindToBadge (VoidSubmission _) = Badge.destructive (Badge.badgeLabel C.LblNichtGemacht)

-- | Compact view for the custom select trigger: badge + date/time.
compactSubmission :: Submission -> M.View m a
compactSubmission sub =
  MH.span_
    [class_ "flex items-center gap-2"]
    [ kindToBadge sub.kind
    , MH.span_ [class_ "text-sm"] [M.text $ C.formatDateTime sub.submittedAt]
    ]

-- | Detailed view for the custom select dropdown item.
detailedSubmission :: Submission -> M.View m a
detailedSubmission sub =
  MH.span_
    [class_ "flex items-center gap-2"]
    [ kindToBadge sub.kind
    , MH.span_ [] [M.text $ C.formatDateTime sub.submittedAt]
    , submissionExtra sub
    ]

-- | Extra info column in dropdown items (file count, location, reason).
submissionExtra :: Submission -> M.View m a
submissionExtra sub = case sub.kind of
  DigitalSubmission files ->
    let n = length files
     in MH.span_
          [class_ "text-muted-foreground"]
          [M.text $ "(" <> ms (show n) <> " " <> C.translate' (if n == 1 then C.LblFile else C.LblFiles) <> ")"]
  NonDigitalSubmission mLoc ->
    case mLoc of
      Nothing -> M.text ""
      Just loc -> MH.span_ [class_ "text-muted-foreground truncate max-w-32"] [M.text $ ms loc]
  VoidSubmission reason ->
    MH.span_ [class_ "text-muted-foreground truncate max-w-32 italic"] [M.text $ ms reason]

-- ===========================================================================
-- Container component (holds selectedId, mounts children)
-- ===========================================================================

data SubmissionPreviewModel = SubmissionPreviewModel
  { selectedId :: !(Maybe SubmissionId)
  }
  deriving (Eq, Show, Generic)

data SubmissionPreviewAction = SPNoOp
  deriving (Eq, Show)

submissionPreviewComponent
  :: SyncContext -> AssignmentId -> UserId
  -> M.Component p SubmissionPreviewModel SubmissionPreviewAction
submissionPreviewComponent r aId uId =
  M.component model update view'
  where
    model = SubmissionPreviewModel {selectedId = Nothing}

    update SPNoOp = pure ()

    selectConfig =
      CustomSelectConfig
        { deriveOptions = \doc ->
            sortOn (Down . (.submittedAt)) $
              Ix.toList $
                doc.submissions Ix.@= aId Ix.@= uId
        , itemKey = (.id)
        , compact = compactSubmission
        , detailed = detailedSubmission
        }

    selectBinding = selectorTransformedLens (.id) id #selectedId

    view' m =
      Layout.vFlow
        Layout.gapM
        [ -- Header: title + custom select
          MH.div_
            [class_ "flex items-center justify-between gap-3"]
            [ Typography.h4 (C.translate' C.LblSubmissions)
            , inlineComponent
                "sub-select"
                (customSelectComponent r selectConfig selectBinding)
            ]
        , -- Preview: keyed by selectedId so it remounts on selection change
          case m.selectedId of
            Nothing ->
              MH.div_
                [class_ "flex items-center justify-center p-8 text-muted-foreground text-sm"]
                [M.text $ C.translate' C.LblNoSubmissionSelected]
            Just sid ->
              inlineComponent
                ("sub-preview-" <> ms (show sid))
                (submissionDetailComponent r sid)
        ]

-- ===========================================================================
-- Detail component (preview for a single submission)
-- ===========================================================================

data DetailProjection = DetailProjection
  { projSubmission :: !(Maybe Submission)
  }
  deriving (Eq, Show, Generic)

detailProjection :: SubmissionId -> Document -> Maybe User -> DetailProjection
detailProjection sid doc _mUser =
  DetailProjection {projSubmission = Ix.getOne (doc.submissions Ix.@= sid)}

data DetailModel = DetailModel
  { submission :: !(Maybe Submission)
  , imageFiles :: ![(FileRef, Maybe Text)]
  , nonImageFiles :: ![FileRef]
  , currentImageIndex :: !Int
  , showFilePopover :: !Bool
  }
  deriving (Eq, Show, Generic)

data DetailAction
  = DetailProjectionChanged !(ProjectedChange DetailProjection)
  | ImageLoaded !SHA256Hash !Text
  | ImageNotAvailable !SHA256Hash
  | PrevImage
  | NextImage
  | ToggleFilePopover
  deriving (Eq, Show)

submissionDetailComponent
  :: SyncContext -> SubmissionId
  -> M.Component p DetailModel DetailAction
submissionDetailComponent r sid =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (detailProjection sid) DetailProjectionChanged]
    }
  where
    model =
      DetailModel
        { submission = Nothing
        , imageFiles = []
        , nonImageFiles = []
        , currentImageIndex = 0
        , showFilePopover = False
        }

    update (DetailProjectionChanged pc) = do
      m <- M.get
      let newSub = pc.projection.projSubmission
      M.put (m & #submission .~ newSub :: DetailModel)
      -- On initial load (Nothing → Just), load files
      case (m.submission, newSub) of
        (Nothing, Just sub) -> loadFilesForSub sub
        _ -> pure ()

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

    loadFilesForSub (sub :: Submission) = case sub.kind of
      DigitalSubmission files ->
        let imgs = filter (isImageMime . (.mimeType)) files
            nonImgs = filter (not . isImageMime . (.mimeType)) files
         in do
              M.modify $ \m ->
                m
                  { imageFiles = map (\f -> (f, Nothing)) imgs
                  , nonImageFiles = nonImgs
                  , currentImageIndex = 0
                  }
              mapM_ initiateDownload imgs
      _ -> M.modify $ \m -> m{imageFiles = [], nonImageFiles = [], currentImageIndex = 0}

    initiateDownload ref = M.io $ do
      mData <- downloadFile r ref.hash
      case mData of
        Just bs -> pure $ ImageLoaded ref.hash (fileToDataUrl ref.mimeType bs)
        Nothing -> pure $ ImageNotAvailable ref.hash

    view' m = case m.submission of
      Nothing ->
        MH.div_
          [class_ "flex items-center justify-center p-8 text-muted-foreground text-sm"]
          [M.text $ C.translate' C.LblNoSubmissionSelected]
      Just sub -> viewSubmissionContent m sub

-- ---------------------------------------------------------------------------
-- Preview Views (used by DetailModel)
-- ---------------------------------------------------------------------------

viewSubmissionContent :: DetailModel -> Submission -> M.View m DetailAction
viewSubmissionContent m sub = case sub.kind of
  DigitalSubmission _files -> viewDigitalContent m
  NonDigitalSubmission mLoc ->
    MH.div_
      [class_ "p-4 text-sm"]
      [ Badge.secondary (Badge.badgeText (C.translate' C.LblGemacht))
      , case mLoc of
          Nothing -> M.text ""
          Just loc ->
            MH.div_
              [class_ "mt-2 text-muted-foreground"]
              [M.text $ ms loc]
      , viewRemark sub
      ]
  VoidSubmission reason ->
    MH.div_
      [class_ "p-4 text-sm"]
      [ Badge.outline (Badge.badgeText (C.translate' C.LblNichtGemacht))
      , MH.div_
          [class_ "mt-2 text-muted-foreground italic"]
          [M.text $ ms reason]
      , viewRemark sub
      ]

viewRemark :: Submission -> M.View m a
viewRemark sub = case sub.remark of
  Nothing -> M.text ""
  Just rmk ->
    MH.div_
      [class_ "mt-2 pt-2 border-t text-sm text-muted-foreground"]
      [ MH.span_ [class_ "font-medium"] [M.text $ C.translate' C.LblRemark <> ": "]
      , M.text (ms rmk)
      ]

-- ---------------------------------------------------------------------------
-- Digital Content: Image Gallery + File List
-- ---------------------------------------------------------------------------

viewDigitalContent :: DetailModel -> M.View m DetailAction
viewDigitalContent m
  | null m.imageFiles && null m.nonImageFiles =
      MH.div_
        [class_ "flex items-center justify-center p-8 text-muted-foreground text-sm"]
        [M.text $ C.translate' C.LblNoSubmissions]
  | null m.imageFiles =
      viewFileList m.nonImageFiles
  | otherwise =
      Layout.vFlow
        mempty
        [ viewImageGallery m
        , viewGalleryBottomBar m
        ]

viewImageGallery :: DetailModel -> M.View m DetailAction
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

viewGalleryBottomBar :: DetailModel -> M.View m DetailAction
viewGalleryBottomBar m =
  let totalImages = length m.imageFiles
      hasNonImageFiles = not (null m.nonImageFiles)
      showNav = totalImages > 1
   in MH.div_
        [class_ "flex items-center justify-between px-3 py-2 bg-stone-100 rounded-b-lg border-t border-stone-200"]
        [ if showNav
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
        , if hasNonImageFiles
            then viewFileIndicator m
            else M.text ""
        ]

viewFileIndicator :: DetailModel -> M.View m DetailAction
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

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Mount a submission preview panel as an inline component.
-- Includes title, custom select dropdown with badges, and preview area.
submissionPreviewPanel
  :: (Eq p) => SyncContext -> AssignmentId -> UserId
  -> M.View p a
submissionPreviewPanel r aId uId =
  MH.div_ []
    [ ("submission-preview-" <> ms (show aId) <> "-" <> ms (show uId))
        M.+> submissionPreviewComponent r aId uId
    ]
