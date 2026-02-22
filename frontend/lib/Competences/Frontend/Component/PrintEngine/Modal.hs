module Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalModel (..)
  , PrintModalAction (..)
  , defaultPrintModalModel
  , updatePrintModal
  , printModalView
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.CSS (printStyleView)
import Competences.Frontend.Component.PrintEngine.Types
  ( PageSize (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , pageSizeMm
  , pageMarginMm
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Function ((&))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as M
import Miso.String (MisoString, ms)

-- | Modal state
data PrintModalModel = PrintModalModel
  { settings :: !PrintSettings
  , previewTaskIndex :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Modal actions
data PrintModalAction
  = SetPageSize !PageSize
  | PreviewNext
  | PreviewPrev
  | ConfirmPrint
  | CancelPrint
  deriving (Eq, Show)

defaultPrintModalModel :: PrintModalModel
defaultPrintModalModel = PrintModalModel
  { settings = defaultPrintSettings
  , previewTaskIndex = 0
  }

-- | Pure update for the modal model
updatePrintModal :: PrintModalAction -> Int -> PrintModalModel -> PrintModalModel
updatePrintModal (SetPageSize ps) _total m =
  m {settings = PrintSettings {pageSize = ps}}
updatePrintModal PreviewNext total m =
  m {previewTaskIndex = min (total - 1) (m.previewTaskIndex + 1)}
updatePrintModal PreviewPrev _total m =
  m {previewTaskIndex = max 0 (m.previewTaskIndex - 1)}
updatePrintModal ConfirmPrint _total m = m
updatePrintModal CancelPrint _total m = m

-- | Construct a ButtonConfig without going through ToAction
-- (avoids overlapping instances when action is polymorphic)
btn :: Button.ToButtonContents c => c -> Maybe action -> Button.ButtonConfig action
btn c a = Button.ButtonConfig
  { Button.contents = Button.toButtonContents c
  , Button.action = a
  }

-- | Render the print preview modal.
printModalView
  :: (Int -> M.View model action)
  -> Int
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
printModalView renderTask totalTasks model wrap =
  -- Backdrop
  M.div_
    [class_ "fixed inset-0 z-50 flex items-center justify-center bg-black/50"]
    [ -- Modal container
      Layout.vFlow
        Layout.wFull
        [ modalHeader wrap
        , modalBody renderTask totalTasks model wrap
        , modalFooter wrap
        , printStyleView model.settings
        ]
        & Layout.addClass "bg-card text-card-foreground rounded-lg shadow-xl border border-border max-h-[90vh] w-[700px]"
    ]

-- | Modal header with title and close button
modalHeader :: (PrintModalAction -> action) -> M.View model action
modalHeader wrap =
  Layout.shrink0 $
    Layout.hFlow
      (Layout.crossCenter <> Layout.mainBetween)
      [ Typography.h4 (C.translate' C.LblPrintPreview)
      , Button.ghostSm (btn Icon.IcnCancel (Just (wrap CancelPrint)))
      ]
      & Layout.addClass "px-6 py-4 border-b border-border"

-- | Modal body: sidebar with page-size selector + preview pane
modalBody
  :: (Int -> M.View model action)
  -> Int
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
modalBody renderTask totalTasks model wrap =
  Layout.hFlow
    Layout.hFull
    [ -- Left sidebar: page size toggle group
      Layout.shrink0 $
        Layout.vFlow
          Layout.gapM
          [ Typography.fieldLabel (C.translate' C.LblPageSize)
          , pageSizeSelector model.settings.pageSize wrap
          ]
          & Layout.addClass "w-40 border-r border-border p-4"
    , -- Right: preview pane with task navigation
      Layout.grow $
        Layout.vFlow
          (Layout.gapM <> Layout.crossCenter <> Layout.mainCenter)
          [ previewPane renderTask model
          , previewNavigation totalTasks model wrap
          ]
          & Layout.addClass "p-6 bg-muted/30 overflow-hidden"
    ]
    & Layout.addClass "flex-1 min-h-0 overflow-hidden"

-- | Modal footer with cancel and print buttons
modalFooter :: (PrintModalAction -> action) -> M.View model action
modalFooter wrap =
  Layout.shrink0 $
    Layout.actionFooter
      [ Button.secondary (btn (Icon.IcnCancel, C.LblCancel) (Just (wrap CancelPrint)))
      , Button.primary (btn (Icon.IcnPrint, C.LblPrint) (Just (wrap ConfirmPrint)))
      ]

-- | Page size selector using button group (same pattern as EnumSelector ButtonsCompact)
pageSizeSelector :: PageSize -> (PrintModalAction -> action) -> M.View model action
pageSizeSelector current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == size) (btn (pageSizeLabel size) (Just (wrap (SetPageSize size))))
    | size <- [A5Portrait, A4Portrait]
    ]

pageSizeLabel :: PageSize -> MisoString
pageSizeLabel A5Portrait = "A5"
pageSizeLabel A4Portrait = "A4"

-- | Preview navigation: previous / "1 / N" / next
previewNavigation :: Int -> PrintModalModel -> (PrintModalAction -> action) -> M.View model action
previewNavigation totalTasks model wrap =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ Button.ghostSm (btn ("\x2039" :: MisoString) prevAction)
    , Typography.muted $
        ms (show (model.previewTaskIndex + 1)) <> " / " <> ms (show totalTasks)
    , Button.ghostSm (btn ("\x203A" :: MisoString) nextAction)
    ]
  where
    prevAction
      | model.previewTaskIndex <= 0 = Nothing
      | otherwise = Just (wrap PreviewPrev)
    nextAction
      | model.previewTaskIndex >= totalTasks - 1 = Nothing
      | otherwise = Just (wrap PreviewNext)

-- | Preview pane: renders one task inside a scaled page representation
previewPane :: (Int -> M.View model action) -> PrintModalModel -> M.View model action
previewPane renderTask model =
  let (wMm, hMm) = pageSizeMm model.settings.pageSize
      margin = pageMarginMm model.settings.pageSize
      -- Convert mm to px at 96 DPI (1 inch = 25.4mm)
      mmToPx mm = mm * 96.0 / 25.4
      pageWPx = mmToPx wMm
      pageHPx = mmToPx hMm
      marginPx = mmToPx margin
      -- Scale to fit available preview width (~440px considering modal padding)
      previewMaxW = 440.0 :: Double
      scaleFactor = previewMaxW / pageWPx
      scaledW = pageWPx * scaleFactor
      scaledH = pageHPx * scaleFactor
   in M.div_
        [ MC.style_
            [ ("width", ms (showPx scaledW))
            , ("height", ms (showPx scaledH))
            , ("overflow", "hidden")
            ]
        , class_ "rounded shadow-md"
        ]
        [ M.div_
            [ MC.style_
                [ ("width", ms (showPx pageWPx))
                , ("height", ms (showPx pageHPx))
                , ("padding", ms (showPx marginPx))
                , ("transform", ms $ "scale(" <> T.pack (show scaleFactor) <> ")")
                , ("transform-origin", "top left")
                ]
            , class_ "bg-white text-black"
            ]
            [ M.div_
                [class_ "prose prose-stone prose-sm max-w-none"]
                [renderTask model.previewTaskIndex]
            ]
        ]

showPx :: Double -> T.Text
showPx d = T.pack (show (round d :: Int)) <> "px"
