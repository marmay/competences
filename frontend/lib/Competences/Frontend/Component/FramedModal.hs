-- |
-- Module      : Competences.Frontend.Component.FramedModal
-- Description : Standard modal chrome wrapper for context-agnostic components
--
-- Provides 'openFramedModal' which wraps any component in standard modal chrome
-- (dialog box styling, header with title + close button). This makes modal
-- components context-agnostic: the same component renders as a page, modal,
-- or pinned dialog without modification — only the surrounding frame differs.
module Competences.Frontend.Component.FramedModal
  ( FramedModalConfig (..)
  , ModalWidth (..)
  , ModalHeight (..)
  , openFramedModal
  )
where

import Competences.Frontend.SyncContext.WindowManager
  ( WindowManagerRef
  , closeModal
  , openModal
  )
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Modal qualified as Modal
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

-- | Width of the framed modal dialog.
data ModalWidth
  = -- | Wide modal (~85vw, capped). For editors, importers.
    ModalWide
  | -- | Narrow modal (~max-w-lg). For dialogs, confirmations.
    ModalNarrow
  deriving (Eq, Show)

-- | Height of the framed modal dialog.
data ModalHeight
  = -- | Full height (~90vh), content scrolls inside.
    ModalFull
  | -- | Content-driven height, max ~90vh with scroll on overflow.
    ModalAuto
  deriving (Eq, Show)

-- | Configuration for a framed modal.
data FramedModalConfig = FramedModalConfig
  { title :: !MisoString
  , width :: !ModalWidth
  , height :: !ModalHeight
  }

-- ---------------------------------------------------------------------------
-- Internal model / action
-- ---------------------------------------------------------------------------

newtype FramedModel = FramedModel ()
  deriving (Eq, Generic)

data FramedAction = CloseFramedModal
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Open a modal with standard chrome around the given component.
-- The component renders as content inside a dialog box with a title bar
-- and close button provided by the frame.
openFramedModal
  :: (Eq m, Typeable m)
  => WindowManagerRef
  -> FramedModalConfig
  -> M.Component FramedModel m a
  -> IO ()
openFramedModal wmRef cfg innerComp =
  openModal wmRef (framedComponent wmRef cfg innerComp)

-- ---------------------------------------------------------------------------
-- Internal component
-- ---------------------------------------------------------------------------

framedComponent
  :: (Eq m, Typeable m)
  => WindowManagerRef
  -> FramedModalConfig
  -> M.Component FramedModel m a
  -> M.Component p FramedModel FramedAction
framedComponent wmRef cfg innerComp =
  M.component model update view
  where
    model = FramedModel ()

    update CloseFramedModal =
      M.io_ $ closeModal wmRef

    view _m =
      Layout.addClass (dialogExtraClasses cfg) $
        Layout.vFlow mempty
          [ Layout.shrink0 $ Modal.modalHeader cfg.title CloseFramedModal
          , Layout.addClass (contentClasses cfg) $
              MH.div_ [] ["framed-modal-content" M.+> innerComp]
          ]

-- | Non-flex CSS classes for the outer dialog box (appearance + sizing).
dialogExtraClasses :: FramedModalConfig -> MisoString
dialogExtraClasses cfg =
  "bg-popover text-popover-foreground rounded-xl shadow-lg "
    <> widthClass cfg.width
    <> " "
    <> heightClass cfg.height

-- | CSS classes for the scrollable content area.
contentClasses :: FramedModalConfig -> MisoString
contentClasses cfg = case cfg.height of
  ModalFull -> "flex-1 min-h-0 overflow-y-auto"
  ModalAuto -> "overflow-y-auto"

widthClass :: ModalWidth -> MisoString
widthClass ModalWide = "w-[85vw] max-w-[1200px]"
widthClass ModalNarrow = "max-w-lg w-full mx-4"

heightClass :: ModalHeight -> MisoString
heightClass ModalFull = "h-[90vh]"
heightClass ModalAuto = "max-h-[90vh]"
