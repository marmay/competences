{- |
Module: Competences.Frontend.View.Modal
Description: Basecoat-inspired modal/dialog styling helpers

Note: The primary modal/pin system is now handled by the WindowManager and WindowHost
components in Competences.Frontend.SyncContext.WindowManager and
Competences.Frontend.Component.WindowHost.

This module provides optional styling helpers for modal content.
-}
module Competences.Frontend.View.Modal
  ( modalDialog
  , modalHeader
  , modalHeaderWith
  , modalFooter
  )
where

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Miso qualified as M
import Miso.Html qualified as M

-- | Modal dialog - styling helper for modal content with shadow and rounded corners
-- This is an optional styling wrapper; modal components can also style themselves directly.
modalDialog :: [M.Attribute a] -> [M.View m a] -> M.View m a
modalDialog attrs =
  M.div_
    ( class_
        "bg-popover text-popover-foreground rounded-xl shadow-lg max-w-96 w-full mx-4"
        : attrs
    )

-- | Modal header with title on the left and a close button on the right.
modalHeader :: M.MisoString -> a -> M.View m a
modalHeader title closeAction =
  modalHeaderWith title [] closeAction

-- | Modal header with title, extra content (e.g. mode switcher), and close button.
modalHeaderWith :: M.MisoString -> [M.View m a] -> a -> M.View m a
modalHeaderWith title extraContent closeAction =
  M.div_
    [class_ "flex items-center justify-between border-b border-border px-6 py-4"]
    [ Typography.h3 title
    , case extraContent of
        [] -> closeButton closeAction
        xs ->
          M.div_
            [class_ "flex items-center gap-4"]
            (xs <> [closeButton closeAction])
    ]

-- | Modal footer with right-aligned action buttons.
modalFooter :: [M.View m a] -> M.View m a
modalFooter buttons =
  M.div_
    [class_ "flex justify-end gap-2 border-t border-border px-6 py-4"]
    buttons

-- | Close button used in modal headers
closeButton :: a -> M.View m a
closeButton closeAction =
  Button.ghost (Button.button Icon.IcnCancel (Just closeAction))
