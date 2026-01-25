{- |
Module: Competences.Frontend.View.Modal
Description: Basecoat-inspired modal/dialog styling helpers

Note: The primary modal system is now handled by the ModalManager and ModalHost
components in Competences.Frontend.SyncContext.ModalManager and
Competences.Frontend.Component.ModalHost.

This module provides optional styling helpers for modal content.
-}
module Competences.Frontend.View.Modal
  ( modalDialog
  )
where

import Competences.Frontend.View.Tailwind (class_)
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
