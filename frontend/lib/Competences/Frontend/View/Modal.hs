{- |
Module: Competences.Frontend.View.Modal
Description: Basecoat-inspired modal/dialog components

This module provides modal and dialog components with consistent Basecoat styling.
-}
module Competences.Frontend.View.Modal
  ( modalHost
  , maybeModalHost
  , modalDialog
  )
where

import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M

-- | Modal host - full-screen overlay with backdrop
modalHost :: [M.Attribute a] -> [M.View m a] -> M.View m a
modalHost attrs =
  M.div_
    ( class_
        "fixed inset-0 z-50 flex items-center justify-center bg-stone-900/50"
        : attrs
    )

-- | Optional modal host for use with components
maybeModalHost :: (Eq child) => Maybe (M.Component model child action') -> M.View model action
maybeModalHost (Just c) =
  componentA
    "modal-host"
    [class_ "fixed inset-0 z-50 flex items-center justify-center bg-stone-900/50"]
    c
maybeModalHost Nothing = M.div_ [] []

-- | Modal dialog - the actual dialog box with shadow and rounded corners
modalDialog :: [M.Attribute a] -> [M.View m a] -> M.View m a
modalDialog attrs =
  M.div_
    ( class_
        "bg-white rounded-xl shadow-lg max-w-96 w-full mx-4"
        : attrs
    )
