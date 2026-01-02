{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Competences.Frontend.View.Typography
Description: Basecoat-inspired typography components

This module provides typography components with consistent styling following Basecoat design patterns.
-}
module Competences.Frontend.View.Typography
  ( -- * Headings
    h1
  , h2
  , h3
  , h4

    -- * Text elements
  , paragraph
  , lead
  , small
  , muted
  , code
  , kbd

    -- * Text utilities
  , bold
  , italic
  , underline
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)

-- ============================================================================
-- HEADINGS
-- ============================================================================

-- | Heading level 1 - Largest heading
h1 :: MisoString -> M.View model action
h1 text =
  M.h1_
    [class_ "text-3xl font-bold tracking-tight text-foreground"]
    [M.text text]

-- | Heading level 2
h2 :: MisoString -> M.View model action
h2 text =
  M.h2_
    [class_ "text-2xl font-semibold tracking-tight text-foreground"]
    [M.text text]

-- | Heading level 3
h3 :: MisoString -> M.View model action
h3 text =
  M.h3_
    [class_ "text-xl font-semibold tracking-tight text-foreground"]
    [M.text text]

-- | Heading level 4
h4 :: MisoString -> M.View model action
h4 text =
  M.h4_
    [class_ "text-lg font-semibold tracking-tight text-foreground"]
    [M.text text]

-- ============================================================================
-- TEXT ELEMENTS
-- ============================================================================

-- | Paragraph text
paragraph :: MisoString -> M.View model action
paragraph text =
  M.p_
    [class_ "text-base text-foreground leading-7"]
    [M.text text]

-- | Lead paragraph - larger text for introductions
lead :: MisoString -> M.View model action
lead text =
  M.p_
    [class_ "text-lg text-muted-foreground leading-relaxed"]
    [M.text text]

-- | Small text
small :: MisoString -> M.View model action
small text =
  M.span_
    [class_ "text-sm text-foreground"]
    [M.text text]

-- | Muted text - for secondary information
muted :: MisoString -> M.View model action
muted text =
  M.span_
    [class_ "text-sm text-muted-foreground"]
    [M.text text]

-- | Inline code
code :: MisoString -> M.View model action
code text =
  M.code_
    [class_ "relative rounded bg-muted px-1.5 py-0.5 font-mono text-sm text-foreground"]
    [M.text text]

-- | Keyboard shortcut
kbd :: MisoString -> M.View model action
kbd text =
  M.kbd_
    [class_ "bg-muted text-muted-foreground inline-flex h-5 w-fit items-center rounded-sm px-1 text-xs font-medium select-none border border-border"]
    [M.text text]

-- ============================================================================
-- TEXT UTILITIES
-- ============================================================================

-- | Bold text
bold :: MisoString -> M.View model action
bold text =
  M.strong_
    [class_ "font-semibold"]
    [M.text text]

-- | Italic text
italic :: MisoString -> M.View model action
italic text =
  M.em_
    [class_ "italic"]
    [M.text text]

-- | Underlined text
underline :: MisoString -> M.View model action
underline text =
  M.span_
    [class_ "underline underline-offset-4"]
    [M.text text]
