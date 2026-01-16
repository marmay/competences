{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.MathJax
-- Description : FFI bindings for MathJax LaTeX rendering
--
-- This module provides FFI bindings to MathJax for rendering LaTeX
-- to SVG. The rendering is done synchronously via MathJax.tex2svg().
module Competences.Frontend.MathJax
  ( -- * Rendering functions
    renderLatex
  , MathDisplay (..)

    -- * Low-level FFI
  , isMathJaxReady
  )
where

import Data.Text (Text)
import Miso.DSL
  ( JSVal
  , Object (..)
  , create
  , fromJSVal
  , jsg
  , setProp
  , toJSVal
  , (!)
  , (#)
  )
import Miso.String (MisoString, ms)

-- | Display mode for math rendering
data MathDisplay
  = -- | Inline math (like $...$)
    Inline
  | -- | Block/display math (like $$...$$)
    Block
  deriving (Eq, Show)

-- | Check if MathJax is loaded and ready
isMathJaxReady :: IO Bool
isMathJaxReady = do
  mMathJax <- jsg ("window" :: MisoString) ! ("MathJax" :: MisoString) >>= fromJSVal @JSVal
  case mMathJax of
    Nothing -> pure False
    Just mathJax -> do
      mTex2svg <- mathJax ! ("tex2svg" :: MisoString) >>= fromJSVal @JSVal
      pure $ case mTex2svg of
        Nothing -> False
        Just _ -> True

-- | Render LaTeX to SVG HTML string
--
-- Returns the outerHTML of the SVG element produced by MathJax.
-- Returns Nothing if MathJax is not ready or rendering fails.
--
-- Example:
--
-- @
-- mSvg <- renderLatex Block "\\frac{1}{2}"
-- case mSvg of
--   Just svg -> -- svg contains "<mjx-container>...</mjx-container>"
--   Nothing -> -- MathJax not ready or error
-- @
renderLatex :: MathDisplay -> Text -> IO (Maybe MisoString)
renderLatex display latex = do
  ready <- isMathJaxReady
  if not ready
    then pure Nothing
    else do
      -- Get MathJax global
      mathJax <- jsg ("MathJax" :: MisoString)

      -- Create options object: {display: true/false}
      options <- create
      displayVal <- toJSVal (display == Block)
      setProp ("display" :: MisoString) displayVal options

      -- Convert latex to JSVal (convert Text to MisoString first)
      latexVal <- toJSVal (ms latex :: MisoString)

      -- Call MathJax.tex2svg(latex, options)
      -- This returns a DOM element (mjx-container)
      result <- mathJax # ("tex2svg" :: MisoString) $ [latexVal, unObject options]

      -- Get the outerHTML of the result
      result ! ("outerHTML" :: MisoString) >>= fromJSVal
