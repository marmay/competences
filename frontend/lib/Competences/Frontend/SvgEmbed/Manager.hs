{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.SvgEmbed.Manager
-- Description : SVG embedding manager for RichContent components
--
-- Renders MathJax formulas to SVG and encodes them as data URLs for use
-- in @\<img\>@ elements. Browsers fully sandbox SVGs loaded through @\<img\>@:
-- no script execution, no event handlers, no external resource loading.
-- This eliminates the entire class of SVG XSS attacks by construction.
--
-- Raw SVG blocks from markdown are handled purely via 'svgToDataUrl' —
-- no IO needed.
module Competences.Frontend.SvgEmbed.Manager
  ( -- * Types
    SymbolId (..)
  , EmbeddedSymbol (..)
  , MathDisplay (..)

    -- * Formula cache
  , FormulaCache (..)
  , newFormulaCache

    -- * Formula rendering (MathJax)
  , renderFormula
  , renderFormulaCached
  , lookupCachedFormulas
  , hashLatex
  , hashLatexColored

    -- * Pure SVG encoding
  , svgToDataUrl

    -- * Utilities
  , isMathJaxReady
  )
where

import Data.Bits (xor, (.&.))
import Data.ByteString.Base64 qualified as Base64
import Data.Char (ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text.Encoding qualified as TE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Miso.DSL
  ( JSVal
  , Object (..)
  , create
  , fromJSVal
  , isNull
  , jsg
  , setProp
  , toJSVal
  , (!)
  , (#)
  )
import Miso.String (MisoString, fromMisoString, ms)
import Numeric (showHex)

-- | Unique ID for an embedded symbol (hash of source content)
newtype SymbolId = SymbolId {unSymbolId :: Text}
  deriving (Eq, Ord, Show)

-- | Embedded symbol with data URL and dimensions for proper display
data EmbeddedSymbol = EmbeddedSymbol
  { symbolId :: !SymbolId
  , dataUrl :: !Text
  -- ^ data:image/svg+xml;charset=utf-8,... (for <img> src)
  , width :: !Text
  -- ^ SVG width (e.g., "1.688ex")
  , height :: !Text
  -- ^ SVG height (e.g., "2.8ex")
  , verticalAlign :: !Text
  -- ^ CSS vertical-align (e.g., "-0.798ex")
  }
  deriving (Eq, Ord, Show)

-- | Display mode for math rendering
data MathDisplay
  = -- | Inline math (like $...$)
    Inline
  | -- | Block/display math (like $$...$$)
    Block
  deriving (Eq, Show)

-- | Hash LaTeX source to create a symbol ID
-- Uses a simple hash for fast, collision-resistant IDs
hashLatex :: MathDisplay -> Text -> SymbolId
hashLatex display latex =
  let displayPrefix = case display of
        Inline -> "i"
        Block -> "b"
      -- Simple DJB2-like hash (works on 32-bit)
      djb2Hash :: Text -> Int
      djb2Hash = T.foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381
      hashVal = djb2Hash latex
   in SymbolId $ "formula-" <> displayPrefix <> "-" <> T.pack (showHex hashVal "")

-- | Hash LaTeX source with optional color to create a symbol ID.
-- When a color is present, appends @"\\0" \<\> hex@ to the LaTeX before hashing
-- so that the same formula in different colors gets distinct cache entries.
hashLatexColored :: MathDisplay -> Text -> Maybe Text -> SymbolId
hashLatexColored display latex Nothing = hashLatex display latex
hashLatexColored display latex (Just hex) = hashLatex display (latex <> "\0" <> hex)

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

-- | Inject @color:#hex@ into the root @\<svg\>@ element's style attribute.
-- MathJax SVG uses @fill="currentColor"@ for glyph paths, which inherits
-- from the CSS @color@ property. This lets us colorize formulas without
-- touching the LaTeX source (avoiding TeX macro-parameter @#@ issues).
--
-- MathJax always emits a @style="…"@ on the root @\<svg\>@ (e.g. for
-- @vertical-align@), so we prepend @color:#hex;@ to the existing value.
-- Falls back to adding a new @style@ attribute if none is found.
injectSvgColor :: Maybe Text -> Text -> Text
injectSvgColor Nothing svg = svg
injectSvgColor (Just hex) svg =
  let needle = "style=\""
   in case T.breakOn needle svg of
        (_, after) | T.null after ->
          -- No existing style attribute; add one to the <svg> tag
          T.replace "<svg " ("<svg style=\"color:" <> hex <> "\" ") svg
        (before, after) ->
          -- Prepend color into existing style value
          before <> "style=\"color:" <> hex <> ";" <> T.drop (T.length needle) after

prepareFormula :: MathDisplay -> Text -> Text
prepareFormula Inline latex = "{" <> latex <> "}"
prepareFormula Block latex = latex

-- | Render a LaTeX formula to SVG via MathJax and return as a data URL.
-- An optional hex color is injected into the SVG root element.
--
-- The MathJax result is a detached DOM element that gets garbage collected —
-- nothing is inserted into the live DOM.
renderFormula :: MathDisplay -> Text -> Maybe Text -> IO (Maybe EmbeddedSymbol)
renderFormula display latex mColor = do
  ready <- isMathJaxReady
  if not ready
    then pure Nothing
    else do
      let sid = hashLatexColored display latex mColor
      -- Render with MathJax (returns a detached container element)
      mathJax <- jsg ("MathJax" :: MisoString)
      options <- create
      displayVal <- toJSVal (display == Block)
      setProp ("display" :: MisoString) displayVal options
      latexVal <- toJSVal (ms (prepareFormula display latex) :: MisoString)
      result <- mathJax # ("tex2svg" :: MisoString) $ [latexVal, unObject options]
      resultIsNull <- isNull result
      if resultIsNull
        then pure Nothing
        else do
          -- Query the <svg> from the result container
          svgElement <- result # ("querySelector" :: MisoString) $ [toJSVal ("svg" :: MisoString)]
          svgIsNull <- isNull svgElement
          if svgIsNull
            then pure Nothing
            else do
              -- Extract dimensions from the SVG element
              widthVal <- svgElement # ("getAttribute" :: MisoString) $ [toJSVal ("width" :: MisoString)]
              mWidth <- fromJSVal @MisoString widthVal
              heightVal <- svgElement # ("getAttribute" :: MisoString) $ [toJSVal ("height" :: MisoString)]
              mHeight <- fromJSVal @MisoString heightVal
              styleObj <- svgElement ! ("style" :: MisoString)
              vertAlignVal <- styleObj ! ("verticalAlign" :: MisoString)
              mVertAlign <- fromJSVal @MisoString vertAlignVal
              -- Serialize SVG to text via .outerHTML
              outerHtmlVal <- svgElement ! ("outerHTML" :: MisoString)
              mOuterHtml <- fromJSVal @MisoString outerHtmlVal
              case (mWidth, mHeight, mOuterHtml) of
                (Just w, Just h, Just svgHtml) ->
                  let svgText = injectSvgColor mColor (fromMisoString svgHtml)
                   in pure $ Just EmbeddedSymbol
                        { symbolId = sid
                        , dataUrl = svgToDataUrl svgText
                        , width = fromMisoString w
                        , height = fromMisoString h
                        , verticalAlign = maybe "0" fromMisoString mVertAlign
                        }
                _ -> pure Nothing

-- | Encode SVG text as a base64 data URL. Pure function, no IO.
--
-- Uses base64 encoding for maximum browser compatibility — Chrome does not
-- render @charset=utf-8@ SVG data URLs inside SVG @\<image\>@ elements.
svgToDataUrl :: Text -> Text
svgToDataUrl svg =
  "data:image/svg+xml;base64," <> TE.decodeLatin1 (Base64.encode (TE.encodeUtf8 svg))

-- ============================================================================
-- Formula cache
-- ============================================================================

-- | Explicit formula cache, held in 'SyncContext'.
newtype FormulaCache = FormulaCache (IORef (Map SymbolId EmbeddedSymbol))

-- | Create a new, empty formula cache.
newFormulaCache :: IO FormulaCache
newFormulaCache = FormulaCache <$> newIORef Map.empty

-- | Render a formula via MathJax, using the given cache.
-- On cache hit, returns immediately without calling MathJax.
renderFormulaCached :: FormulaCache -> MathDisplay -> Text -> Maybe Text -> IO (Maybe EmbeddedSymbol)
renderFormulaCached (FormulaCache ref) display latex mColor = do
  let sid = hashLatexColored display latex mColor
  cache <- readIORef ref
  case Map.lookup sid cache of
    Just es -> pure (Just es)
    Nothing -> do
      result <- renderFormula display latex mColor
      case result of
        Just es -> do
          atomicModifyIORef' ref $ \c -> (Map.insert sid es c, ())
          pure (Just es)
        Nothing -> pure Nothing

-- | Bulk cache lookup for a list of symbol IDs.
-- Returns all currently cached symbols without rendering anything.
lookupCachedFormulas :: FormulaCache -> [SymbolId] -> IO (Map SymbolId EmbeddedSymbol)
lookupCachedFormulas (FormulaCache ref) sids = do
  cache <- readIORef ref
  pure $ Map.fromList
    [(sid, es) | sid <- sids, Just es <- [Map.lookup sid cache]]
