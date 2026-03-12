{-# LANGUAGE CPP #-}
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
  , FormulaResult (..)
  , formulaResultId

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

import Competences.Frontend.Logging (logWarn)
import Control.Exception (SomeException, displayException, try)
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
  , fromJSVal
  , isNull
  , jsg
  , toJSVal
  , (!)
  , (#)
  )
import Miso.String (MisoString, fromMisoString, ms)
import Numeric (showHex)

#ifdef WASM
foreign import javascript safe
  "await MathJax.tex2svgPromise($1, {display: $2})"
  js_tex2svgPromise :: MisoString -> Bool -> IO JSVal
#else
js_tex2svgPromise :: MisoString -> Bool -> IO JSVal
js_tex2svgPromise _ _ = error "SvgEmbed.Manager: tex2svgPromise not available outside WASM"
#endif

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

-- | Result of rendering a LaTeX formula via MathJax.
--
-- Three states with clear caching/retry semantics:
--
-- * 'FormulaPending' — transient failure (MathJax not loaded, FFI exception).
--   NOT cached, triggers retry with exponential backoff.
-- * 'FormulaSuccess' — rendered SVG. Cached permanently.
-- * 'FormulaError' — permanent failure (TeX error, malformed output).
--   Cached permanently — the source needs fixing, retrying won't help.
data FormulaResult
  = -- | Transient: MathJax not ready or FFI exception, will retry
    FormulaPending !SymbolId !Text
  | -- | Permanent: successfully rendered SVG with dimensions
    FormulaSuccess !EmbeddedSymbol
  | -- | Permanent: TeX error or malformed MathJax output
    FormulaError !SymbolId !Text
  deriving (Eq, Ord, Show)

-- | Extract the symbol ID from a formula result.
formulaResultId :: FormulaResult -> SymbolId
formulaResultId (FormulaPending sid _) = sid
formulaResultId (FormulaSuccess es) = es.symbolId
formulaResultId (FormulaError sid _) = sid

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
      mTex2svgPromise <- mathJax ! ("tex2svgPromise" :: MisoString) >>= fromJSVal @JSVal
      pure $ case mTex2svgPromise of
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
-- Always returns a 'FormulaResult':
--
-- * 'FormulaPending' for transient failures (MathJax not loaded, FFI exception)
-- * 'FormulaSuccess' on successful render
-- * 'FormulaError' for permanent failures (TeX error, malformed output)
--
-- The MathJax result is a detached DOM element that gets garbage collected —
-- nothing is inserted into the live DOM.
renderFormula :: MathDisplay -> Text -> Maybe Text -> IO FormulaResult
renderFormula display latex mColor = do
  let sid = hashLatexColored display latex mColor
  ready <- isMathJaxReady
  if not ready
    then pure $ FormulaPending sid "MathJax loading"
    else do
      -- MathJax is ready — failures from here are permanent (TeX error,
      -- missing SVG, malformed output). Only FFI exceptions are transient.
      result <- try @SomeException $ do
        -- Render with MathJax (returns a detached container element).
        -- Uses tex2svgPromise via FFI to properly await dynamic font loading.
        mjResult <- js_tex2svgPromise (ms (prepareFormula display latex)) (display == Block)
        resultIsNull <- isNull mjResult
        if resultIsNull
          then pure $ FormulaError sid "MathJax returned null"
          else do
            -- Check for MathJax TeX error before extracting SVG.
            -- MathJax puts data-mjx-error on a child <span>, not the container.
            errElement <- mjResult # ("querySelector" :: MisoString) $ [toJSVal ("[data-mjx-error]" :: MisoString)]
            errIsNull <- isNull errElement
            if not errIsNull
              then do
                errAttrVal <- errElement # ("getAttribute" :: MisoString) $ [toJSVal ("data-mjx-error" :: MisoString)]
                mErrAttr <- fromJSVal @MisoString errAttrVal
                pure $ FormulaError sid (maybe "Unknown TeX error" fromMisoString mErrAttr)
              else do
                -- Query the <svg> from the result container
                svgElement <- mjResult # ("querySelector" :: MisoString) $ [toJSVal ("svg" :: MisoString)]
                svgIsNull <- isNull svgElement
                if svgIsNull
                  then pure $ FormulaError sid "MathJax produced no SVG element"
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
                         in pure $ FormulaSuccess EmbeddedSymbol
                              { symbolId = sid
                              , dataUrl = svgToDataUrl svgText
                              , width = fromMisoString w
                              , height = fromMisoString h
                              , verticalAlign = maybe "0" fromMisoString mVertAlign
                              }
                      _ -> pure $ FormulaError sid "Could not extract SVG dimensions"
      case result of
        Right fr -> pure fr
        Left e -> do
          logWarn $ ms ("MathJax renderFormula exception for: " <> latex <> " — " <> T.pack (displayException e))
          pure $ FormulaPending sid (T.pack (displayException e))

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
-- Only stores permanent results ('FormulaSuccess', 'FormulaError').
-- 'FormulaPending' is never cached — it triggers retries.
newtype FormulaCache = FormulaCache (IORef (Map SymbolId FormulaResult))

-- | Create a new, empty formula cache.
newFormulaCache :: IO FormulaCache
newFormulaCache = FormulaCache <$> newIORef Map.empty

-- | Render a formula via MathJax, using the given cache.
-- On cache hit, returns immediately without calling MathJax.
-- 'FormulaSuccess' and 'FormulaError' are cached permanently;
-- 'FormulaPending' bypasses the cache and will be retried.
renderFormulaCached :: FormulaCache -> MathDisplay -> Text -> Maybe Text -> IO FormulaResult
renderFormulaCached (FormulaCache ref) display latex mColor = do
  let sid = hashLatexColored display latex mColor
  cache <- readIORef ref
  case Map.lookup sid cache of
    Just fr -> pure fr
    Nothing -> do
      fr <- renderFormula display latex mColor
      case fr of
        FormulaPending {} -> pure fr
        _ -> do
          atomicModifyIORef' ref $ \c -> (Map.insert sid fr c, ())
          pure fr

-- | Bulk cache lookup for a list of symbol IDs.
-- Returns all currently cached results (only 'FormulaSuccess' and
-- 'FormulaError') without rendering anything.
lookupCachedFormulas :: FormulaCache -> [SymbolId] -> IO (Map SymbolId FormulaResult)
lookupCachedFormulas (FormulaCache ref) sids = do
  cache <- readIORef ref
  pure $ Map.fromList
    [(sid, fr) | sid <- sids, Just fr <- [Map.lookup sid cache]]
