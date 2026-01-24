{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.MathJax.Manager
-- Description : MathJax SVG container management for RichContent components
--
-- This module provides FFI bindings for managing MathJax-rendered SVGs
-- in a hidden container outside of Miso's virtual DOM. Each RichContent
-- component gets its own sub-container, which is cleared on unmount.
--
-- The architecture:
-- - #mathjax-defs (hidden div in HTML, outside Miso's control)
--   - #mathjax-{componentId1} (per-component container)
--     - #formula-{hash1} (rendered SVG)
--     - #formula-{hash2} (rendered SVG)
--   - #mathjax-{componentId2} (another component's container)
--     - ...
--
-- Miso views reference these SVGs using <svg><use href="#formula-{hash}"/></svg>
module Competences.Frontend.MathJax.Manager
  ( -- * Types
    FormulaId (..)
  , RenderedFormula (..)
  , ComponentContainerId (..)
  , MathDisplay (..)

    -- * Container management
  , createComponentContainer
  , destroyComponentContainer
  , clearComponentContainer

    -- * Formula rendering
  , renderFormula
  , hashLatex

    -- * Utilities
  , isMathJaxReady
  )
where

import Data.Bits (xor, (.&.))
import Data.Char (ord)
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

-- | Unique ID for a rendered formula (hash of LaTeX source + display mode)
newtype FormulaId = FormulaId {unFormulaId :: Text}
  deriving (Eq, Ord, Show)

-- | Rendered formula with dimensions for proper display
data RenderedFormula = RenderedFormula
  { formulaId :: !FormulaId
  , width :: !Text
  -- ^ SVG width (e.g., "1.688ex")
  , height :: !Text
  -- ^ SVG height (e.g., "2.8ex")
  , viewBox :: !Text
  -- ^ SVG viewBox (e.g., "0 -884.7 746.2 1237.5")
  , verticalAlign :: !Text
  -- ^ CSS vertical-align (e.g., "-0.798ex")
  }
  deriving (Eq, Ord, Show)

-- | Unique ID for a component's SVG container
newtype ComponentContainerId = ComponentContainerId {unComponentContainerId :: Text}
  deriving (Eq, Ord, Show)

-- | Display mode for math rendering
data MathDisplay
  = -- | Inline math (like $...$)
    Inline
  | -- | Block/display math (like $$...$$)
    Block
  deriving (Eq, Show)

-- | Hash LaTeX source to create a formula ID
-- Uses a simple hash for fast, collision-resistant IDs
hashLatex :: MathDisplay -> Text -> FormulaId
hashLatex display latex =
  let displayPrefix = case display of
        Inline -> "i"
        Block -> "b"
      -- Simple DJB2-like hash (works on 32-bit)
      djb2Hash :: Text -> Int
      djb2Hash = T.foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381
      hashVal = djb2Hash latex
   in FormulaId $ "formula-" <> displayPrefix <> "-" <> T.pack (showHex hashVal "")

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

-- | Create a container SVG with defs for a component's symbol definitions
-- The container is created as a child of #mathjax-defs
-- Structure: <svg id="mathjax-{cid}" width="0" height="0"><defs></defs></svg>
createComponentContainer :: ComponentContainerId -> IO ()
createComponentContainer (ComponentContainerId cid) = do
  document <- jsg ("document" :: MisoString)
  -- Get the main mathjax-defs container
  parentContainer <- document # ("getElementById" :: MisoString) $ [toJSVal ("mathjax-defs" :: MisoString)]
  -- Create a new SVG element for this component (must use createElementNS for SVG)
  svgNs <- toJSVal ("http://www.w3.org/2000/svg" :: MisoString)
  svgTag <- toJSVal ("svg" :: MisoString)
  newSvg <- document # ("createElementNS" :: MisoString) $ [svgNs, svgTag]
  -- Set the id
  idVal <- toJSVal (ms ("mathjax-" <> cid) :: MisoString)
  setProp ("id" :: MisoString) idVal (Object newSvg)
  -- Set width and height to 0 (hidden)
  zeroVal <- toJSVal ("0" :: MisoString)
  widthAttr <- toJSVal ("width" :: MisoString)
  heightAttr <- toJSVal ("height" :: MisoString)
  _ <- newSvg # ("setAttribute" :: MisoString) $ [widthAttr, zeroVal]
  _ <- newSvg # ("setAttribute" :: MisoString) $ [heightAttr, zeroVal]
  -- Create a <defs> element inside the SVG
  defsTag <- toJSVal ("defs" :: MisoString)
  defsEl <- document # ("createElementNS" :: MisoString) $ [svgNs, defsTag]
  _ <- newSvg # ("appendChild" :: MisoString) $ [defsEl]
  -- Append SVG to parent
  _ <- parentContainer # ("appendChild" :: MisoString) $ [newSvg]
  pure ()

-- | Remove a component's container and all its SVGs
destroyComponentContainer :: ComponentContainerId -> IO ()
destroyComponentContainer (ComponentContainerId cid) = do
  document <- jsg ("document" :: MisoString)
  -- Get the component's container
  container <- document # ("getElementById" :: MisoString) $ [toJSVal (ms ("mathjax-" <> cid) :: MisoString)]
  mContainer <- fromJSVal @JSVal container
  case mContainer of
    Nothing -> pure () -- Container doesn't exist, nothing to do
    Just c -> do
      -- Remove the element (and all children)
      _ <- c # ("remove" :: MisoString) $ ([] :: [JSVal])
      pure ()

-- | Clear all SVGs from a component's container (but keep the container)
clearComponentContainer :: ComponentContainerId -> IO ()
clearComponentContainer (ComponentContainerId cid) = do
  document <- jsg ("document" :: MisoString)
  container <- document # ("getElementById" :: MisoString) $ [toJSVal (ms ("mathjax-" <> cid) :: MisoString)]
  mContainer <- fromJSVal @JSVal container
  case mContainer of
    Nothing -> pure ()
    Just c -> do
      -- Set innerHTML to empty string to remove all children
      emptyStr <- toJSVal ("" :: MisoString)
      setProp ("innerHTML" :: MisoString) emptyStr (Object c)

-- | Render a LaTeX formula to SVG and insert into component's container
-- Returns the RenderedFormula with dimensions for use in <use href="#..."/>
-- If the formula already exists in this container, returns its info without re-rendering
renderFormula :: ComponentContainerId -> MathDisplay -> Text -> IO (Maybe RenderedFormula)
renderFormula (ComponentContainerId cid) display latex = do
  consoleLog $ "[renderFormula] Starting for cid=" <> ms cid <> ", latex=" <> ms latex
  ready <- isMathJaxReady
  consoleLog $ "[renderFormula] MathJax ready=" <> ms (show ready)
  if not ready
    then pure Nothing
    else do
      let fid = hashLatex display latex
      consoleLog $ "[renderFormula] Formula ID=" <> ms fid.unFormulaId
      -- Check if formula already exists in this component's container
      document <- jsg ("document" :: MisoString)
      existing <- document # ("getElementById" :: MisoString) $ [toJSVal (ms fid.unFormulaId :: MisoString)]
      -- Note: getElementById returns null for non-existent elements, but fromJSVal @JSVal
      -- will return Just for null. We need to check if it's actually null.
      existingIsNull <- isNull existing
      if not existingIsNull
        then do
          consoleLog "[renderFormula] Formula already exists, extracting dimensions..."
          -- Extract dimensions from existing SVG
          extractFormulaDimensions fid existing
        else do
          consoleLog "[renderFormula] Rendering new formula..."
          -- Get MathJax and render
          mathJax <- jsg ("MathJax" :: MisoString)
          -- Create options object: {display: true/false}
          options <- create
          displayVal <- toJSVal (display == Block)
          setProp ("display" :: MisoString) displayVal options
          -- Convert latex to JSVal
          latexVal <- toJSVal (ms latex :: MisoString)
          -- Call MathJax.tex2svg(latex, options)
          consoleLog "[renderFormula] Calling MathJax.tex2svg..."
          result <- mathJax # ("tex2svg" :: MisoString) $ [latexVal, unObject options]
          -- The result is an mjx-container element containing an SVG
          -- We need to extract the SVG and set its id
          resultIsNull <- isNull result
          if resultIsNull
            then do
              consoleLog "[renderFormula] tex2svg returned null/undefined"
              pure Nothing
            else do
              let mjxContainer = result
              consoleLog "[renderFormula] Got mjx-container, extracting SVG..."
              -- Get the SVG element inside the container
              svgElement <- mjxContainer # ("querySelector" :: MisoString) $ [toJSVal ("svg" :: MisoString)]
              svgIsNull <- isNull svgElement
              if svgIsNull
                then do
                  consoleLog "[renderFormula] No SVG found in mjx-container"
                  pure Nothing
                else do
                  let svg = svgElement
                  consoleLog "[renderFormula] Found SVG, converting to symbol..."
                  -- Extract dimensions before converting
                  mDims <- extractFormulaDimensions fid svg
                  case mDims of
                    Nothing -> do
                      consoleLog "[renderFormula] Failed to extract dimensions"
                      pure Nothing
                    Just dims -> do
                      -- Create a <symbol> element (can be referenced by <use>)
                      -- Note: <use> cannot reference an entire <svg>, only <symbol>, <g>, etc.
                      symbol <- document # ("createElementNS" :: MisoString) $
                        [toJSVal ("http://www.w3.org/2000/svg" :: MisoString), toJSVal ("symbol" :: MisoString)]
                      -- Set the id on the symbol
                      fidVal <- toJSVal (ms fid.unFormulaId :: MisoString)
                      setProp ("id" :: MisoString) fidVal (Object symbol)
                      -- Copy viewBox from SVG to symbol
                      viewBoxAttr <- toJSVal ("viewBox" :: MisoString)
                      viewBoxVal <- toJSVal (ms dims.viewBox :: MisoString)
                      _ <- symbol # ("setAttribute" :: MisoString) $ [viewBoxAttr, viewBoxVal]
                      -- Store dimensions as data attributes (for later retrieval)
                      dataWidthAttr <- toJSVal ("data-width" :: MisoString)
                      widthVal <- toJSVal (ms dims.width :: MisoString)
                      _ <- symbol # ("setAttribute" :: MisoString) $ [dataWidthAttr, widthVal]
                      dataHeightAttr <- toJSVal ("data-height" :: MisoString)
                      heightVal <- toJSVal (ms dims.height :: MisoString)
                      _ <- symbol # ("setAttribute" :: MisoString) $ [dataHeightAttr, heightVal]
                      dataVertAlignAttr <- toJSVal ("data-vertical-align" :: MisoString)
                      vertAlignVal <- toJSVal (ms dims.verticalAlign :: MisoString)
                      _ <- symbol # ("setAttribute" :: MisoString) $ [dataVertAlignAttr, vertAlignVal]
                      -- Move the SVG's children to the symbol
                      let moveChildren = do
                            firstChild <- svg ! ("firstChild" :: MisoString)
                            childIsNull <- isNull firstChild
                            if childIsNull
                              then pure ()
                              else do
                                _ <- symbol # ("appendChild" :: MisoString) $ [firstChild]
                                moveChildren
                      moveChildren
                      -- Get the component's container SVG and its defs element
                      let containerId = "mathjax-" <> cid
                      consoleLog $ "[renderFormula] Looking for container: " <> ms containerId
                      container <- document # ("getElementById" :: MisoString) $ [toJSVal (ms containerId :: MisoString)]
                      containerIsNull <- isNull container
                      if containerIsNull
                        then do
                          consoleLog "[renderFormula] Container not found!"
                          pure Nothing
                        else do
                          -- Get the <defs> element inside the container SVG
                          defsEl <- container # ("querySelector" :: MisoString) $ [toJSVal ("defs" :: MisoString)]
                          defsIsNull <- isNull defsEl
                          if defsIsNull
                            then do
                              consoleLog "[renderFormula] Defs element not found in container!"
                              pure Nothing
                            else do
                              -- Append the symbol to defs
                              _ <- defsEl # ("appendChild" :: MisoString) $ [symbol]
                              consoleLog "[renderFormula] Symbol appended to defs successfully"
                              pure (Just dims)

-- | Extract dimensions from an SVG or symbol element
-- For SVG elements (freshly rendered), reads width/height/viewBox attributes and style.verticalAlign
-- For symbol elements (cached), reads data-width/data-height/data-vertical-align and viewBox
extractFormulaDimensions :: FormulaId -> JSVal -> IO (Maybe RenderedFormula)
extractFormulaDimensions fid element = do
  -- Try to get width from attribute (SVG) or data attribute (symbol)
  widthVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("width" :: MisoString)]
  mWidth <- fromJSVal @MisoString widthVal
  mWidth' <- case mWidth of
    Just w -> pure (Just w)
    Nothing -> do
      -- Try data attribute (for symbols)
      dataWidthVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("data-width" :: MisoString)]
      fromJSVal @MisoString dataWidthVal
  -- Try to get height from attribute (SVG) or data attribute (symbol)
  heightVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("height" :: MisoString)]
  mHeight <- fromJSVal @MisoString heightVal
  mHeight' <- case mHeight of
    Just h -> pure (Just h)
    Nothing -> do
      dataHeightVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("data-height" :: MisoString)]
      fromJSVal @MisoString dataHeightVal
  -- Get viewBox attribute (same for both SVG and symbol)
  viewBoxVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("viewBox" :: MisoString)]
  mViewBox <- fromJSVal @MisoString viewBoxVal
  -- Try to get verticalAlign from style (SVG) or data attribute (symbol)
  styleObj <- element ! ("style" :: MisoString)
  vertAlignVal <- styleObj ! ("verticalAlign" :: MisoString)
  mVertAlign <- fromJSVal @MisoString vertAlignVal
  mVertAlign' <- case mVertAlign of
    Just va | fromMisoString va /= ("" :: Text) -> pure (Just va)
    _ -> do
      dataVertAlignVal <- element # ("getAttribute" :: MisoString) $ [toJSVal ("data-vertical-align" :: MisoString)]
      fromJSVal @MisoString dataVertAlignVal
  case (mWidth', mHeight', mViewBox) of
    (Just w, Just h, Just vb) -> do
      let rendered = RenderedFormula
            { formulaId = fid
            , width = fromMisoString w
            , height = fromMisoString h
            , viewBox = fromMisoString vb
            , verticalAlign = maybe "0" fromMisoString mVertAlign'
            }
      consoleLog $ "[extractFormulaDimensions] Got dimensions: " <> ms (show rendered)
      pure (Just rendered)
    _ -> do
      consoleLog "[extractFormulaDimensions] Failed to get dimensions"
      pure Nothing

-- | Log to browser console
consoleLog :: MisoString -> IO ()
consoleLog msg = do
  console <- jsg ("console" :: MisoString)
  msgVal <- toJSVal msg
  _ <- console # ("log" :: MisoString) $ [msgVal]
  pure ()

