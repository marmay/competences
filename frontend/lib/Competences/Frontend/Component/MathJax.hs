{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.MathJax
-- Description : MathJax rendering component for LaTeX math
--
-- A self-contained Miso component that renders LaTeX to SVG using MathJax.
-- The component handles the FFI calls internally - consumers just provide
-- the LaTeX source string.
--
-- Usage:
--
-- @
-- import Competences.Frontend.Component.MathJax (mathJaxView, MathDisplay(..))
--
-- -- Inline math
-- mathJaxView Inline "x^2 + 1"
--
-- -- Block/display math
-- mathJaxView Block "\\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}"
-- @
module Competences.Frontend.Component.MathJax
  ( -- * Component
    mathJaxView
  , mathJaxComponent

    -- * Types
  , MathDisplay (..)
  , MathJaxModel
  , MathJaxAction
  )
where

import Competences.Frontend.MathJax (MathDisplay (..), renderLatex)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Optics.Core ((.~))

-- | Render result - either SVG or error message
data RenderResult
  = RenderSuccess !MisoString
  | RenderError !Text
  deriving (Eq, Show)

-- | Internal model - manages source and rendered state
data MathJaxModel = MathJaxModel
  { source :: !Text
  -- ^ LaTeX source (from parent)
  , result :: !(Maybe RenderResult)
  -- ^ Render result (success with SVG or error)
  , display :: !MathDisplay
  -- ^ Inline or block display mode
  }
  deriving (Eq, Generic, Show)

-- | Internal actions
data MathJaxAction
  = -- | Trigger rendering of the current source
    TriggerRender
  | -- | FFI returned successfully with SVG
    RenderComplete !MisoString
  | -- | FFI failed (MathJax not ready or error)
    RenderFailed !Text
  deriving (Eq, Show)

-- | Create a MathJax view that renders LaTeX
--
-- This is the main entry point. It creates a named component that
-- handles all FFI calls internally.
mathJaxView :: MathDisplay -> Text -> M.View p a
mathJaxView displayMode latexSource =
  V.component
    ("mathjax-" <> M.ms (take 20 $ show $ abs $ hash latexSource))
    (mathJaxComponent displayMode latexSource)
  where
    -- Simple hash to create unique component names
    hash :: Text -> Int
    hash = fromIntegral . sum . map fromEnum . show

-- | The MathJax component
--
-- Handles FFI calls to MathJax and stores the rendered SVG.
mathJaxComponent :: MathDisplay -> Text -> M.Component p MathJaxModel MathJaxAction
mathJaxComponent displayMode latexSource =
  (M.component model update view)
    { M.initialAction = Just TriggerRender
    }
  where
    model =
      MathJaxModel
        { source = latexSource
        , result = Nothing
        , display = displayMode
        }

    update TriggerRender = do
      m <- M.get
      -- Only render if we haven't tried yet
      case m.result of
        Just _ -> pure () -- Already attempted
        Nothing -> M.io $ do
          mSvg <- renderLatex m.display m.source
          pure $ case mSvg of
            Just svg -> RenderComplete svg
            Nothing -> RenderFailed "MathJax not ready"

    update (RenderComplete svg) =
      M.modify $ #result .~ Just (RenderSuccess svg)

    update (RenderFailed err) =
      M.modify $ #result .~ Just (RenderError err)

    view m = case m.result of
      Just (RenderSuccess svg) ->
        -- Inject pre-rendered SVG into virtual DOM using innerHTML property
        -- Note: This bypasses Miso's virtual DOM diffing for this element
        -- CSS to hide assistive MathML is in static/input.css
        let wrapper = if m.display == Block then M.div_ else M.span_
            wrapperClasses = case m.display of
              Block -> "mathjax-block flex justify-center my-2"
              Inline -> "mathjax-inline inline-block align-middle"
         in wrapper
              [ class_ wrapperClasses
              , M.prop (ms ("innerHTML" :: Text)) svg
              ]
              [] -- Children are replaced by innerHTML
      Just (RenderError err) ->
        -- Render failed - show source with error tooltip
        M.code_
          [ class_ "mathjax-error font-mono text-sm text-red-600 bg-red-50 px-1 rounded"
          , M.textProp (ms ("title" :: Text)) (ms err)
          ]
          [M.text $ ms m.source]
      Nothing ->
        -- Still loading
        M.span_
          [class_ "mathjax-loading text-stone-400"]
          [M.text "..."]
