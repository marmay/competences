{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.RichContent
-- Description : Rich content component with MathJax formula rendering
--
-- A single Miso component that manages content containing math formulas.
-- Formulas are rendered to SVGs in a hidden container outside Miso's
-- virtual DOM, and referenced via <svg><use href="..."/></svg>.
--
-- Architecture:
-- - Each component instance gets a unique container ID
-- - On mount: create container, render all formulas
-- - On content change: clear container, re-render formulas
-- - On unmount: destroy container (cleanup)
-- - View: render AST with <use> references for math
--
-- Usage:
--
-- @
-- import Competences.Frontend.Component.RichContent (richContentView)
-- import Competences.TaskContent.Parser (parseTaskContent)
--
-- case parseTaskContent text of
--   Right ast -> richContentView componentKey ast
--   Left err -> errorView err
-- @
module Competences.Frontend.Component.RichContent
  ( -- * Convenience functions
    renderRichText
  , taskContentView

    -- * Component
  , richContentView
  , richContentComponent

    -- * Types (re-exported)
  , TaskContent (..)
  )
where

import Competences.TaskContent.Parser (parseTaskContent)
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Competences.Frontend.MathJax.Manager
  ( ComponentContainerId (..)
  , FormulaId (..)
  , MathDisplay (..)
  , RenderedFormula (..)
  , createComponentContainer
  , destroyComponentContainer
  , hashLatex
  , renderFormula
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Competences.TaskContent.AST
import Data.Bits (xor, (.&.))
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Event (onBeforeDestroyed)
import Miso.Html qualified as M
import Miso.Html.Property (height_, href_, width_)
import Miso.String (ms)
import Numeric (showHex)
import Miso.Svg.Element qualified as Svg
import Optics.Core ((.~))

-- | Render state
data RenderState
  = -- | Math not yet rendered, content not visible
    Pending
  | -- | All math rendered, safe to display
    Ready
  deriving (Eq, Show, Generic)

-- | Model tracks the content and render state
data RichContentModel = RichContentModel
  { content :: !TaskContent
  -- ^ Parsed AST
  , containerId :: !ComponentContainerId
  -- ^ Unique container ID for this instance
  , renderedFormulas :: !(Map FormulaId RenderedFormula)
  -- ^ Formulas that have been rendered, with their dimensions
  , renderState :: !RenderState
  -- ^ Current render state
  }
  deriving (Eq, Show, Generic)

-- | Component actions
data RichContentAction
  = -- | Initial action: create container and render all formulas
    RenderMath
  | -- | All formulas rendered successfully
    MathRendered !(Map FormulaId RenderedFormula)
  | -- | Rendering failed
    MathFailed !Text
  | -- | Component unmounting: cleanup container
    Unmounted
  deriving (Eq, Show)

-- | Create a RichContent view
--
-- @key@ should be unique per content instance (e.g., task ID).
-- The component will manage its own SVG container lifecycle.
richContentView :: Text -> TaskContent -> M.View p a
richContentView key content' =
  V.component
    ("rich-" <> M.ms key)
    (richContentComponent key content')

-- | The RichContent component
richContentComponent :: Text -> TaskContent -> M.Component p RichContentModel RichContentAction
richContentComponent key content' =
  (M.component model update view)
    { M.initialAction = Just RenderMath
    }
  where
    containerId' = ComponentContainerId key

    model =
      RichContentModel
        { content = content'
        , containerId = containerId'
        , renderedFormulas = Map.empty
        , renderState = Pending
        }

    update RenderMath = do
      m <- M.get
      -- Create container and render formulas
      M.io $ do
        createComponentContainer m.containerId
        let formulas = extractFormulas m.content
        rendered <- mapM (renderSingleFormula m.containerId) formulas
        let successful = Map.fromList [(rf.formulaId, rf) | Just rf <- rendered]
        pure $ MathRendered successful

    update (MathRendered formulaMap) =
      M.modify $ \m ->
        m
          { renderedFormulas = formulaMap
          , renderState = Ready
          }

    update (MathFailed _err) =
      -- On failure, still show content (formulas will be empty)
      M.modify $ #renderState .~ Ready

    update Unmounted = do
      m <- M.get
      -- Cleanup: destroy container when component unmounts
      M.io_ $ destroyComponentContainer m.containerId

    view m = case m.renderState of
      Pending -> M.text "" -- Empty during render
      Ready -> renderContent m.renderedFormulas m.content

-- | Extract all math formulas from content AST
extractFormulas :: TaskContent -> [(MathDisplay, Text)]
extractFormulas (TaskContent blocks) = concatMap extractFromBlock blocks

extractFromBlock :: Block -> [(MathDisplay, Text)]
extractFromBlock = \case
  Paragraph inlines -> concatMap extractFromInline inlines
  SubTaskList items -> concatMap (concatMap extractFromBlock . (.content)) items
  SubQuestionList items -> concatMap (concatMap extractFromBlock . (.content)) items
  MathBlock latex -> [(Block, latex)]
  Heading _ inlines -> concatMap extractFromInline inlines

extractFromInline :: Inline -> [(MathDisplay, Text)]
extractFromInline = \case
  Plain _ -> []
  Emph inlines -> concatMap extractFromInline inlines
  Strong inlines -> concatMap extractFromInline inlines
  MathInline latex -> [(Inline, latex)]

-- | Render a single formula to the container
renderSingleFormula :: ComponentContainerId -> (MathDisplay, Text) -> IO (Maybe RenderedFormula)
renderSingleFormula cid (display, latex) = renderFormula cid display latex

-- | Render content AST with SVG references for math, including cleanup hook
renderContent :: Map FormulaId RenderedFormula -> TaskContent -> M.View RichContentModel RichContentAction
renderContent formulas (TaskContent blocks) =
  M.div_
    [ class_ "rich-content space-y-4"
    , onBeforeDestroyed Unmounted -- Trigger cleanup BEFORE component unmounts
    ]
    $ map (renderBlock formulas) blocks

renderBlock :: Map FormulaId RenderedFormula -> Block -> M.View RichContentModel RichContentAction
renderBlock formulas = \case
  Paragraph inlines ->
    M.p_ [class_ "text-stone-800 leading-relaxed"] $
      map (renderInline formulas) inlines
  SubTaskList items ->
    M.ol_
      [class_ "list-[lower-alpha] ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem formulas) items
  SubQuestionList items ->
    M.ol_
      [class_ "list-decimal ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem formulas) items
  MathBlock latex ->
    svgUseRef formulas (hashLatex Block latex) Block
  Heading level inlines ->
    let (tag, classes) = headingStyle level
     in tag [class_ classes] $ map (renderInline formulas) inlines

-- | Get HTML tag and CSS classes for heading level
headingStyle :: Int -> ([M.Attribute action] -> [M.View model action] -> M.View model action, Text)
headingStyle 1 = (M.h1_, "text-2xl font-bold text-stone-900 mb-4")
headingStyle 2 = (M.h2_, "text-xl font-semibold text-stone-800 mb-3")
headingStyle 3 = (M.h3_, "text-lg font-semibold text-stone-800 mb-2")
headingStyle 4 = (M.h4_, "text-base font-semibold text-stone-700 mb-2")
headingStyle 5 = (M.h5_, "text-sm font-semibold text-stone-700 mb-1")
headingStyle _ = (M.h6_, "text-sm font-medium text-stone-600 mb-1")

renderListItem :: Map FormulaId RenderedFormula -> ListItem -> M.View RichContentModel RichContentAction
renderListItem formulas item =
  M.li_ [class_ "text-stone-800 leading-relaxed pl-1"] $
    map (renderBlock formulas) item.content

renderInline :: Map FormulaId RenderedFormula -> Inline -> M.View RichContentModel RichContentAction
renderInline formulas = \case
  Plain text -> M.text (ms text)
  Emph inlines ->
    M.em_ [class_ "italic"] $ map (renderInline formulas) inlines
  Strong inlines ->
    M.strong_ [class_ "font-semibold"] $ map (renderInline formulas) inlines
  MathInline latex ->
    svgUseRef formulas (hashLatex Inline latex) Inline

-- | Create SVG with <use> reference to rendered formula
-- Uses dimensions from RenderedFormula for proper sizing
-- Uses Miso.Svg.Element to ensure proper SVG namespace
svgUseRef :: Map FormulaId RenderedFormula -> FormulaId -> MathDisplay -> M.View RichContentModel RichContentAction
svgUseRef formulas fid display =
  case Map.lookup fid formulas of
    Nothing ->
      -- Formula not rendered yet - show placeholder
      M.span_ [class_ "text-red-500"] [M.text "[math]"]
    Just rf ->
      let wrapperClasses = case display of
            Block -> "mathjax-block flex justify-center my-2"
            Inline -> "mathjax-inline inline-block"
       in Svg.svg_
            [ class_ wrapperClasses
            , width_ (ms rf.width)
            , height_ (ms rf.height)
            , M.textProp (ms ("style" :: Text)) (ms ("vertical-align: " <> rf.verticalAlign))
            ]
            [ Svg.use_
                [href_ (ms ("#" <> fid.unFormulaId))]
            ]

-- ============================================================================
-- Convenience functions (merged from View.TaskContent)
-- ============================================================================

-- | Render parsed TaskContent AST to Miso view
--
-- Uses RichContent component which renders math formulas to a hidden
-- SVG container and references them via <use> elements.
taskContentView :: TaskContent -> M.View p a
taskContentView ast =
  -- Generate a stable key from the content for component identity
  let key = hashContent ast
   in richContentView key ast

-- | Convenience function to parse and render rich content in one step
--
-- On parse failure, shows the raw text in a code block with error styling.
renderRichText :: RichContent -> M.View p a
renderRichText rc =
  let raw = toRawText rc
   in case parseTaskContent raw of
        Left _err ->
          -- Parse error - show raw text as fallback
          M.pre_
            [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
            [M.text (ms raw)]
        Right ast ->
          taskContentView ast

-- | Generate a stable hash key from TaskContent
-- Uses DJB2-like hash (works on 32-bit WASM)
hashContent :: TaskContent -> Text
hashContent (TaskContent blocks) =
  let str = show blocks
      djb2Hash = foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381 str
   in "tc-" <> T.pack (showHex djb2Hash "")
