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
-- import Competences.Markdown.Parser (parseMarkdown)
--
-- case parseMarkdown text of
--   Right doc -> documentView componentKey doc
--   Left err -> errorView err
-- @
module Competences.Frontend.Component.RichContent
  ( -- * Convenience functions
    renderRichText
  , documentView

    -- * Component
  , richContentView
  , richContentComponent

    -- * Internal (used by MarkdownEditor preview)
  , renderMarkdownText

    -- * Types (re-exported)
  , MD.Document (..)
  )
where

import Competences.Frontend.Component.Geometry (renderGeometryText)
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
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, toRawText)
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
import Miso.Svg.Element qualified as Svg
import Numeric (showHex)
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
  { content :: !MD.Document
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

-- | Create a RichContent view from a new Document AST
--
-- @key@ should be unique per content instance (e.g., task ID).
-- The component will manage its own SVG container lifecycle.
richContentView :: Text -> MD.Document -> M.View p a
richContentView key doc =
  component
    ("rich-" <> M.ms key)
    (richContentComponent key doc)

-- | The RichContent component
richContentComponent :: Text -> MD.Document -> M.Component p RichContentModel RichContentAction
richContentComponent key doc =
  (M.component model update view)
    { M.initialAction = Just RenderMath
    , M.eventPropagation = True
    }
  where
    containerId' = ComponentContainerId key

    model =
      RichContentModel
        { content = doc
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

-- | Extract all math formulas from a Document AST
extractFormulas :: MD.Document -> [(MathDisplay, Text)]
extractFormulas (MD.Document blocks) = concatMap extractFromBlock blocks

extractFromBlock :: MD.Block -> [(MathDisplay, Text)]
extractFromBlock = \case
  MD.Paragraph inlines -> concatMap extractFromInline inlines
  MD.Heading _ inlines -> concatMap extractFromInline inlines
  MD.FencedCodeBlock _ _ -> []
  MD.OrderedList _ items -> concatMap (concatMap extractFromBlock) items
  MD.LetterList items -> concatMap (concatMap extractFromBlock) items
  MD.MathBlock latex -> [(Block, latex)]
  MD.ThematicBreak -> []

extractFromInline :: MD.Inline -> [(MathDisplay, Text)]
extractFromInline = \case
  MD.Plain _ -> []
  MD.Emph inlines -> concatMap extractFromInline inlines
  MD.Strong inlines -> concatMap extractFromInline inlines
  MD.Code _ -> []
  MD.MathInline latex -> [(Inline, latex)]
  MD.Link _ inlines _ -> concatMap extractFromInline inlines
  MD.SoftLineBreak -> []
  MD.HardLineBreak -> []

-- | Render a single formula to the container
renderSingleFormula :: ComponentContainerId -> (MathDisplay, Text) -> IO (Maybe RenderedFormula)
renderSingleFormula cid (display, latex) = renderFormula cid display latex

-- | Render Document AST with SVG references for math, including cleanup hook
renderContent :: Map FormulaId RenderedFormula -> MD.Document -> M.View RichContentModel RichContentAction
renderContent formulas (MD.Document blocks) =
  M.div_
    [ class_ "rich-content space-y-4"
    , onBeforeDestroyed Unmounted -- Trigger cleanup BEFORE component unmounts
    ]
    $ map (renderBlock formulas) blocks

renderBlock :: Map FormulaId RenderedFormula -> MD.Block -> M.View RichContentModel RichContentAction
renderBlock formulas = \case
  MD.Paragraph inlines ->
    M.p_ [class_ "text-stone-800 leading-relaxed"] $
      map (renderInline formulas) inlines
  MD.Heading level inlines ->
    let (tag, classes) = headingStyle level
     in tag [class_ classes] $ map (renderInline formulas) inlines
  MD.FencedCodeBlock info body ->
    case info of
      Just "geometry" -> renderGeometryText body
      Just "svg" ->
        -- SVG source displayed as code (raw HTML injection not supported in Miso)
        M.pre_
          [class_ "bg-stone-100 border border-stone-200 rounded-md p-3 text-sm font-mono overflow-x-auto"]
          [M.code_ [] [M.text (ms body)]]
      _ ->
        M.pre_
          [class_ "bg-stone-100 border border-stone-200 rounded-md p-3 text-sm font-mono overflow-x-auto"]
          [M.code_ [] [M.text (ms body)]]
  MD.OrderedList _start items ->
    M.ol_
      [class_ "list-decimal ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem formulas) items
  MD.LetterList items ->
    M.ol_
      [class_ "list-[lower-alpha] ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem formulas) items
  MD.MathBlock latex ->
    svgUseRef formulas (hashLatex Block latex) Block
  MD.ThematicBreak ->
    M.hr_ [class_ "border-t border-stone-300 my-4"]

-- | Get HTML tag and CSS classes for heading level
headingStyle :: Int -> ([M.Attribute action] -> [M.View model action] -> M.View model action, Text)
headingStyle 1 = (M.h1_, "text-2xl font-bold text-stone-900 mb-4")
headingStyle 2 = (M.h2_, "text-xl font-semibold text-stone-800 mb-3")
headingStyle 3 = (M.h3_, "text-lg font-semibold text-stone-800 mb-2")
headingStyle 4 = (M.h4_, "text-base font-semibold text-stone-700 mb-2")
headingStyle 5 = (M.h5_, "text-sm font-semibold text-stone-700 mb-1")
headingStyle _ = (M.h6_, "text-sm font-medium text-stone-600 mb-1")

renderListItem :: Map FormulaId RenderedFormula -> [MD.Block] -> M.View RichContentModel RichContentAction
renderListItem formulas blocks =
  M.li_ [class_ "text-stone-800 leading-relaxed pl-1"] $
    map (renderBlock formulas) blocks

renderInline :: Map FormulaId RenderedFormula -> MD.Inline -> M.View RichContentModel RichContentAction
renderInline formulas = \case
  MD.Plain text -> M.text (ms text)
  MD.Emph inlines ->
    M.em_ [class_ "italic"] $ map (renderInline formulas) inlines
  MD.Strong inlines ->
    M.strong_ [class_ "font-semibold"] $ map (renderInline formulas) inlines
  MD.Code text ->
    M.code_ [class_ "bg-stone-100 px-1.5 py-0.5 rounded text-sm font-mono"] [M.text (ms text)]
  MD.MathInline latex ->
    svgUseRef formulas (hashLatex Inline latex) Inline
  MD.Link url inlines _title ->
    M.a_
      [ M.textProp (ms ("href" :: Text)) (ms url)
      , class_ "text-sky-600 hover:text-sky-700 underline"
      ]
      $ map (renderInline formulas) inlines
  MD.SoftLineBreak -> M.text " "
  MD.HardLineBreak -> M.br_ []

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
-- Convenience functions
-- ============================================================================

-- | Render a Document AST to Miso view
documentView :: MD.Document -> M.View p a
documentView doc =
  let key = hashDocument doc
   in richContentView key doc

-- | Convenience function to parse and render rich content in one step
--
-- On parse failure, shows the raw text in a code block with error styling.
renderRichText :: RichContent -> M.View p a
renderRichText rc = renderMarkdownText (toRawText rc)

-- | Parse and render raw 'Text' as markdown.
--
-- Useful for rendering 'Text' fields (e.g. phase notes) that aren't
-- wrapped in 'RichContent'. On parse failure, shows the raw text
-- in a code block with error styling.
renderMarkdownText :: Text -> M.View p a
renderMarkdownText raw =
  case Markdown.parseMarkdown raw of
    Right doc -> documentView doc
    Left _err ->
      M.pre_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text (ms raw)]

-- | Generate a stable hash key from Document
hashDocument :: MD.Document -> Text
hashDocument (MD.Document blocks) =
  let str = show blocks
      djb2Hash = foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381 str
   in "md-" <> T.pack (showHex djb2Hash "")
