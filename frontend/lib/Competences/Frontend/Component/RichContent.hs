{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.RichContent
-- Description : Rich content component with SVG embedding (MathJax + raw SVG)
--
-- A single Miso component that manages content containing math formulas
-- and raw SVG blocks. MathJax formulas are rendered to data URLs via
-- 'renderFormula', and raw SVGs are encoded purely via 'svgToDataUrl'.
-- Both are displayed as @\<img\>@ elements, which browsers fully sandbox
-- (no script execution, no event handlers, no external resource loading).
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
  , FormulaCache
  , MD.Document (..)
  )
where

import Competences.Frontend.Component.Geometry (renderGeometryBlock)
import Competences.Markdown.Geometry.Eval (extractMathLabels)
import Competences.Markdown.Geometry.Parser (isGeometryInfo, parseGeometry)
import Competences.Frontend.SvgEmbed.Manager
  ( EmbeddedSymbol (..)
  , FormulaCache
  , MathDisplay (..)
  , SymbolId (..)
  , hashLatex
  , lookupCachedFormulas
  , renderFormulaCached
  , svgToDataUrl
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when)
import Data.Bits (xor, (.&.))
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Numeric (showHex)

-- | Model tracks the content and rendered symbols
data RichContentModel = RichContentModel
  { content :: !MD.Document
  -- ^ Parsed AST
  , embeddedSymbols :: !(Map SymbolId EmbeddedSymbol)
  -- ^ MathJax-rendered formulas with their data URLs and dimensions
  }
  deriving (Eq, Show, Generic)

-- | Component actions
data RichContentAction
  = -- | Initial action: render all MathJax formulas
    RenderMath
  | -- | A batch of formulas rendered successfully (merged into existing)
    SymbolsReady !(Map SymbolId EmbeddedSymbol)
  | -- | Retry rendering after exponential backoff (attempt number)
    RetryRender !Int
  deriving (Eq, Show)

-- | Create a RichContent view from a new Document AST
--
-- @key@ should be unique per content instance (e.g., task ID).
richContentView :: FormulaCache -> Text -> MD.Document -> M.View p a
richContentView fc key doc =
  inlineComponent
    ("rich-" <> M.ms key)
    (richContentComponent fc key doc)

-- | The RichContent component
richContentComponent :: FormulaCache -> Text -> MD.Document -> M.Component p RichContentModel RichContentAction
richContentComponent fc _key doc =
  (M.component model update view)
    { M.initialAction = Just RenderMath
    , M.eventPropagation = True
    }
  where
    model =
      RichContentModel
        { content = doc
        , embeddedSymbols = Map.empty
        }

    update RenderMath = do
      m <- M.get
      let formulas = extractFormulas m.content
          sids = [hashLatex d l | (d, l) <- formulas]
      -- Phase 1: instant cache lookup (sub-microsecond IORef read)
      M.io $ do
        cached <- lookupCachedFormulas fc sids
        pure (SymbolsReady cached)
      -- Phase 2: async MathJax render for uncached formulas
      M.withSink $ \sink -> do
        _ <- forkIO $ do
          rendered <- mapM (uncurry (renderFormulaCached fc)) formulas
          let successful = Map.fromList [(es.symbolId, es) | Just es <- rendered]
              failCount = length formulas - Map.size successful
          sink (SymbolsReady successful)
          when (failCount > 0) $ sink (RetryRender 0)
        pure ()

    update (SymbolsReady newSymbols) =
      M.modify $ \m -> m {embeddedSymbols = m.embeddedSymbols <> newSymbols}

    update (RetryRender n)
      | n >= 5 = pure () -- give up after 5 retries
      | otherwise = M.io $ do
          threadDelay (200_000 * (2 ^ n)) -- 200ms, 400ms, 800ms, 1.6s, 3.2s
          pure RenderMath

    view m = renderContent m.embeddedSymbols m.content

-- | Extract all math formulas from a Document AST
extractFormulas :: MD.Document -> [(MathDisplay, Text)]
extractFormulas (MD.Document blocks) = concatMap extractFromBlock blocks

extractFromBlock :: MD.Block -> [(MathDisplay, Text)]
extractFromBlock = \case
  MD.Paragraph inlines -> concatMap extractFromInline inlines
  MD.Heading _ inlines -> concatMap extractFromInline inlines
  MD.FencedCodeBlock info body ->
    case info of
      Just i | isGeometryInfo i ->
        case parseGeometry body of
          Right cmds -> [(Inline, latex) | latex <- extractMathLabels cmds]
          Left _ -> []
      _ -> []
  MD.OrderedList _ items -> concatMap (concatMap extractFromBlock) items
  MD.BulletList items -> concatMap (concatMap extractFromBlock) items
  MD.LetterList items -> concatMap (concatMap extractFromBlock) items
  MD.MathBlock latex -> [(Block, latex)]
  MD.ThematicBreak -> []
  MD.Admonition _ mTitle blocks ->
    maybe [] (concatMap extractFromInline) mTitle
      ++ concatMap extractFromBlock blocks
  MD.NotesGrid c1 c2 c3 c4 ->
    concatMap extractFromBlock (c1 ++ c2 ++ c3 ++ c4)

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

-- | Render Document AST with <img> data URLs for embedded symbols
renderContent :: Map SymbolId EmbeddedSymbol -> MD.Document -> M.View RichContentModel RichContentAction
renderContent symbols (MD.Document blocks) =
  M.div_
    [class_ "rich-content space-y-4"]
    $ map (renderBlock symbols) blocks

renderBlock :: Map SymbolId EmbeddedSymbol -> MD.Block -> M.View RichContentModel RichContentAction
renderBlock symbols = \case
  MD.Paragraph inlines ->
    M.p_ [class_ "text-stone-800 leading-relaxed"] $
      map (renderInline symbols) inlines
  MD.Heading level inlines ->
    let (tag, classes) = headingStyle level
     in tag [class_ classes] $ map (renderInline symbols) inlines
  MD.FencedCodeBlock info body ->
    case info of
      Just i | isGeometryInfo i -> renderGeometryBlock symbols info body
      Just "svg" ->
        M.div_ [class_ "flex justify-center my-4"]
          [ M.img_
              [ M.textProp (ms ("src" :: Text)) (ms (svgToDataUrl body))
              , M.textProp (ms ("style" :: Text)) (ms ("max-width:100%;height:auto" :: Text))
              ]
          ]
      _ ->
        M.pre_
          [class_ "bg-stone-100 border border-stone-200 rounded-md p-3 text-sm font-mono overflow-x-auto"]
          [M.code_ [] [M.text (ms body)]]
  MD.OrderedList _start items ->
    M.ol_
      [class_ "list-decimal ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem symbols) items
  MD.BulletList items ->
    M.ul_
      [class_ "list-disc ml-6 space-y-2"]
      $ map (renderListItem symbols) items
  MD.LetterList items ->
    M.ol_
      [class_ "list-[lower-alpha] ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem symbols) items
  MD.MathBlock latex ->
    mathImgRef symbols (hashLatex Block latex) latex Block
  MD.ThematicBreak ->
    M.hr_ [class_ "border-t border-stone-300 my-4"]
  MD.Admonition adType mTitle bodyBlocks ->
    renderAdmonition symbols adType mTitle bodyBlocks
  MD.NotesGrid c1 c2 c3 c4 ->
    renderNotesGrid symbols c1 c2 c3 c4

-- | Get HTML tag and CSS classes for heading level
headingStyle :: Int -> ([M.Attribute action] -> [M.View model action] -> M.View model action, Text)
headingStyle 1 = (M.h1_, "text-2xl font-bold text-stone-900 mb-4")
headingStyle 2 = (M.h2_, "text-xl font-semibold text-stone-800 mb-3")
headingStyle 3 = (M.h3_, "text-lg font-semibold text-stone-800 mb-2")
headingStyle 4 = (M.h4_, "text-base font-semibold text-stone-700 mb-2")
headingStyle 5 = (M.h5_, "text-sm font-semibold text-stone-700 mb-1")
headingStyle _ = (M.h6_, "text-sm font-medium text-stone-600 mb-1")

renderListItem :: Map SymbolId EmbeddedSymbol -> [MD.Block] -> M.View RichContentModel RichContentAction
renderListItem symbols blocks =
  M.li_ [class_ "text-stone-800 leading-relaxed pl-1"] $
    map (renderBlock symbols) blocks

renderAdmonition
  :: Map SymbolId EmbeddedSymbol
  -> MD.AdmonitionType
  -> Maybe [MD.Inline]
  -> [MD.Block]
  -> M.View RichContentModel RichContentAction
renderAdmonition symbols adType mTitle bodyBlocks =
  let label = admonitionLabel adType
      titleView =
        M.div_
          [class_ "font-semibold text-stone-900 mb-2"]
          $ case mTitle of
            Nothing -> [M.text (ms label)]
            Just inlines ->
              M.text (ms (label <> ". "))
                : map (renderInline symbols) inlines
   in M.div_
        [class_ "border-l-4 border-stone-300 pl-4 my-4"]
        (titleView : map (renderBlock symbols) bodyBlocks)

-- | Render a 2×2 BTC notes grid
renderNotesGrid
  :: Map SymbolId EmbeddedSymbol
  -> [MD.Block]
  -> [MD.Block]
  -> [MD.Block]
  -> [MD.Block]
  -> M.View RichContentModel RichContentAction
renderNotesGrid symbols c1 c2 c3 c4 =
  M.div_
    [class_ "grid grid-cols-2 border border-stone-300 rounded-lg my-4 overflow-hidden"]
    [ cell "p-3 border-b border-r border-stone-200 bg-stone-50" c1
    , cell "p-3 border-b border-stone-200 bg-stone-50" c2
    , cell "p-3 border-r border-stone-200" c3
    , cell "p-3" c4
    ]
  where
    cell cls blocks =
      M.div_ [class_ cls] $ map (renderBlock symbols) blocks

-- | German display label for each admonition type
admonitionLabel :: MD.AdmonitionType -> Text
admonitionLabel = \case
  MD.Definition -> "Definition"
  MD.Theorem -> "Satz"
  MD.Lemma -> "Lemma"
  MD.Proof -> "Beweis"
  MD.Remark -> "Bemerkung"
  MD.Merksatz -> "Merksatz"
  MD.Example -> "Beispiel"

renderInline :: Map SymbolId EmbeddedSymbol -> MD.Inline -> M.View RichContentModel RichContentAction
renderInline symbols = \case
  MD.Plain text -> M.text (ms text)
  MD.Emph inlines ->
    M.em_ [class_ "italic"] $ map (renderInline symbols) inlines
  MD.Strong inlines ->
    M.strong_ [class_ "font-semibold"] $ map (renderInline symbols) inlines
  MD.Code text ->
    M.code_ [class_ "bg-stone-100 px-1.5 py-0.5 rounded text-sm font-mono"] [M.text (ms text)]
  MD.MathInline latex ->
    mathImgRef symbols (hashLatex Inline latex) latex Inline
  MD.Link url inlines _title ->
    M.a_
      [ M.textProp (ms ("href" :: Text)) (ms url)
      , class_ "text-sky-600 hover:text-sky-700 underline"
      ]
      $ map (renderInline symbols) inlines
  MD.SoftLineBreak -> M.text " "
  MD.HardLineBreak -> M.br_ []

-- | Create <img> element with data URL for a MathJax-rendered formula.
-- Shows the LaTeX source as a muted placeholder while rendering is pending.
mathImgRef :: Map SymbolId EmbeddedSymbol -> SymbolId -> Text -> MathDisplay -> M.View RichContentModel RichContentAction
mathImgRef symbols sid latex display =
  case Map.lookup sid symbols of
    Nothing ->
      Typography.placeholder (ms latex)
    Just es ->
      let styleVal =
            "width:" <> es.width
              <> ";height:" <> es.height
              <> ";vertical-align:" <> es.verticalAlign
          img =
            M.img_
              [ class_ "math-formula"
              , M.textProp (ms ("src" :: Text)) (ms es.dataUrl)
              , M.textProp (ms ("style" :: Text)) (ms styleVal)
              ]
       in case display of
            Block ->
              M.div_ [class_ "flex justify-center my-2"] [img]
            Inline ->
              M.img_
                [ class_ "math-formula inline-block"
                , M.textProp (ms ("src" :: Text)) (ms es.dataUrl)
                , M.textProp (ms ("style" :: Text)) (ms styleVal)
                ]

-- ============================================================================
-- Convenience functions
-- ============================================================================

-- | Render a Document AST to Miso view
documentView :: FormulaCache -> MD.Document -> M.View p a
documentView fc doc =
  let key = hashDocument doc
   in richContentView fc key doc

-- | Convenience function to parse and render rich content in one step
--
-- On parse failure, shows the raw text in a code block with error styling.
renderRichText :: FormulaCache -> RichContent -> M.View p a
renderRichText fc rc = renderMarkdownText fc (toRawText rc)

-- | Parse and render raw 'Text' as markdown.
--
-- Useful for rendering 'Text' fields (e.g. phase notes) that aren't
-- wrapped in 'RichContent'. On parse failure, shows the raw text
-- in a code block with error styling.
renderMarkdownText :: FormulaCache -> Text -> M.View p a
renderMarkdownText fc raw =
  case Markdown.parseMarkdown raw of
    Right doc -> documentView fc doc
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
