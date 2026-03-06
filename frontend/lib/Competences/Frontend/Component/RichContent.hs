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
-- File embeds (@!\[\](file:...)@) are resolved via a 'FileResolver' function,
-- allowing uniform error handling regardless of whether no file context is
-- available or a specific file is not found.
module Competences.Frontend.Component.RichContent
  ( -- * Convenience functions
    renderRichText
  , documentView
  , renderRichTextWithFiles
  , documentViewWithFiles

    -- * Component
  , richContentView
  , richContentComponent

    -- * Internal (used by MarkdownEditor preview)
  , renderMarkdownText

    -- * File resolution
  , FileResolver
  , noFiles
  , mkFileResolver
  , resolveFileRef

    -- * Types (re-exported)
  , FormulaCache
  , MD.Document (..)
  )
where

import Competences.Document.FileRef (FileRef (..), SHA256Hash)
import Competences.Frontend.Component.FilePreview (filePreviewComponent)
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.Component.Geometry (renderGeometryBlock)
import Competences.Frontend.SvgEmbed.Manager
  ( EmbeddedSymbol (..)
  , FormulaCache
  , MathDisplay (..)
  , SymbolId (..)
  , hashLatex
  , hashLatexColored
  , lookupCachedFormulas
  , renderFormulaCached
  , svgToDataUrl
  )
import Competences.Frontend.SyncContext.SyncDocument (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Geometry.Eval (extractMathLabels)
import Competences.Markdown.Geometry.Palette (resolveStrokeColor)
import Competences.Markdown.Geometry.Parser (isGeometryInfo, parseGeometry)
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (when)
import Data.Bits (xor, (.&.))
import Data.Char (ord)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Numeric (showHex)
import Text.Read (readMaybe)

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

-- ============================================================================
-- File resolution
-- ============================================================================

-- | A file resolver maps a file embed URL to either an error message
-- or a rendered view. When no file context is available, the resolver
-- always returns 'Left'.
type FileResolver = Text -> Either Text (M.View RichContentModel RichContentAction)

-- | A resolver that always fails -- for use when no file context is available.
-- Returns the URL unchanged so it can be shown as bracketed text.
noFiles :: FileResolver
noFiles url = Left url

-- | Build a resolver from a 'SyncContext' and list of attachments.
-- Successfully resolved files are rendered as 'filePreviewComponent' views.
mkFileResolver :: SyncContext -> [FileRef] -> FileResolver
mkFileResolver syncCtx attachments url =
  case resolveFileRef attachments url of
    Nothing -> Left $ "Datei nicht gefunden: " <> url
    Just fileRef ->
      Right $
        inlineComponent
          ("file-preview-" <> M.ms (show fileRef.hash))
          (filePreviewComponent syncCtx fileRef)

-- | Resolve a file URL (file:name or fileIdx:N) to a FileRef from attachments.
resolveFileRef :: [FileRef] -> Text -> Maybe FileRef
resolveFileRef attachments url
  | Just name <- T.stripPrefix "file:" url =
      find (\fr -> fr.fileName == name) attachments
  | Just idxText <- T.stripPrefix "fileIdx:" url =
      case readMaybe (T.unpack idxText) :: Maybe Int of
        Just idx | idx >= 0, idx < length attachments -> Just (attachments !! idx)
        _ -> Nothing
  | otherwise = Nothing

-- ============================================================================
-- Component
-- ============================================================================

-- | Create a RichContent view from a Document AST.
--
-- @key@ should be unique per content instance (e.g., task ID).
richContentView :: FormulaCache -> Text -> MD.Document -> M.View p a
richContentView fc key doc =
  inlineComponent
    ("rich-" <> M.ms key)
    (richContentComponent fc noFiles [] key doc)

-- | The RichContent component.
--
-- Takes a 'FileResolver' for rendering file embeds, and a list of
-- footer views (e.g., unreferenced attachments) appended after the content.
richContentComponent
  :: FormulaCache
  -> FileResolver
  -> [M.View RichContentModel RichContentAction]
  -> Text
  -> MD.Document
  -> M.Component p RichContentModel RichContentAction
richContentComponent fc resolver footer _key doc =
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
          sids = [hashLatexColored d l mc | (d, l, mc) <- formulas]
      -- Phase 1: instant cache lookup (sub-microsecond IORef read)
      M.io $ do
        cached <- lookupCachedFormulas fc sids
        pure (SymbolsReady cached)
      -- Phase 2: async MathJax render for uncached formulas
      M.withSink $ \sink -> do
        _ <- forkIO $ do
          rendered <- mapM (\(d, l, mc) -> renderFormulaCached fc d l mc) formulas
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

    view m =
      let MD.Document blocks = m.content
       in M.div_
            [class_ "rich-content space-y-4"]
            $ map (renderBlock resolver m.embeddedSymbols) blocks
              ++ footer

-- ============================================================================
-- Formula extraction
-- ============================================================================

-- | Extract all math formulas from a Document AST.
-- Returns (display, latex, maybeColor) triples where the color is a resolved
-- hex string for geometry labels, or Nothing for regular math.
extractFormulas :: MD.Document -> [(MathDisplay, Text, Maybe Text)]
extractFormulas (MD.Document blocks) = concatMap extractFromBlock blocks

extractFromBlock :: MD.Block -> [(MathDisplay, Text, Maybe Text)]
extractFromBlock = \case
  MD.Paragraph inlines -> concatMap extractFromInline inlines
  MD.Heading _ inlines -> concatMap extractFromInline inlines
  MD.FencedCodeBlock info body ->
    case info of
      Just i
        | isGeometryInfo i ->
            case parseGeometry body of
              Right cmds ->
                [ (Inline, latex, resolveColor c)
                | (c, latex) <- extractMathLabels cmds
                ]
              Left _ -> []
      _ -> []
  MD.OrderedList _ items -> concatMap (concatMap extractFromBlock) items
  MD.BulletList items -> concatMap (concatMap extractFromBlock) items
  MD.LetterList items -> concatMap (concatMap extractFromBlock) items
  MD.MathBlock latex -> [(Block, latex, Nothing)]
  MD.ThematicBreak -> []
  MD.Admonition _ mTitle blocks ->
    maybe [] (concatMap extractFromInline) mTitle
      ++ concatMap extractFromBlock blocks
  MD.NotesGrid c1 c2 c3 c4 ->
    concatMap extractFromBlock (c1 ++ c2 ++ c3 ++ c4)
  where
    resolveColor c =
      let hex = resolveStrokeColor c
       in if hex == "currentColor" then Nothing else Just hex

extractFromInline :: MD.Inline -> [(MathDisplay, Text, Maybe Text)]
extractFromInline = \case
  MD.Plain _ -> []
  MD.Emph inlines -> concatMap extractFromInline inlines
  MD.Strong inlines -> concatMap extractFromInline inlines
  MD.Code _ -> []
  MD.MathInline latex -> [(Inline, latex, Nothing)]
  MD.Link _ inlines _ -> concatMap extractFromInline inlines
  MD.FileEmbed _ inlines _ -> concatMap extractFromInline inlines
  MD.SoftLineBreak -> []
  MD.HardLineBreak -> []

-- ============================================================================
-- Rendering
-- ============================================================================

renderBlock :: FileResolver -> Map SymbolId EmbeddedSymbol -> MD.Block -> M.View RichContentModel RichContentAction
renderBlock resolver symbols = \case
  MD.Paragraph inlines ->
    M.p_ [class_ "text-stone-800 leading-relaxed"] $
      map (renderInline resolver symbols) inlines
  MD.Heading level inlines ->
    let (tag, classes) = headingStyle level
     in tag [class_ classes] $ map (renderInline resolver symbols) inlines
  MD.FencedCodeBlock info body ->
    case info of
      Just i | isGeometryInfo i -> renderGeometryBlock symbols info body
      Just "svg" ->
        M.div_
          [class_ "flex justify-center my-4"]
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
      $ map (renderListItem resolver symbols) items
  MD.BulletList items ->
    M.ul_
      [class_ "list-disc ml-6 space-y-2"]
      $ map (renderListItem resolver symbols) items
  MD.LetterList items ->
    M.ol_
      [class_ "list-[lower-alpha] ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map (renderListItem resolver symbols) items
  MD.MathBlock latex ->
    mathImgRef symbols (hashLatex Block latex) latex Block
  MD.ThematicBreak ->
    M.hr_ [class_ "border-t border-stone-300 my-4"]
  MD.Admonition adType mTitle bodyBlocks ->
    renderAdmonition resolver symbols adType mTitle bodyBlocks
  MD.NotesGrid c1 c2 c3 c4 ->
    renderNotesGrid resolver symbols c1 c2 c3 c4

-- | Get HTML tag and CSS classes for heading level
headingStyle :: Int -> ([M.Attribute action] -> [M.View model action] -> M.View model action, Text)
headingStyle 1 = (M.h1_, "text-2xl font-bold text-stone-900 mb-4")
headingStyle 2 = (M.h2_, "text-xl font-semibold text-stone-800 mb-3")
headingStyle 3 = (M.h3_, "text-lg font-semibold text-stone-800 mb-2")
headingStyle 4 = (M.h4_, "text-base font-semibold text-stone-700 mb-2")
headingStyle 5 = (M.h5_, "text-sm font-semibold text-stone-700 mb-1")
headingStyle _ = (M.h6_, "text-sm font-medium text-stone-600 mb-1")

renderListItem :: FileResolver -> Map SymbolId EmbeddedSymbol -> [MD.Block] -> M.View RichContentModel RichContentAction
renderListItem resolver symbols blocks =
  M.li_ [class_ "text-stone-800 leading-relaxed pl-1"] $
    map (renderBlock resolver symbols) blocks

renderAdmonition
  :: FileResolver
  -> Map SymbolId EmbeddedSymbol
  -> MD.AdmonitionType
  -> Maybe [MD.Inline]
  -> [MD.Block]
  -> M.View RichContentModel RichContentAction
renderAdmonition resolver symbols adType mTitle bodyBlocks =
  let label = admonitionLabel adType
      titleView =
        M.div_
          [class_ "font-semibold text-stone-900 mb-2"]
          $ case mTitle of
            Nothing -> [M.text (ms label)]
            Just inlines ->
              M.text (ms (label <> ". "))
                : map (renderInline resolver symbols) inlines
   in M.div_
        [class_ "border-l-4 border-stone-300 pl-4 my-4"]
        (titleView : map (renderBlock resolver symbols) bodyBlocks)

-- | Render a 2x2 BTC notes grid
renderNotesGrid
  :: FileResolver
  -> Map SymbolId EmbeddedSymbol
  -> [MD.Block]
  -> [MD.Block]
  -> [MD.Block]
  -> [MD.Block]
  -> M.View RichContentModel RichContentAction
renderNotesGrid resolver symbols c1 c2 c3 c4 =
  M.div_
    [class_ "grid grid-cols-2 border border-stone-300 rounded-lg my-4 overflow-hidden"]
    [ cell "p-3 border-b border-r border-stone-200 bg-stone-50" c1
    , cell "p-3 border-b border-stone-200 bg-stone-50" c2
    , cell "p-3 border-r border-stone-200" c3
    , cell "p-3" c4
    ]
  where
    cell cls blocks =
      M.div_ [class_ cls] $ map (renderBlock resolver symbols) blocks

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

renderInline :: FileResolver -> Map SymbolId EmbeddedSymbol -> MD.Inline -> M.View RichContentModel RichContentAction
renderInline resolver symbols = \case
  MD.Plain text -> M.text (ms text)
  MD.Emph inlines ->
    M.em_ [class_ "italic"] $ map (renderInline resolver symbols) inlines
  MD.Strong inlines ->
    M.strong_ [class_ "font-semibold"] $ map (renderInline resolver symbols) inlines
  MD.Code text ->
    M.code_ [class_ "bg-stone-100 px-1.5 py-0.5 rounded text-sm font-mono"] [M.text (ms text)]
  MD.MathInline latex ->
    mathImgRef symbols (hashLatex Inline latex) latex Inline
  MD.Link url inlines _title ->
    M.a_
      [ M.textProp (ms ("href" :: Text)) (ms url)
      , class_ "text-sky-600 hover:text-sky-700 underline"
      ]
      $ map (renderInline resolver symbols) inlines
  MD.FileEmbed url _caption _title ->
    case resolver url of
      Left err -> M.span_ [class_ "text-stone-500 text-sm"] [M.text $ ms $ "[" <> err <> "]"]
      Right fileView -> fileView
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
              , M.textProp (ms ("alt" :: Text)) (ms latex)
              , M.textProp (ms ("style" :: Text)) (ms styleVal)
              ]
       in case display of
            Block ->
              M.div_ [class_ "flex justify-center my-2"] [img]
            Inline ->
              M.img_
                [ class_ "math-formula inline-block"
                , M.textProp (ms ("src" :: Text)) (ms es.dataUrl)
                , M.textProp (ms ("alt" :: Text)) (ms latex)
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

-- | Render a Document AST with file attachment support.
-- FileEmbed nodes are rendered as FilePreview components. Unreferenced
-- attachments are listed at the bottom.
documentViewWithFiles :: FormulaCache -> SyncContext -> [FileRef] -> MD.Document -> M.View p a
documentViewWithFiles fc syncCtx attachments doc =
  let key = hashDocument doc
      resolver = mkFileResolver syncCtx attachments
      footer = unreferencedFilesList attachments doc
   in inlineComponent
        ("rich-" <> M.ms key)
        (richContentComponent fc resolver footer key doc)

-- | Convenience function to parse and render rich content with file attachments.
renderRichTextWithFiles :: FormulaCache -> SyncContext -> [FileRef] -> RichContent -> M.View p a
renderRichTextWithFiles fc syncCtx attachments rc =
  case Markdown.parseMarkdown (toRawText rc) of
    Right doc -> documentViewWithFiles fc syncCtx attachments doc
    Left _err ->
      M.pre_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text (ms (toRawText rc))]

-- ============================================================================
-- Unreferenced files
-- ============================================================================

-- | Collect all referenced file hashes from the document AST.
referencedFileHashes :: [FileRef] -> MD.Document -> Set.Set SHA256Hash
referencedFileHashes attachments (MD.Document blocks) =
  Set.fromList $ concatMap (concatMap extractRefsFromInline . blockInlines) blocks
  where
    blockInlines :: MD.Block -> [MD.Inline]
    blockInlines = \case
      MD.Paragraph inlines -> inlines
      MD.Heading _ inlines -> inlines
      MD.OrderedList _ items -> concatMap (concatMap blockInlines) items
      MD.BulletList items -> concatMap (concatMap blockInlines) items
      MD.LetterList items -> concatMap (concatMap blockInlines) items
      MD.Admonition _ mTitle bs ->
        maybe [] id mTitle ++ concatMap blockInlines bs
      MD.NotesGrid c1 c2 c3 c4 ->
        concatMap blockInlines (c1 ++ c2 ++ c3 ++ c4)
      _ -> []

    extractRefsFromInline :: MD.Inline -> [SHA256Hash]
    extractRefsFromInline = \case
      MD.FileEmbed url _ _ ->
        case resolveFileRef attachments url of
          Just fr -> [fr.hash]
          Nothing -> []
      MD.Emph inlines -> concatMap extractRefsFromInline inlines
      MD.Strong inlines -> concatMap extractRefsFromInline inlines
      MD.Link _ inlines _ -> concatMap extractRefsFromInline inlines
      _ -> []

-- | Render the list of unreferenced attachments (files attached but not used in markdown).
unreferencedFilesList
  :: [FileRef] -> MD.Document -> [M.View RichContentModel RichContentAction]
unreferencedFilesList attachments doc
  | null unreferenced = []
  | otherwise =
      [ M.div_
          [class_ "mt-4 pt-4 border-t border-stone-200 space-y-2"]
          ( Typography.small "Anhänge:"
              : map viewUnreferencedFile unreferenced
          )
      ]
  where
    referenced = referencedFileHashes attachments doc
    unreferenced = filter (\fr -> not $ Set.member fr.hash referenced) attachments

    viewUnreferencedFile fr =
      M.div_
        [class_ "flex items-center gap-3 p-2 bg-stone-50 rounded-md border border-stone-200"]
        [ M.div_
            [class_ "flex-1 min-w-0"]
            [ M.div_ [class_ "text-sm font-medium truncate"] [M.text $ ms fr.fileName]
            , Typography.small $
                ms $
                  fr.mimeType <> " (" <> showFileSize fr.fileSize <> ")"
            ]
        ]

-- ============================================================================
-- Internal helpers
-- ============================================================================

-- | Generate a stable hash key from Document
hashDocument :: MD.Document -> Text
hashDocument (MD.Document blocks) =
  let str = show blocks
      djb2Hash = foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381 str
   in "md-" <> T.pack (showHex djb2Hash "")
