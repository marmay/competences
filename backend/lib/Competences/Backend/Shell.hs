module Competences.Backend.Shell
  ( ShellHashes(..)
  , ShellConfig(..)
  , mkShellConfig
  , renderShell
  )
  where

import Competences.Backend.HashedFile (FileHashRef, readFileHash)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Data.Text (Text)
import qualified Data.Text as T

-- | Hashes for frontend cache busting
data ShellHashes = ShellHashes
  { wasmHash :: !FileHashRef
  , indexJsHash :: !FileHashRef
  , jsffiHash :: !FileHashRef
  , mathjaxHash :: !FileHashRef
  , outputCssHash :: !FileHashRef
  }

data ShellConfig = ShellConfig
  { wasmHash :: !Text
  , indexJsHash :: !Text
  , jsffiHash :: !Text
  , mathjaxHash :: !Text
  , outputCssHash :: !Text
  , returnUrl :: !Text
  } deriving (Eq, Show)
  
mkShellConfig :: ShellHashes -> IO ShellConfig
mkShellConfig hashes = ShellConfig
  <$> readFileHash hashes.wasmHash
  <*> readFileHash hashes.indexJsHash
  <*> readFileHash hashes.jsffiHash
  <*> readFileHash hashes.mathjaxHash
  <*> readFileHash hashes.outputCssHash
  <*> pure ""

-- | Render frontend HTML with JWT, return URL, and WASM hash embedded
renderShell :: ShellConfig -> Html
renderShell shellConfig = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    -- Content Security Policy via meta tag
    -- Prevents XSS attacks by restricting script/style sources
    H.meta ! A.httpEquiv "Content-Security-Policy" ! A.content (H.toValue cspHeaderValue)
    H.title "Meine Mathe-Kompetenzen"
    -- Favicon (inline SVG - competence grid icon in sky-600)
    H.link ! A.rel "icon" ! A.type_ "image/svg+xml"
      ! A.href "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%230284c7' stroke-width='1.5'><rect x='3' y='3' width='7' height='7' rx='1'/><rect x='14' y='3' width='7' height='7' rx='1'/><rect x='3' y='14' width='7' height='7' rx='1'/><rect x='14' y='14' width='7' height='7' rx='1'/></svg>"
    -- Load Tailwind CSS + Basecoat (single unified build)
    let outputCssUrl = "/static/output.css?v=" <> shellConfig.outputCssHash
    H.link ! A.rel "stylesheet" ! A.href (H.toValue outputCssUrl)
    -- MathJax configuration (must come before loading MathJax)
    H.script $ H.toHtml
      ("window.MathJax = {\
        \loader: { paths: { fonts: '/static' } },\
        \startup: { typeset: false },\
        \tex: { packages: ['base', 'ams'] },\
        \svg: { fontCache: 'local' },\
        \options: { enableMenu: false, enableEnrichment: false, enableSpeech: false, enableBraille: false, enableExplorer: false, enableComplexity: false, menuOptions: { settings: { enrich: false, speech: false, braille: false, collapsible: false } } }\
      \};" :: Text)
    -- Load MathJax 4 for LaTeX rendering (async to not block page load)
    let mathjaxUrl = "/static/mathjax-tex-svg.js?v=" <> shellConfig.mathjaxHash
    H.script ! A.src (H.toValue mathjaxUrl) ! H.customAttribute "async" "" $ ""
    H.script $ H.toHtml $
      "// Debug logging flag (set to true for verbose console output)\n\
      \window.COMPETENCES_DEBUG = false;\n\
      \// File hashes for cache busting\n\
      \window.COMPETENCES_WASM_HASH = '" <> shellConfig.wasmHash <> "';\n\
      \window.COMPETENCES_JSFFI_HASH = '" <> shellConfig.jsffiHash <> "';\n\
      \// Restore original URL after OAuth redirect\n\
      \history.replaceState(null, '', '" <> shellConfig.returnUrl <> "');"
  H.body ! A.class_ "theme-claude" $ do
    -- Loading indicator (replaced when Miso mounts)
    H.div ! A.class_ "flex items-center justify-center h-screen" $
      H.p ! A.class_ "text-lg text-muted-foreground" $
        "Anwendung wird geladen\x2026"
    -- Load application code (with cache-busting hash)
    let indexJsUrl = "/static/index.js?v=" <> shellConfig.indexJsHash
    H.script ! A.src (H.toValue indexJsUrl) ! A.type_ "module" $ ""

-- | Content Security Policy header value
-- Restricts script/style sources to prevent XSS attacks.
-- Note: frame-ancestors must be delivered via HTTP header, not meta tag.
-- For clickjacking protection, consider adding X-Frame-Options header.
cspHeaderValue :: Text
cspHeaderValue = T.intercalate "; "
  [ "default-src 'self'"
  , "script-src 'self' 'unsafe-inline' 'wasm-unsafe-eval' blob:"  -- unsafe-inline for JWT, wasm-unsafe-eval for WASM, blob: for MathJax workers
  , "style-src 'self' 'unsafe-inline'"   -- unsafe-inline needed for inline styles
  , "connect-src 'self' ws: wss:"        -- Allow WebSocket connections
  , "img-src 'self' data:"               -- Allow data URIs for images
  , "font-src 'self'"
  , "base-uri 'self'"                    -- Prevent base tag injection
  , "form-action 'self'"                 -- Restrict form submissions
  ]
