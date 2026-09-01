-- | Security headers as a path-keyed WAI middleware. A middleware
-- (rather than per-route Servant headers) because the policy must fail
-- closed: it covers error responses, 404s, the static file server, and
-- any route added later without further opt-in.
--
--   /static/*       -> untouched (subresources; also keeps throwaway
--                      test pages under static frameable)
--   /app/*          -> base CSP + frame-ancestors <Teams allowlist>
--                      (Teams iframes the app itself — decision 9 of
--                      docs/teams-poc-plan.md; the allowlist is
--                      Microsoft hosts only, everything else stays
--                      blocked)
--   everything else -> base CSP + frame-ancestors 'none'
module Competences.Backend.Middleware
  ( securityHeaders
  , defaultTeamsFrameAncestors
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.Wai qualified as Wai

-- | Origins allowed to iframe the app; mirrors marmay-auth's default
-- (not importable — the list is config data, not exported API). Verify
-- against current MS docs when Microsoft's hosting domains churn.
defaultTeamsFrameAncestors :: [Text]
defaultTeamsFrameAncestors =
  [ "teams.microsoft.com"
  , "*.teams.microsoft.com"
  , "*.office.com"
  , "*.microsoft365.com"
  , "*.cloud.microsoft"
  ]

securityHeaders :: [Text] -> Wai.Middleware
securityHeaders frameAncestors app req respond =
  app req (respond . Wai.mapResponseHeaders (headersFor (Wai.pathInfo req) <>))
  where
    headersFor path = case path of
      ("static" : _) -> []
      ("app" : _) -> [csp (T.unwords frameAncestors)]
      _ -> [csp "'none'"]
    csp ancestors =
      ( "Content-Security-Policy"
      , encodeUtf8 (cspHeaderValue <> "; frame-ancestors " <> ancestors)
      )

-- | The base Content Security Policy (formerly a meta tag in the
-- shell; moved here because frame-ancestors cannot be delivered via
-- meta tag and double delivery invites drift).
cspHeaderValue :: Text
cspHeaderValue =
  T.intercalate
    "; "
    [ "default-src 'self'"
    , "script-src 'self' 'unsafe-inline' 'wasm-unsafe-eval' blob:" -- unsafe-inline for JWT, wasm-unsafe-eval for WASM, blob: for MathJax workers
    , "style-src 'self' 'unsafe-inline'" -- unsafe-inline needed for inline styles
    , "connect-src 'self' ws: wss:" -- Allow WebSocket connections
    , "img-src 'self' data:" -- Allow data URIs for images
    , "font-src 'self'"
    , "base-uri 'self'" -- Prevent base tag injection
    , "form-action 'self'" -- Restrict form submissions
    ]
