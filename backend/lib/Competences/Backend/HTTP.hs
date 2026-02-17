{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
  , FrontendHashes (..)
  )
where

import Competences.Backend.Auth
  ( JWTSecret
  , OAuth2Config (..)
  , Office365User (..)
  , exchangeCodeForToken
  , generateJWT
  , getUserInfo
  )
import Competences.Backend.HashedFile (FileHashRef, readFileHash)
import Competences.Backend.State (AppState, getDocument)
import Competences.Document (Document (..), User (..))
import Competences.Document.User (Office365Id (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as Char
import Data.IxSet.Typed qualified as Ix
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types (status302, urlEncode)
import Network.Wai (Application, pathInfo, rawQueryString, responseLBS)
import Servant
  ( (:<|>) (..)
  , (:>)
  , Get
  , Handler
  , Header
  , Proxy (..)
  , QueryParam
  , Raw
  , Server
  , ServerError (..)
  , err302
  , err400
  , err500
  , errHeaders
  , serveDirectoryWebApp
  , throwError
  )
import Servant.API (NoContent (..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookieBS)

-- | Hashes for frontend cache busting
data FrontendHashes = FrontendHashes
  { wasmHash :: !FileHashRef
  , indexJsHash :: !FileHashRef
  , jsffiHash :: !FileHashRef
  , mathjaxHash :: !FileHashRef
  , outputCssHash :: !FileHashRef
  }

type AppAPI =
  -- Root redirect to /app/grid
  Get '[HTML] NoContent
    -- OAuth callback - exchange code for token and serve frontend
    :<|> "oauth" :> "callback"
           :> QueryParam "code" Text
           :> QueryParam "state" Text
           :> Header "Cookie" Text
           :> Get '[HTML] Html
    -- Static files
    :<|> "static" :> Raw
    -- App catch-all - initiate OAuth with return URL preservation
    :<|> "app" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: AppState -> OAuth2Config -> JWTSecret -> FilePath -> FrontendHashes -> Servant.Server AppAPI
server state oauth2Config jwtSecret staticDir hashes =
  rootRedirectHandler
    :<|> oauthCallbackHandler state oauth2Config jwtSecret hashes
    :<|> serveDirectoryWebApp staticDir
    :<|> appCatchAllHandler oauth2Config

-- | Cookie name for OAuth state parameter
oauthStateCookieName :: BS.ByteString
oauthStateCookieName = "oauth_state"

-- | Cookie name for return URL after OAuth
oauthReturnUrlCookieName :: BS.ByteString
oauthReturnUrlCookieName = "oauth_return_url"

-- | Redirect root "/" to "/app/grid"
rootRedirectHandler :: Handler NoContent
rootRedirectHandler =
  throwError err302 {errHeaders = [("Location", "/app/grid")]}

-- | Catch-all handler for /app/* routes
-- Saves the requested URL in a cookie and redirects to Office365 OAuth
appCatchAllHandler :: OAuth2Config -> Tagged Handler Application
appCatchAllHandler config = Tagged $ \req respond -> do
  -- Reconstruct the return URL from the request
  -- Servant strips the "app" segment, so pathInfo has segments after /app/
  let segments = pathInfo req
      queryStr = decodeUtf8 $ rawQueryString req
      returnUrl = validateReturnUrl $ "/app/" <> T.intercalate "/" segments <> queryStr

  -- Generate random state for CSRF protection
  csrfState <- UUID.toText <$> UUID.nextRandom

  -- Build authorization URL with state parameter
  let authUrl = getAuthorizationUrlWithState config csrfState

  -- Create cookies (both scoped to /oauth/callback, HttpOnly)
  let stateCookie =
        renderSetCookieBS $
          defaultSetCookie
            { setCookieName = oauthStateCookieName
            , setCookieValue = encodeUtf8 csrfState
            , setCookiePath = Just "/oauth/callback"
            , setCookieHttpOnly = True
            }
      returnUrlCookie =
        renderSetCookieBS $
          defaultSetCookie
            { setCookieName = oauthReturnUrlCookieName
            , setCookieValue = encodeUtf8 returnUrl
            , setCookiePath = Just "/oauth/callback"
            , setCookieHttpOnly = True
            }

  -- Redirect to Office365 with both cookies
  respond $
    responseLBS
      status302
      [ ("Location", encodeUtf8 authUrl)
      , ("Set-Cookie", stateCookie)
      , ("Set-Cookie", returnUrlCookie)
      ]
      ""


-- | Build OAuth authorization URL with state parameter
getAuthorizationUrlWithState :: OAuth2Config -> Text -> Text
getAuthorizationUrlWithState config state =
  T.concat
    [ "https://login.microsoftonline.com/"
    , config.tenantId
    , "/oauth2/v2.0/authorize?"
    , "client_id=" <> config.clientId
    , "&response_type=code"
    , "&redirect_uri=" <> config.redirectUri
    , "&response_mode=query"
    , "&scope=openid%20profile%20email%20User.Read"
    , "&state=" <> decodeUtf8 (urlEncode False (encodeUtf8 state))
    ]

-- | OAuth callback - exchange code for token and serve frontend with JWT
-- Validates state parameter to prevent CSRF attacks
oauthCallbackHandler :: AppState -> OAuth2Config -> JWTSecret -> FrontendHashes -> Maybe Text -> Maybe Text -> Maybe Text -> Handler Html
oauthCallbackHandler appState oauth2Config jwtSecret hashes maybeCode maybeState maybeCookie = do
  -- Validate state parameter (CSRF protection)
  stateFromQuery <- case maybeState of
    Nothing -> throwError err400 {errBody = "Missing state parameter"}
    Just s -> pure s

  stateFromCookie <- case extractStateFromCookie maybeCookie of
    Nothing -> throwError err400 {errBody = "Missing or invalid state cookie"}
    Just s -> pure s

  if stateFromQuery /= stateFromCookie
    then throwError err400 {errBody = "State mismatch - possible CSRF attack"}
    else pure ()

  code <- case maybeCode of
    Nothing -> throwError err400 {errBody = "Missing authorization code"}
    Just c -> pure c

  -- Exchange code for access token
  tokenResult <- liftIO $ exchangeCodeForToken oauth2Config code
  accessToken <- case tokenResult of
    Left err -> throwError err500 {errBody = BL.fromStrict $ encodeUtf8 $ T.pack err}
    Right token -> pure token

  -- Get user info from Microsoft Graph
  userInfoResult <- liftIO $ getUserInfo accessToken
  o365User <- case userInfoResult of
    Left err -> throwError err500 {errBody = BL.fromStrict $ encodeUtf8 $ T.pack err}
    Right info -> pure info

  -- Find user in document by email address
  let email = case o365User.mail of
        Just m -> m
        Nothing -> o365User.userPrincipalName

  userResult <- liftIO $ findUserByEmail appState email
  user <- case userResult of
    Just u -> pure u
    Nothing -> throwError err400
      { errBody = BL.fromStrict $ encodeUtf8 $
          "No user found with email address: " <> email <>
          ". Please contact an administrator to create your user account."
      }

  -- Generate JWT
  jwt <- liftIO $ generateJWT jwtSecret user

  -- Extract return URL from cookie (defaults to /app/grid)
  let returnUrl = extractReturnUrlFromCookie maybeCookie

  -- Read current file hashes (may have been updated by file watcher)
  wasmHash <- liftIO $ readFileHash hashes.wasmHash
  indexJsHash <- liftIO $ readFileHash hashes.indexJsHash
  jsffiHash <- liftIO $ readFileHash hashes.jsffiHash
  mathjaxHash <- liftIO $ readFileHash hashes.mathjaxHash
  outputCssHash <- liftIO $ readFileHash hashes.outputCssHash

  -- Serve frontend HTML with JWT and hashes embedded
  pure $ renderFrontendHTML jwt returnUrl wasmHash indexJsHash jsffiHash mathjaxHash outputCssHash

-- | Extract state value from Cookie header
-- Parses the Cookie header and looks for the oauth_state cookie
extractStateFromCookie :: Maybe Text -> Maybe Text
extractStateFromCookie Nothing = Nothing
extractStateFromCookie (Just cookieHeader) =
  let cookies = parseCookies (encodeUtf8 cookieHeader)
   in decodeUtf8 <$> lookup oauthStateCookieName cookies

-- | Extract return URL from Cookie header (defaults to /app/grid)
extractReturnUrlFromCookie :: Maybe Text -> Text
extractReturnUrlFromCookie Nothing = "/app/grid"
extractReturnUrlFromCookie (Just cookieHeader) =
  let cookies = parseCookies (encodeUtf8 cookieHeader)
   in case decodeUtf8 <$> lookup oauthReturnUrlCookieName cookies of
        Just url -> validateReturnUrl url
        Nothing -> "/app/grid"

-- | Validate a return URL to prevent open redirect and XSS attacks.
-- Must start with "/app" and contain only safe URL characters.
-- Explicitly excludes ' and \ which would break JS string literals.
validateReturnUrl :: Text -> Text
validateReturnUrl url
  | T.isPrefixOf "/app" url && T.all isSafeUrlChar url = url
  | otherwise = "/app/grid"
  where
    isSafeUrlChar c =
      Char.isAlphaNum c || c `elem` ("-._~:/?#[]@!$&()*+,;=%" :: [Char])

-- | Find existing user by email address stored in office365Id field
findUserByEmail :: AppState -> Text -> IO (Maybe User)
findUserByEmail appState email = do
  doc <- getDocument appState
  let o365Id = Office365Id email
  pure $ Ix.getOne $ doc.users Ix.@= o365Id

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

-- | Render frontend HTML with JWT, return URL, and WASM hash embedded
renderFrontendHTML :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Html
renderFrontendHTML jwt returnUrl wasmHash indexJsHash jsffiHash mathjaxHash outputCssHash = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    -- Content Security Policy via meta tag
    -- Prevents XSS attacks by restricting script/style sources
    H.meta ! A.httpEquiv "Content-Security-Policy" ! A.content (H.toValue cspHeaderValue)
    H.title "Meine Mathe-Kompetenzen"
    -- Load Tailwind CSS + Basecoat (single unified build)
    let outputCssUrl = "/static/output.css?v=" <> outputCssHash
    H.link ! A.rel "stylesheet" ! A.href (H.toValue outputCssUrl)
    -- MathJax configuration (must come before loading MathJax)
    H.script $ H.toHtml
      ("window.MathJax = {\
        \startup: { typeset: false },\
        \tex: { packages: ['base', 'ams'] },\
        \svg: { fontCache: 'none' },\
        \options: { enableMenu: false }\
      \};" :: Text)
    -- Load MathJax 4 for LaTeX rendering (async to not block page load)
    let mathjaxUrl = "/static/mathjax-tex-svg.js?v=" <> mathjaxHash
    H.script ! A.src (H.toValue mathjaxUrl) ! H.customAttribute "async" "" $ ""
    H.script $ H.toHtml $
      "// JWT token for WebSocket authentication\n\
      \window.COMPETENCES_JWT = '" <> jwt <> "';\n\
      \// Debug logging flag (set to true for verbose console output)\n\
      \window.COMPETENCES_DEBUG = false;\n\
      \// File hashes for cache busting\n\
      \window.COMPETENCES_WASM_HASH = '" <> wasmHash <> "';\n\
      \window.COMPETENCES_JSFFI_HASH = '" <> jsffiHash <> "';\n\
      \// Restore original URL after OAuth redirect\n\
      \history.replaceState(null, '', '" <> returnUrl <> "');"
  H.body ! A.class_ "theme-claude" $ do
    -- Loading indicator (replaced when Miso mounts)
    H.div ! A.class_ "flex items-center justify-center h-screen" $
      H.p ! A.class_ "text-lg text-muted-foreground" $
        "Anwendung wird geladen\x2026"
    -- Load application code (with cache-busting hash)
    let indexJsUrl = "/static/index.js?v=" <> indexJsHash
    H.script ! A.src (H.toValue indexJsUrl) ! A.type_ "module" $ ""
