{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
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
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types (urlEncode)
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
import Servant.HTML.Blaze (HTML)
import Servant.API (NoContent (..))
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Cookie (SetCookie (..), defaultSetCookie, renderSetCookieBS, parseCookies)

type AppAPI =
  -- OAuth initiation - redirect to Office365 with state parameter for CSRF protection
  -- (Headers set via throwError err302 with custom errHeaders)
  Get '[HTML] NoContent
    -- OAuth callback - exchange code for token and serve frontend
    :<|> "oauth" :> "callback"
           :> QueryParam "code" Text
           :> QueryParam "state" Text
           :> Header "Cookie" Text
           :> Get '[HTML] Html
    -- Static files
    :<|> "static" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: AppState -> OAuth2Config -> JWTSecret -> FilePath -> FileHashRef -> Servant.Server AppAPI
server state oauth2Config jwtSecret staticDir wasmHashRef =
  oauthInitHandler oauth2Config
    :<|> oauthCallbackHandler state oauth2Config jwtSecret wasmHashRef
    :<|> serveDirectoryWebApp staticDir

-- | Cookie name for OAuth state parameter
oauthStateCookieName :: BS.ByteString
oauthStateCookieName = "oauth_state"

-- | Redirect to Office365 login with CSRF protection via state parameter
oauthInitHandler :: OAuth2Config -> Handler NoContent
oauthInitHandler config = do
  -- Generate random state for CSRF protection
  state <- liftIO $ UUID.toText <$> UUID.nextRandom

  -- Build authorization URL with state parameter
  let authUrl = getAuthorizationUrlWithState config state

  -- Create cookie with state value (HttpOnly for security)
  -- Note: SameSite=Lax is the browser default for cookies without SameSite set
  let cookie = defaultSetCookie
        { setCookieName = oauthStateCookieName
        , setCookieValue = encodeUtf8 state
        , setCookiePath = Just "/oauth/callback"
        , setCookieHttpOnly = True
        }
      cookieBS = renderSetCookieBS cookie

  -- Redirect with state cookie
  throwError err302
    { errHeaders =
        [ ("Location", encodeUtf8 authUrl)
        , ("Set-Cookie", cookieBS)
        ]
    }


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
oauthCallbackHandler :: AppState -> OAuth2Config -> JWTSecret -> FileHashRef -> Maybe Text -> Maybe Text -> Maybe Text -> Handler Html
oauthCallbackHandler appState oauth2Config jwtSecret wasmHashRef maybeCode maybeState maybeCookie = do
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

  -- Read current WASM hash (may have been updated by file watcher)
  wasmHash <- liftIO $ readFileHash wasmHashRef

  -- Serve frontend HTML with JWT embedded
  pure $ renderFrontendHTML jwt wasmHash

-- | Extract state value from Cookie header
-- Parses the Cookie header and looks for the oauth_state cookie
extractStateFromCookie :: Maybe Text -> Maybe Text
extractStateFromCookie Nothing = Nothing
extractStateFromCookie (Just cookieHeader) =
  let cookies = parseCookies (encodeUtf8 cookieHeader)
   in decodeUtf8 <$> lookup oauthStateCookieName cookies

-- | Find existing user by email address stored in office365Id field
findUserByEmail :: AppState -> Text -> IO (Maybe User)
findUserByEmail appState email = do
  doc <- getDocument appState
  let o365Id = Office365Id email
  pure $ Ix.getOne $ doc.users Ix.@= o365Id

-- | Content Security Policy header value
-- Restricts script/style sources to prevent XSS attacks
cspHeaderValue :: Text
cspHeaderValue = T.intercalate "; "
  [ "default-src 'self'"
  , "script-src 'self' 'unsafe-inline'"  -- unsafe-inline needed for embedded JWT
  , "style-src 'self' 'unsafe-inline'"   -- unsafe-inline needed for inline styles
  , "connect-src 'self' ws: wss:"        -- Allow WebSocket connections
  , "img-src 'self' data:"               -- Allow data URIs for images
  , "font-src 'self'"
  , "frame-ancestors 'none'"             -- Prevent clickjacking
  , "base-uri 'self'"                    -- Prevent base tag injection
  , "form-action 'self'"                 -- Restrict form submissions
  ]

-- | Render frontend HTML with JWT and WASM hash embedded
renderFrontendHTML :: Text -> Text -> Html
renderFrontendHTML jwt wasmHash = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    -- Content Security Policy via meta tag
    -- Prevents XSS attacks by restricting script/style sources
    H.meta ! A.httpEquiv "Content-Security-Policy" ! A.content (H.toValue cspHeaderValue)
    H.title "Meine Mathe-Kompetenzen"
    -- Load Basecoat UI CSS first (provides component styles and default theme)
    H.link ! A.rel "stylesheet" ! A.href "/static/basecoat.cdn.min.css"
    -- Load our Tailwind CSS last (overrides Basecoat's CSS variables with our theme)
    H.link ! A.rel "stylesheet" ! A.href "/static/output.css"
    H.script $ H.toHtml $
      "// JWT token for WebSocket authentication\n\
      \window.COMPETENCES_JWT = '" <> jwt <> "';\n\
      \// WASM hash for cache busting\n\
      \window.COMPETENCES_WASM_HASH = '" <> wasmHash <> "';"
  H.body ! A.class_ "theme-claude" $ do
    -- Load application code
    H.script ! A.src "/static/index.js" ! A.type_ "module" $ ""
