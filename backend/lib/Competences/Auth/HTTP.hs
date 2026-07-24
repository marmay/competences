module Competences.Auth.HTTP
  ( authServer
  , authAPI
  ) where

import Servant (Get, (:<|>) (..), (:>), QueryParam, Header, Server, Handler, throwError, ServerError (..), err302, err400, err500)
import Data.Text (Text)
import Competences.Auth.SecurityConfig (SecurityConfig(..))
import Control.Monad (unless)
import qualified Data.UUID.V4 as UUID
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.UUID as UUID
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.HTTP.Types (urlEncode, urlDecode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Competences.Auth.OAuth2Config (OAuth2Config(..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import Network.URI (parseAbsoluteURI, URI (..), URIAuth (..), uriToString)
import qualified Data.Text as T
import Web.Cookie (parseCookies, Cookies)
import Competences.Auth.Microsoft (exchangeCodeForToken, getUserInfo, Office365User(..))
import Network.HTTP.Client (Manager)
import Competences.Auth.Assertion (IdentityAssertion(..), generateIdentityAssertion')
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
  
type AuthAPI =
  "auth" :>
  ( "login"
         :> QueryParam "return" Text
         :> Get '[HTML] Html
  :<|> "callback"
         :> QueryParam "code" Text
         :> QueryParam "state" Text
         :> Header "Cookie" Text
         :> Get '[HTML] Html
  )

authAPI :: Proxy AuthAPI
authAPI = Proxy

authServer :: Manager -> SecurityConfig -> Server AuthAPI
authServer tlsManager securityConfig = (loginHandler securityConfig) :<|> (callbackHandler tlsManager securityConfig)

loginHandler :: SecurityConfig -> Maybe Text -> Handler Html
loginHandler securityConfig returnUrl = do
  returnUrl' <- maybe handleMissingReturnUrl pure returnUrl
                >>= pure . parseAbsoluteURI . T.unpack
                >>= maybe handleReturnUrlInvalid pure
  unless (isAllowedReturnUrl securityConfig.laxReturnUrlCheck securityConfig.allowedReturnDomain returnUrl') $
    handleDisallowedReturnUrl
  csrfState <- liftIO UUID.nextRandom
  let
    locationHeader = ("Location", getAuthorizationUrlWithState securityConfig.oauth2Config (UUID.toText csrfState))
    csrfStateCookie = mkCookie "csrfState" (UUID.toText csrfState)
    returnUrlCookie = mkCookie "returnUrl" (T.pack (uriToString id returnUrl' ""))

  throwError err302
               { errHeaders = [ locationHeader, csrfStateCookie, returnUrlCookie ] }

  where
    handleMissingReturnUrl =
      throwError err400 {errBody = "Missing return URL"}
    handleReturnUrlInvalid =
      throwError err400 {errBody = "Invalid return URL"}
    handleDisallowedReturnUrl =
      throwError err400 {errBody = "Disallowed return URL"}
    mkCookie name value =
      ("Set-Cookie", B.concat [name, "=", urlEncode False (encodeUtf8 value), "; HttpOnly; Secure; SameSite=Lax; Path=/auth/callback; Max-Age=600"])

callbackHandler :: Manager -> SecurityConfig -> Maybe Text -> Maybe Text -> Maybe Text -> Handler Html
callbackHandler tlsManager securityConfig code state cookies = do
  code' <- maybe handleMissingCode pure code
  state' <- maybe handleMissingState pure state
  (csrfState, returnUrl) <- maybe handleMissingCookies pure cookies
                              >>= parseAllCookies
  returnUrl' <- maybe (handleInvalidReturnUrl returnUrl) pure
                  $ parseAbsoluteURI $ T.unpack $ returnUrl
  unless (csrfState == state') $
    handleNonMatchingCsrfState state' csrfState
  unless (isAllowedReturnUrl securityConfig.laxReturnUrlCheck securityConfig.allowedReturnDomain returnUrl') $
    handleDisallowedReturnUrl
  token <- liftIO (exchangeCodeForToken tlsManager securityConfig.oauth2Config code')
             >>= either handleTokenExchangeError pure
  userInfo <- liftIO (getUserInfo tlsManager token)
             >>= either handleTokenExchangeError pure
  identityAssertion <- liftIO (mkIdentityAssertion userInfo)
  let mintedTokenAudience = returnUrl' { uriPath = "", uriQuery = "", uriFragment = "" }
  mintedToken <- liftIO (generateIdentityAssertion' securityConfig.authIssuerJwk securityConfig.tokenExpiryDuration mintedTokenAudience identityAssertion)
    >>= either handleMintingError pure
  throwError err302 {
    errHeaders = [ ("Location", B.concat [ encodeUtf8 returnUrl
                                         , "#itoken="
                                         , B.toStrict mintedToken
                                         ])
                 , clearCookie "csrfState"
                 , clearCookie "returnUrl"
                 ]
    }
                 
  where
    handleMissingCode =
      throwError err400 {errBody = "Missing code"}
    handleMissingState =
      throwError err400 {errBody = "Missing state"}
    handleInvalidReturnUrl returnUrl =
      throwError err400 {errBody = B.fromStrict $ B.concat
                          [ "Invalid return URL: "
                          , encodeUtf8 returnUrl
                          ]}
    handleDisallowedReturnUrl =
      throwError err400 {errBody = "Disallowed return URL"}
    handleMissingCookies =
      throwError err400 {errBody = "Missing cookies"}
    handleNonMatchingCsrfState fromAssertingParty fromCookie =
      throwError err400 {errBody = B.fromStrict $ B.concat
                          [ "CSRF state does not match; received from asserting party: "
                          , encodeUtf8 fromAssertingParty
                          , "; cookie value: "
                          , encodeUtf8 fromCookie
                          ]}
    handleTokenExchangeError err =
      throwError err500 {errBody = B.fromStrict $ B.concat
                          [ "Token exchange failed: "
                          , encodeUtf8 $ T.pack err
                          ]}
    handleMintingError err =
      throwError err500 {errBody = B.fromStrict $ B.concat
                          [ "Minting failed: "
                          , encodeUtf8 $ T.pack $ show err
                          ]}
    parseAllCookies cs = do
      let parsed = parseCookies (encodeUtf8 cs)
      csrfState <- readCookie parsed "csrfState"
      returnUrl <- readCookie parsed "returnUrl"
      pure (csrfState, returnUrl)
    readCookie :: Cookies -> ByteString -> Handler Text
    readCookie cs n = do
      case lookup n cs of
        Just v -> pure $ decodeUtf8 $ urlDecode False v
        Nothing -> throwError err400 {errBody = "Missing cookie: " <> B.fromStrict n}
    clearCookie n = ("Set-Cookie", B.concat [n, "=; HttpOnly; Secure; SameSite=Lax; Path=/auth/callback; Max-Age=0"])
    mkIdentityAssertion office365User = do
      assertionId <- UUID.nextRandom
      pure IdentityAssertion
        { assertionId = assertionId
        , name = office365User.displayName
        , office365Id = fromMaybe office365User.userPrincipalName office365User.mail 
        }

isAllowedReturnUrl :: Bool -> Text -> URI -> Bool
isAllowedReturnUrl laxReturnUrlCheck allowedPattern returnUrl =
     (uriScheme returnUrl == "https:"
      || (laxReturnUrlCheck && uriScheme returnUrl == "http:"))
  && null (uriFragment returnUrl)
  && maybe False isAllowedUriAuthority (uriAuthority returnUrl)
  where
    isAllowedUriAuthority auth =
         null (uriUserInfo auth)
      && (laxReturnUrlCheck || null (uriPort auth))
      && isAllowedHost (T.pack (uriRegName auth))
    isAllowedHost host =
         allowedPattern == host
      || ("." <> allowedPattern) `T.isSuffixOf` host
      
getAuthorizationUrlWithState :: OAuth2Config -> Text -> ByteString
getAuthorizationUrlWithState config state =
  B.concat
    [ "https://login.microsoftonline.com/"
    , encodeUtf8 config.tenantId
    , "/oauth2/v2.0/authorize?"
    , "client_id="
    , urlEncode False (encodeUtf8 config.clientId)
    , "&response_type=code"
    , "&redirect_uri="
    , urlEncode False (encodeUtf8 config.redirectUri)
    , "&response_mode=query"
    , "&scope=openid%20profile%20email%20User.Read"
    , "&state="
    , urlEncode False (encodeUtf8 state)
    ]
