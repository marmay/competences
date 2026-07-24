{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
  )
where

import Competences.Auth.ClientConfig (ClientConfig(..))
import Competences.Auth.ReplayProtection (ensureUnconsumed)
import Competences.Backend.Auth (generateJWT', toAuthUser)
import Competences.Backend.Exchange (exchangeFromYaml, exchangeToYaml)
import Competences.Backend.SecurityConfig (SecurityConfig(..))
import Competences.Exchange.Types (ExchangeDoc)
import Control.Monad.IO.Class (liftIO)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Aeson qualified as A
import GHC.Generics (Generic)
import Servant
  ( (:<|>) (..)
  , (:>)
  , Get
  , Handler
  , JSON
  , OctetStream
  , PlainText
  , Post
  , Proxy (..)
  , Raw
  , ReqBody
  , Server
  , ServerError (..)
  , err302
  , err400
  , err500
  , errBody
  , errHeaders
  , serveDirectoryWebApp
  , throwError, err403, CaptureAll
  )
import Servant.API (NoContent (..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Competences.Backend.Shell (ShellHashes, mkShellConfig, renderShell)
import Competences.Auth.Assertion (validateIdentityAssertion', IdentityAssertion(..))
import qualified Data.Text as T
import Competences.Backend.State (RestState(..))
import Competences.Query.User (findUserByOffice365Id)
import Competences.Document.User (Office365Id(..))
import Optics.Core ((&))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (unless)
import qualified Data.UUID as UUID

type AppAPI =
  -- Root redirect to /app/grid
  Get '[HTML] NoContent
    -- Exchange codec: ExchangeDoc (Binary) -> YAML.
    :<|> "api" :> "exchange" :> "encode"
           :> ReqBody '[OctetStream] BL.ByteString
           :> Post '[PlainText] Text
    -- Exchange codec: YAML (UTF-8 bytes) -> ExchangeDoc (Binary).
    -- 400 on parse error. We ship YAML as octet-stream rather than
    -- text/plain to dodge servant's strict charset matching against
    -- the browser-supplied Content-Type.
    :<|> "api" :> "exchange" :> "decode"
           :> ReqBody '[OctetStream] BL.ByteString
           :> Post '[OctetStream] BL.ByteString
    -- Receives a security assertion; returns {"jwt": ...}.
    -- Failures are JSON {"error": <code>, "message": ...} where the
    -- code is a contract with the shell bootstrap: "unknown-user"
    -- renders the no-account panel, everything else a generic
    -- failure panel with a retry link.
    :<|> "api" :> "login"
           :> ReqBody '[OctetStream] BL.ByteString
           :> Post '[JSON] LoginResponse
    -- Static files
    :<|> "static" :> Raw
    -- App catch-all - initiate OAuth with return URL preservation
    :<|> "app" :> CaptureAll "path" Text :> Get '[HTML] Html

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: SecurityConfig -> FilePath -> ShellHashes -> RestState -> Servant.Server AppAPI
server securityConfig staticDir hashes restState =
  rootRedirectHandler
    :<|> exchangeEncodeHandler
    :<|> exchangeDecodeHandler
    :<|> loginHandler securityConfig restState
    :<|> serveDirectoryWebApp staticDir
    :<|> appCatchAllHandler securityConfig hashes

-- | Redirect root "/" to "/app/grid"
rootRedirectHandler :: Handler NoContent
rootRedirectHandler =
  throwError err302 {errHeaders = [("Location", "/app/grid")]}

-- | Encode endpoint: deserialise a 'Binary' 'ExchangeDoc' and emit its
-- YAML rendering. Malformed input yields 400.
exchangeEncodeHandler :: BL.ByteString -> Handler Text
exchangeEncodeHandler body =
  case Bin.decodeOrFail body of
    Left (_, _, err) ->
      throwError err400 {errBody = TLE.encodeUtf8 (TL.pack ("Invalid ExchangeDoc binary: " <> err))}
    Right (_, _, xdoc :: ExchangeDoc) ->
      pure (exchangeToYaml xdoc)

-- | Decode endpoint: parse UTF-8-encoded YAML bytes into an
-- 'ExchangeDoc' and return its 'Binary' encoding. Parse failures
-- (including non-UTF-8 input) surface as 400 with a plain-text body.
exchangeDecodeHandler :: BL.ByteString -> Handler BL.ByteString
exchangeDecodeHandler body = do
  decodedBody <- TLE.decodeUtf8' body
                   & either handleDecodeError pure
  forExchange <- exchangeFromYaml (TL.toStrict decodedBody)
                   & either handleToExchangeError pure
  pure $ Bin.encode forExchange

  where
    handleDecodeError _ =
      throwError err400 {errBody = "request body is not valid UTF-8"}
    handleToExchangeError reason =
      throwError err400 {errBody = TLE.encodeUtf8 (TL.fromStrict reason)}

-- | Successful login response; the bootstrap reads the jwt field.
newtype LoginResponse = LoginResponse
  { jwt :: Text
  } deriving (Generic, Show)

instance A.ToJSON LoginResponse

loginHandler :: SecurityConfig -> RestState -> BL.ByteString -> Handler LoginResponse
loginHandler securityConfig restState inputToken = do
  let validateConfig = securityConfig.authClientConfig
  (validateResult, validUntil) <-
    liftIO (validateIdentityAssertion' validateConfig.authPublicKey validateConfig.allowedExpirySkewDuration validateConfig.origin inputToken) >>= either handleInvalidAssertion pure
  isUnconsumed <- liftIO (ensureUnconsumed validateResult.assertionId validUntil restState.consumedAssertionIds)
  unless isUnconsumed $
    handleAlreadyConsumedAssertion validateResult.assertionId
  doc <- liftIO (readTVarIO restState.document)
  user <- findUserByOffice365Id doc (Office365Id validateResult.office365Id) & maybe (handleUserNotFound validateResult.office365Id) pure
  liftIO (generateJWT' securityConfig.sessionIssuerJwk (toAuthUser user))
    >>= either handleMintingError (pure . LoginResponse . TL.toStrict . TLE.decodeUtf8)

  where
    handleInvalidAssertion jwtError =
      jsonError err403 "invalid-assertion" (T.pack $ "Could not validate input token: " <> show jwtError)
    handleAlreadyConsumedAssertion assertionId =
      jsonError err403 "invalid-assertion" ("Assertion " <> UUID.toText assertionId <> " has already been consumed.")
    handleUserNotFound o365Id =
      jsonError err403 "unknown-user" ("Could not find user with id '" <> o365Id <> "' from token.")
    handleMintingError jwtError =
      jsonError err500 "minting-failed" (T.pack $ "Internal error when minting the session token: " <> show jwtError)
    jsonError :: forall a. ServerError -> Text -> Text -> Handler a
    jsonError baseError code message =
      throwError baseError
        { errBody = A.encode $ A.object ["error" A..= code, "message" A..= message]
        , errHeaders = [("Content-Type", "application/json")]
        }

-- | Catch-all handler for /app/* routes: always serves the shell,
-- which acquires the session token client-side (see Backend.Shell).
appCatchAllHandler :: SecurityConfig -> ShellHashes -> [Text] -> Handler Html
appCatchAllHandler securityConfig frontendHashes _path = do
  shellConfig <- liftIO $ mkShellConfig frontendHashes securityConfig.authClientConfig.authBaseUrl
  pure $ renderShell shellConfig
