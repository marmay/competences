{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
  )
where

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
import Servant
  ( (:<|>) (..)
  , (:>)
  , Get
  , Handler
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
  , throwError, err403
  )
import Servant.API (NoContent (..))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Competences.Backend.Shell (ShellHashes, mkShellConfig, renderShell)
import qualified Data.Text.Encoding as T
import Competences.Auth.Assertion (validateIdentityAssertion', IdentityAssertion(..))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Competences.Backend.State (RestState(..), ensureUnconsumed)
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
    -- Receives a security assertion; returns a JWT
    -- todo: types shall encode this!
    :<|> "api" :> "login"
           :> ReqBody '[OctetStream] BL.ByteString
           :> Post '[OctetStream] BL.ByteString
    -- Static files
    :<|> "static" :> Raw
    -- App catch-all - initiate OAuth with return URL preservation
    :<|> "app" :> Get '[HTML] Html

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: SecurityConfig -> FilePath -> ShellHashes -> RestState -> Servant.Server AppAPI
server securityConfig staticDir hashes restState =
  rootRedirectHandler
    :<|> exchangeEncodeHandler
    :<|> exchangeDecodeHandler
    :<|> loginHandler securityConfig restState
    :<|> serveDirectoryWebApp staticDir
    :<|> appCatchAllHandler hashes

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

loginHandler :: SecurityConfig -> RestState -> BL.ByteString -> Handler BL.ByteString
loginHandler securityConfig restState inputToken = do
  (validateResult, validUntil) <-
    liftIO (validateIdentityAssertion' securityConfig.authPublicKey securityConfig.allowedExpirySkewDuration securityConfig.origin inputToken) >>= either handleInvalidAssertion pure
  isUnconsumed <- liftIO (ensureUnconsumed validateResult.assertionId validUntil restState.consumedAssertionIds)
  unless isUnconsumed $
    handleAlreadyConsumedAssertion validateResult.assertionId
  doc <- liftIO (readTVarIO restState.document)
  user <- findUserByOffice365Id doc (Office365Id validateResult.office365Id) & maybe (handleUserNotFound validateResult.office365Id) pure
  liftIO (generateJWT' securityConfig.sessionIssuerJwk (toAuthUser user))
    >>= either handleMintingError pure

  where
    handleInvalidAssertion jwtError = 
      throwError err403 {errBody = B.fromStrict $ T.encodeUtf8 $ T.pack $ "Could not validate input token: " <> show jwtError}
    handleAlreadyConsumedAssertion assertionId =
      throwError err403 {errBody = B.fromStrict $ T.encodeUtf8 $ "Assertion " <> UUID.toText assertionId <> " has already been consumed."}
    handleUserNotFound o365Id =
      throwError err403 {errBody = B.fromStrict $ T.encodeUtf8 $ "Could not find user with id '" <> o365Id <> "' from token."}
    handleMintingError jwtError =
      throwError err500 {errBody = B.fromStrict $ T.encodeUtf8 $ T.pack $ "Internal error when minting the session token: " <> show jwtError}

-- | Catch-all handler for /app/* routes
-- Saves the requested URL in a cookie and redirects to Office365 OAuth
appCatchAllHandler :: ShellHashes -> Handler Html
appCatchAllHandler frontendHashes = do
  shellConfig <- liftIO $ mkShellConfig frontendHashes
  pure $ renderShell shellConfig
