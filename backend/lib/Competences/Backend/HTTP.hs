{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
  )
where

import Marmay.Auth.ClientConfig (ClientConfig(..))
import Marmay.Auth.ReplayProtection (ensureUnconsumed)
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
import Marmay.Auth.Assertion (validateIdentityAssertion', IdentityAssertion(..))
import qualified Data.Text as T
import Competences.Backend.State (RestState(..))
import Competences.Backend.CommandProcessor qualified as CP
import Competences.Command (Command (Migration), MigrationCommand (BindEntraOid, CompleteUserIdentity))
import Competences.Command.Common (CommandContext (..))
import Competences.Document (Document (..), User (..))
import Competences.Document.Session (legacySessionId)
import Competences.Query.User (findUserByEntraOid, findUserByOffice365Id)
import Competences.Document.User (EntraOid (..), Office365Id (..))
import Control.Applicative ((<|>))
import Data.IxSet.Typed qualified as Ix
import Data.List (find)
import Optics.Core ((&))
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (unless, when)
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
  -- Identity resolution (lazy oid binding): the bound Entra oid is
  -- authoritative; an address match is only acceptable for a not yet
  -- bound user and binds the oid as a side effect. An address whose
  -- user is bound to a DIFFERENT oid is rejected — that account
  -- belongs to someone else.
  user <- case findUserByEntraOid doc (EntraOid validateResult.oid) of
    Just u -> do
      -- Self-completion for oid-provisioned stubs: fill the empty
      -- address and the placeholder name from the assertion, once.
      let stubName = u.name == "" || Just (EntraOid (T.toLower u.name)) == u.entraOid
      when (stubName || u.office365Id == Office365Id "") $ do
        completeResult <- liftIO $ CP.submitCommand
          restState.processor
          (CommandContext u.id legacySessionId)
          (Migration (CompleteUserIdentity u.id validateResult.upn validateResult.name))
        either (liftIO . putStrLn . ("CompleteUserIdentity failed: " <>) . T.unpack) (const (pure ())) completeResult
      pure u
    Nothing -> case findUserByAddress doc validateResult.upn of
      Nothing -> handleUserNotFound validateResult.upn
      Just u -> case u.entraOid of
        Just _ -> handleUserNotFound validateResult.upn
        Nothing -> do
          bindResult <- liftIO $ CP.submitCommand
            restState.processor
            (CommandContext u.id legacySessionId)
            (Migration (BindEntraOid u.id validateResult.oid))
          -- Best effort: a failed bind is retried on the next login.
          either (liftIO . putStrLn . ("BindEntraOid failed: " <>) . T.unpack) (const (pure ())) bindResult
          pure u
  liftIO (generateJWT' securityConfig.sessionIssuerJwk (toAuthUser user))
    >>= either handleMintingError (pure . LoginResponse . TL.toStrict . TLE.decodeUtf8)

  where
    -- Exact index lookup first; linear case-insensitive fallback for
    -- teacher-typed addresses (the assertion upn is lowercased).
    findUserByAddress doc upn =
      findUserByOffice365Id doc (Office365Id upn)
        <|> find (\u -> let Office365Id a = u.office365Id in a /= "" && T.toLower a == upn)
              (Ix.toList doc.users)
    handleInvalidAssertion jwtError =
      jsonError err403 "invalid-assertion" (T.pack $ "Could not validate input token: " <> show jwtError)
    handleAlreadyConsumedAssertion assertionId =
      jsonError err403 "invalid-assertion" ("Assertion " <> UUID.toText assertionId <> " has already been consumed.")
    handleUserNotFound upn =
      jsonError err403 "unknown-user" ("Could not find user with address '" <> upn <> "' from token.")
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
