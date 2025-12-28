module Competences.Backend.Auth
  ( OAuth2Config (..)
  , Office365User (..)
  , JWTSecret (..)
  , getAuthorizationUrl
  , exchangeCodeForToken
  , getUserInfo
  , generateJWT
  , validateJWT
  , extractUserFromJWT
  )
where

import Competences.Document (User (..), UserId)
import Competences.Document.Id (Id (..), mkId)
import Competences.Document.User (Office365Id (..), UserRole (..))
import Data.UUID.Types qualified as UUID
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request, RequestBody (..), httpLbs, method, parseRequest, requestBody, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType)
import Web.JWT qualified as JWT
import Web.JWT (stringOrURIToText)

-- | OAuth2 configuration for Office365
data OAuth2Config = OAuth2Config
  { clientId :: !Text
  , clientSecret :: !Text
  , redirectUri :: !Text
  , tenantId :: !Text
  }
  deriving (Eq, Show)

-- | Office365 user information from Microsoft Graph API
data Office365User = Office365User
  { o365Id :: !Text
  , displayName :: !Text
  , mail :: !(Maybe Text)
  , userPrincipalName :: !Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Office365User where
  parseJSON = withObject "Office365User" $ \v ->
    Office365User
      <$> v .: "id"
      <*> v .: "displayName"
      <*> v .: "mail"
      <*> v .: "userPrincipalName"

-- | JWT secret for signing tokens
newtype JWTSecret = JWTSecret Text
  deriving (Eq, Show)

-- | Get the authorization URL to redirect users to
getAuthorizationUrl :: OAuth2Config -> Text
getAuthorizationUrl config =
  T.concat
    [ "https://login.microsoftonline.com/"
    , config.tenantId
    , "/oauth2/v2.0/authorize?"
    , "client_id=" <> config.clientId
    , "&response_type=code"
    , "&redirect_uri=" <> config.redirectUri
    , "&response_mode=query"
    , "&scope=openid%20profile%20email%20User.Read"
    ]

-- | Exchange authorization code for access token
exchangeCodeForToken :: OAuth2Config -> Text -> IO (Either String Text)
exchangeCodeForToken config code = do
  manager <- newTlsManager

  let tokenUrl = T.concat
        [ "https://login.microsoftonline.com/"
        , config.tenantId
        , "/oauth2/v2.0/token"
        ]

  request <- parseRequest $ T.unpack tokenUrl
  let body = T.concat
        [ "client_id=" <> config.clientId
        , "&client_secret=" <> config.clientSecret
        , "&code=" <> code
        , "&redirect_uri=" <> config.redirectUri
        , "&grant_type=authorization_code"
        ]

  let request' = request
        { requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
        , method = "POST"
        , requestBody = RequestBodyBS (encodeUtf8 body)
        }

  response <- httpLbs request' manager

  case eitherDecode (responseBody response) of
    Left err -> pure $ Left $ "Failed to parse token response: " <> err
    Right (Object obj) -> case KM.lookup "access_token" obj of
      Just (String token) -> pure $ Right token
      _ -> pure $ Left "No access_token in response"
    Right _ -> pure $ Left "Invalid token response format"

-- | Get user information from Microsoft Graph API
getUserInfo :: Text -> IO (Either String Office365User)
getUserInfo accessToken = do
  manager <- newTlsManager

  request <- parseRequest "https://graph.microsoft.com/v1.0/me"
  let request' = request
        { requestHeaders = [("Authorization", encodeUtf8 $ "Bearer " <> accessToken)]
        }

  response <- httpLbs request' manager

  case eitherDecode (responseBody response) of
    Left err -> pure $ Left $ "Failed to parse user info: " <> err
    Right userInfo -> pure $ Right userInfo

-- | Generate a JWT token for a user
generateJWT :: JWTSecret -> User -> IO Text
generateJWT (JWTSecret secret) user = do
  now <- getCurrentTime
  let expiry = addUTCTime (24 * 60 * 60) now -- 24 hours

  let claims = JWT.JWTClaimsSet
        { JWT.iss = JWT.stringOrURI "competences-backend"
        , JWT.sub = JWT.stringOrURI $ UUID.toText user.id.unId
        , JWT.aud = Nothing
        , JWT.exp = JWT.numericDate $ utcTimeToPOSIXSeconds expiry
        , JWT.nbf = Nothing
        , JWT.iat = JWT.numericDate $ utcTimeToPOSIXSeconds now
        , JWT.jti = Nothing
        , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList
            [ ("name", String user.name)
            , ("role", String $ T.pack $ show user.role)
            , ("o365Id", let Office365Id oid = user.office365Id in String oid)
            ]
        }

  let signer = JWT.hmacSecret secret
  pure $ JWT.encodeSigned signer mempty claims

-- | Validate a JWT token
validateJWT :: JWTSecret -> Text -> Either String JWT.JWTClaimsSet
validateJWT (JWTSecret secret) token =
  case JWT.decodeAndVerifySignature (JWT.toVerify $ JWT.hmacSecret secret) token of
    Nothing -> Left "Invalid JWT signature"
    Just jwt -> Right $ JWT.claims jwt

-- | Extract user information from validated JWT claims
extractUserFromJWT :: JWT.JWTClaimsSet -> Either String (UserId, Text, UserRole, Office365Id)
extractUserFromJWT claims = do
  -- Extract subject (user ID)
  subText <- case JWT.sub claims of
    Nothing -> Left "Missing subject in JWT"
    Just uri -> Right $ stringOrURIToText uri

  userId <- case mkId subText of
    Nothing -> Left $ "Invalid user ID in JWT: " <> T.unpack subText
    Just uid -> Right uid

  -- Extract custom claims
  let customClaims = JWT.unClaimsMap $ JWT.unregisteredClaims claims

  name <- case Map.lookup "name" customClaims of
    Just (String n) -> Right n
    _ -> Left "Missing or invalid name in JWT"

  role <- case Map.lookup "role" customClaims of
    Just (String r) -> case reads (T.unpack r) of
      [(role, "")] -> Right role
      _ -> Left "Invalid role in JWT"
    _ -> Left "Missing or invalid role in JWT"

  o365Id <- case Map.lookup "o365Id" customClaims of
    Just (String oid) -> Right $ Office365Id oid
    Just Null -> Right $ Office365Id ""
    Nothing -> Right $ Office365Id ""
    _ -> Left "Invalid o365Id in JWT"

  pure (userId, name, role, o365Id)
