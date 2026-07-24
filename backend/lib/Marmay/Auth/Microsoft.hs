module Marmay.Auth.Microsoft
  ( exchangeCodeForToken
  , getUserInfo
  , Office365User(..)
  ) where

import Marmay.Auth.OAuth2Config (OAuth2Config(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types as H
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
    
-- | Office365 user information from Microsoft Graph API
data Office365User = Office365User
  { o365Id :: !T.Text
  , displayName :: !T.Text
  , mail :: !(Maybe T.Text)
  , userPrincipalName :: !T.Text
  }
  deriving (Eq, Generic, Show)

instance A.FromJSON Office365User where
  parseJSON = A.withObject "Office365User" $ \v ->
    Office365User
      <$> v A..: "id"
      <*> v A..: "displayName"
      <*> v A..: "mail"
      <*> v A..: "userPrincipalName"

-- | Exchange authorization code for access token
exchangeCodeForToken :: Manager -> OAuth2Config -> T.Text -> IO (Either String T.Text)
exchangeCodeForToken tlsManager config code = do
  let tokenUrl = T.concat
        [ "https://login.microsoftonline.com/"
        , config.tenantId
        , "/oauth2/v2.0/token"
        ]

  request <- H.parseRequest $ T.unpack tokenUrl
  let body = T.concat
        [ "client_id=" <> config.clientId
        , "&client_secret=" <> config.clientSecret
        , "&code=" <> code
        , "&redirect_uri=" <> config.redirectUri
        , "&grant_type=authorization_code"
        ]

  let request' = request
        { H.requestHeaders = [(H.hContentType, "application/x-www-form-urlencoded")]
        , H.method = "POST"
        , H.requestBody = H.RequestBodyBS (T.encodeUtf8 body)
        }

  response <- H.httpLbs request' tlsManager

  case A.eitherDecode (H.responseBody response) of
    Left err -> pure $ Left $ "Failed to parse token response: " <> err
    Right (A.Object obj) -> case A.lookup "access_token" obj of
      Just (A.String token) -> pure $ Right token
      _ -> pure $ Left "No access_token in response"
    Right _ -> pure $ Left "Invalid token response format"

-- | Get user information from Microsoft Graph API
getUserInfo :: Manager -> T.Text -> IO (Either String Office365User)
getUserInfo tlsManager accessToken = do
  request <- H.parseRequest "https://graph.microsoft.com/v1.0/me"
  let request' = request
        { H.requestHeaders = [("Authorization", T.encodeUtf8 $ "Bearer " <> accessToken)]
        }

  response <- H.httpLbs request' tlsManager

  case A.eitherDecode (H.responseBody response) of
    Left err -> pure $ Left $ "Failed to parse user info: " <> err
    Right userInfo -> pure $ Right userInfo
