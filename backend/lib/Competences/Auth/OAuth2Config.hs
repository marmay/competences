module Competences.Auth.OAuth2Config
  ( OAuth2Config( .. )
  , getAuthorizationUrl
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Text as T

-- | OAuth2 configuration from file
data OAuth2Config = OAuth2Config
  { clientId :: !Text
  , clientSecret :: !Text
  , redirectUri :: !Text
  , tenantId :: !Text
  }
  deriving (Generic, Show)

instance FromJSON OAuth2Config

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
