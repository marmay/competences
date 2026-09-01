-- | Configuration file handling for backend secrets
module Competences.Backend.SecurityConfig
  ( SecurityConfig (..)
  , loadSecurityConfig
  )
where

import Competences.Backend.Middleware (defaultTeamsFrameAncestors)
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Crypto.JOSE as JOSE
import Marmay.Auth.ConfigFile (forceLoadConfigFile)
import qualified Marmay.Auth.ClientConfig as Auth

-- | Security configuration of the backend.
--
-- This is loaded from a JSON file to keep secrets out of command-line arguments
-- (which are visible in process lists).
data SecurityConfig = SecurityConfig
  { sessionIssuerJwk :: !JOSE.JWK
  -- ^ Secret key for JWT token signing
  , authClientConfig :: !Auth.ClientConfig
  -- ^ Configuration as a client to the authentication service.
  , teamsFrameAncestors :: ![Text]
  -- ^ Origins allowed to iframe /app/* (Teams hosts). Optional;
  -- override only when Microsoft's hosting domains churn.
  }
  deriving (Generic, Show)

instance FromJSON SecurityConfig where
  -- Manual instance so existing config files keep parsing without the
  -- optional key.
  parseJSON = withObject "SecurityConfig" $ \o -> do
    sessionIssuerJwk <- o .: "sessionIssuerJwk"
    authClientConfig <- o .: "authClientConfig"
    teamsFrameAncestors <- o .:? "teamsFrameAncestors" .!= defaultTeamsFrameAncestors
    pure
      SecurityConfig
        { sessionIssuerJwk = sessionIssuerJwk
        , authClientConfig = authClientConfig
        , teamsFrameAncestors = teamsFrameAncestors
        }

-- | Load security configuration from JSON file
--
-- Exits with error message if file cannot be read or parsed.
loadSecurityConfig :: FilePath -> IO SecurityConfig
loadSecurityConfig = forceLoadConfigFile @SecurityConfig
