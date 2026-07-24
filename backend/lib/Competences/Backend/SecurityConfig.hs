-- | Configuration file handling for backend secrets
module Competences.Backend.SecurityConfig
  ( SecurityConfig (..)
  , loadSecurityConfig
  )
where

import Data.Aeson (FromJSON)
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
  }
  deriving (Generic, Show)

instance FromJSON SecurityConfig

-- | Load security configuration from JSON file
--
-- Exits with error message if file cannot be read or parsed.
loadSecurityConfig :: FilePath -> IO SecurityConfig
loadSecurityConfig = forceLoadConfigFile @SecurityConfig
