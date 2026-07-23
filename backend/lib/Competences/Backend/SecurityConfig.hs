-- | Configuration file handling for backend secrets
module Competences.Backend.SecurityConfig
  ( SecurityConfig (..)
  , loadSecurityConfig
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import qualified Crypto.JOSE as JOSE
import Competences.Internal.SecurityConfig (forceLoadSecurityConfig)
import qualified Crypto.JOSE.Types as JOSE
import Data.Time (NominalDiffTime)

-- | Security configuration of the backend.
--
-- This is loaded from a JSON file to keep secrets out of command-line arguments
-- (which are visible in process lists).
data SecurityConfig = SecurityConfig
  { sessionIssuerJwk :: !JOSE.JWK
  -- ^ Secret key for JWT token signing
  , authPublicKey :: !JOSE.JWK
  -- ^ Public key of the authentication service.
  , allowedExpirySkewDuration :: !NominalDiffTime
  -- ^ Since JWTs from the auth service are minted on a different
  -- system, we allow for a small skew in clocks when validating
  -- the expiry time of the token.
  , origin :: !JOSE.URI
  -- ^ Origin of the instance; used to check whether a security
  -- token is for us.
  }
  deriving (Generic, Show)

instance FromJSON SecurityConfig

-- | Load security configuration from JSON file
--
-- Exits with error message if file cannot be read or parsed.
loadSecurityConfig :: FilePath -> IO SecurityConfig
loadSecurityConfig = forceLoadSecurityConfig @SecurityConfig
