module Competences.Auth.SecurityConfig
  ( SecurityConfig (.. )
  , loadSecurityConfig
  )
  where

import GHC.Generics (Generic)
import Competences.Auth.OAuth2Config
import Data.Aeson (FromJSON)
import qualified Crypto.JOSE as JOSE
import Competences.Internal.SecurityConfig (forceLoadSecurityConfig)
import Data.Time (NominalDiffTime)

data SecurityConfig = SecurityConfig
  { oauth2Config :: !OAuth2Config
  , authIssuerJwk :: !JOSE.JWK
  , tokenExpiryDuration :: !NominalDiffTime
  }
  deriving (Generic, Show)

instance FromJSON SecurityConfig

loadSecurityConfig :: FilePath -> IO SecurityConfig
loadSecurityConfig = forceLoadSecurityConfig @SecurityConfig

