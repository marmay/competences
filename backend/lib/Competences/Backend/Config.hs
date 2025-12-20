-- | Configuration file handling for backend secrets
module Competences.Backend.Config
  ( Config (..)
  , OAuth2ConfigFile (..)
  , loadConfig
  )
where

import Competences.Backend.Auth (JWTSecret (..), OAuth2Config (..))
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (die)

-- | Backend configuration containing secrets
--
-- This is loaded from a JSON file to keep secrets out of command-line arguments
-- (which are visible in process lists).
data Config = Config
  { jwtSecret :: !Text
  -- ^ Secret key for JWT token signing
  , oauth2 :: !OAuth2ConfigFile
  -- ^ Office365 OAuth2 configuration
  }
  deriving (Generic, Show)

instance FromJSON Config

-- | OAuth2 configuration from file
data OAuth2ConfigFile = OAuth2ConfigFile
  { clientId :: !Text
  , clientSecret :: !Text
  , redirectUri :: !Text
  , tenantId :: !Text
  }
  deriving (Generic, Show)

instance FromJSON OAuth2ConfigFile

-- | Load configuration from JSON file
--
-- Exits with error message if file cannot be read or parsed.
loadConfig :: FilePath -> IO (JWTSecret, OAuth2Config)
loadConfig path = do
  result <- eitherDecodeFileStrict path :: IO (Either String Config)
  case result of
    Left err -> die $ "Failed to parse config file " <> path <> ": " <> err
    Right cfg ->
      pure
        ( JWTSecret cfg.jwtSecret
        , OAuth2Config
            { clientId = cfg.oauth2.clientId
            , clientSecret = cfg.oauth2.clientSecret
            , redirectUri = cfg.oauth2.redirectUri
            , tenantId = cfg.oauth2.tenantId
            }
        )
