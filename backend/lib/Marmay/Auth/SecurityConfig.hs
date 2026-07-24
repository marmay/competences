{-# LANGUAGE RecordWildCards #-}

module Marmay.Auth.SecurityConfig
  ( SecurityConfig (.. )
  , loadSecurityConfig
  )
  where

import GHC.Generics (Generic)
import Marmay.Auth.OAuth2Config
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import qualified Crypto.JOSE as JOSE
import Marmay.Auth.ConfigFile (forceLoadConfigFile)
import Data.Time (NominalDiffTime)
import Data.Text (Text)

data SecurityConfig = SecurityConfig
  { oauth2Config :: !OAuth2Config
  , authIssuerJwk :: !JOSE.JWK
  , allowedReturnDomain :: !Text
  , tokenExpiryDuration :: !NominalDiffTime
  , laxReturnUrlCheck :: !Bool
  }
  deriving (Generic, Show)

instance FromJSON SecurityConfig where
  -- Manual parseJSON with default values for tokenExpiryDuration and laxReturnUrlCheck:
  parseJSON = withObject "SecurityConfig" $ \o -> do
    oauth2Config <- o .: "oauth2Config"
    authIssuerJwk <- o .: "authIssuerJwk"
    allowedReturnDomain <- o .: "allowedReturnDomain"
    tokenExpiryDuration <- o .:? "tokenExpiryDuration" .!= 60
    laxReturnUrlCheck <- o .:? "laxReturnUrlCheck" .!= False
    pure SecurityConfig {..}

loadSecurityConfig :: FilePath -> IO SecurityConfig
loadSecurityConfig = forceLoadConfigFile @SecurityConfig
