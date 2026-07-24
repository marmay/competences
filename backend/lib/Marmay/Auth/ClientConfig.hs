module Marmay.Auth.ClientConfig
  ( ClientConfig(..)
  )
  where

import qualified Crypto.JOSE as JOSE
import qualified Crypto.JOSE.Types as JOSE
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

data ClientConfig = ClientConfig
  { authPublicKey :: !JOSE.JWK
  -- ^ Public key of the authentication service.
  , allowedExpirySkewDuration :: !NominalDiffTime
  -- ^ Since JWTs from the auth service are minted on a different
  -- system, we allow for a small skew in clocks when validating
  -- the expiry time of the token.
  , origin :: !JOSE.URI
  -- ^ Origin of the instance; used to check whether a security
  -- token is for us.
  , authBaseUrl :: !(Maybe Text)
  -- ^ Base URL of the authentication service (no trailing slash);
  -- the shell bootstrap redirects to <authBaseUrl>/auth/login when
  -- no valid session token is available. Nothing preserves the
  -- disconnected dev mode: the app starts without a token and never
  -- redirects.
  } deriving (Eq, Show, Generic)

instance FromJSON ClientConfig
