module Competences.Backend.Auth
  ( AuthUser( .. )
  , generateJWT
  , generateJWT'
  , validateJWT
  , validateJWT'
  , toAuthUser
  )
where

import Competences.Document (User (..))
import Competences.Document.Id (Id (..))
import Data.UUID.Types qualified as UUID
import Data.Aeson (FromJSON (..), ToJSON(..), Value (..), withObject, (.:))
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import GHC.Generics (Generic)
import qualified Crypto.JWT as JWT
import Control.Lens.Operators
import qualified Data.Aeson.KeyMap as AKV
import qualified Data.Set as Set
import Competences.Document.User (UserId, UserRole, Office365Id)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Time (MonadTime (..))
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- | Information about a user that goes into the main authentication token.
-- Equivalent to the full user for now, but conceptually, a projection of
-- the relevant parts to decide whether a user is granted authorization.
data AuthUser = AuthUser
  { id :: !UserId
  , name :: !Text
  , role :: !UserRole
  , office365Id :: !Office365Id
  } deriving (Eq, Generic, Show)

instance FromJSON AuthUser
instance ToJSON AuthUser

-- | Projects full user to token authentication user.
toAuthUser :: User -> AuthUser
toAuthUser user =
  AuthUser
  { id = user.id
  , name = user.name
  , role = user.role
  , office365Id = user.office365Id
  }

-- | Contents of the JWT; base JWT + and AuthUser object.
data UserClaims = UserClaims
  { jwtClaims :: !JWT.ClaimsSet
  , user :: !AuthUser
  } deriving (Eq, Show)

instance JWT.HasClaimsSet UserClaims where
  claimsSet f s = fmap (\cs -> s {jwtClaims = cs}) (JWT.claimsSet f s.jwtClaims)

instance FromJSON UserClaims where
  parseJSON = withObject "UserClaims" $ \o -> do
    jwtClaims <- parseJSON (Object o)
    user <- o .: "https://bu-ki.at/#user"
    pure $ UserClaims jwtClaims user

instance ToJSON UserClaims where
  toJSON s =
    toJSON s.jwtClaims `merge`
               [ ("https://bu-ki.at/#user", toJSON s.user)
               ]
    where
      merge :: Value -> [(AKV.Key, Value)] -> Value
      merge (Object o) kvs = Object $ o `AKV.union` AKV.fromList kvs
      merge x _ = x

-- | Generate a JWT token for a user
generateJWT :: JWT.JWK -> AuthUser -> IO (Either JWT.JWTError JWT.SignedJWT)
generateJWT key user = JWT.runJOSE $ generateJWT_ key user

generateJWT' :: JWT.JWK -> AuthUser -> IO (Either JWT.JWTError BL.ByteString)
generateJWT' key user = JWT.runJOSE $ do
  jwt <- generateJWT_ key user
  pure $ JWT.encodeCompact jwt

generateJWT_ :: (Monad m, MonadTime m, MonadError e m, JWT.AsError e, JWT.MonadRandom m) => JWT.JWK -> AuthUser -> m JWT.SignedJWT
generateJWT_ key user = do
  now <- currentTime
  let expiry = addUTCTime (24 * 60 * 60) now -- 24 hours

  let claims = JWT.emptyClaimsSet
        & JWT.claimIss ?~ "competences-backend"
        & JWT.claimAud ?~ JWT.Audience [JWT.string # "competences-backend"]
        & JWT.claimSub ?~ (JWT.string # UUID.toText user.id.unId)
        & JWT.claimExp ?~ (JWT.NumericDate expiry)
        & JWT.claimIat ?~ (JWT.NumericDate now)
  let userClaims = UserClaims claims user
  alg <- JWT.bestJWSAlg key
  JWT.signJWT key (JWT.newJWSHeader (JWT.RequiredProtection, alg)) userClaims

validateJWT :: JWT.JWK -> JWT.SignedJWT -> IO (Either JWT.JWTError AuthUser)
validateJWT key token = JWT.runJOSE $ validateJWT_ key token

validateJWT' :: JWT.JWK -> Text -> IO (Either JWT.JWTError AuthUser)
validateJWT' key encoded = JWT.runJOSE $ do
  token <- JWT.decodeCompact $ B.fromStrict $ T.encodeUtf8 $ encoded
  validateJWT_ key token
  
-- | Validate a JWT token
validateJWT_ :: (Monad m, MonadTime m, MonadError e m, JWT.AsError e, JWT.AsJWTError e) => JWT.JWK -> JWT.SignedJWT -> m AuthUser
validateJWT_ key token = do
  alg <- JWT.bestJWSAlg key   -- Enforce best algorithm supported by key, as we
                              -- are the signing party too.
  let validationSettings =
       JWT.defaultJWTValidationSettings (== "competences-backend")
       & JWT.jwtValidationSettingsIssuerPredicate .~ (== "competences-backend")
       & JWT.validationSettingsAlgorithms .~ Set.fromList [alg]
  (userClaims :: UserClaims) <- JWT.verifyJWT validationSettings key token
  pure $ userClaims.user
