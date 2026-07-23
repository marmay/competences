module Competences.Auth.Assertion
  ( generateIdentityAssertion
  , generateIdentityAssertion'
  , validateIdentityAssertion
  , validateIdentityAssertion'
  , IdentityAssertion(..)
  ) where

import Data.Text (Text)
import qualified Crypto.JOSE as JOSE
import qualified Crypto.JWT as JOSE
import Control.Lens (_Just)
import Control.Lens.Operators
import Data.Time (addUTCTime, NominalDiffTime, UTCTime)
import qualified Data.Text as T
import Control.Monad.Time (MonadTime (..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (throwError)
import qualified Data.UUID as UUID
import qualified Crypto.JOSE.Types as JOSE
import qualified Data.Set as Set
import Data.Aeson (FromJSON (..), withObject, (.:), Value (..), ToJSON (..))
import qualified Data.Aeson.KeyMap as AKV
import Data.ByteString.Lazy (ByteString)

data IdentityAssertion = IdentityAssertion
  { assertionId :: !UUID.UUID
  , name :: !Text
  , office365Id :: !Text
  } deriving (Eq, Show)

data JOSEAssertion = JOSEAssertion
  { jwtClaims :: !JOSE.ClaimsSet
  , userName :: !Text
  } deriving (Eq, Show)

instance JOSE.HasClaimsSet JOSEAssertion where
  claimsSet f s = fmap (\cs -> s{ jwtClaims = cs}) (JOSE.claimsSet f s.jwtClaims)
instance FromJSON JOSEAssertion where
  parseJSON = withObject "JOSEAssertion" $ \o -> do
    jwtClaims <- parseJSON (Object o)
    userName <- o .: "https://auth.bu-ki.at/#userName"
    pure $ JOSEAssertion jwtClaims userName
instance ToJSON JOSEAssertion where
  toJSON s =
    toJSON s.jwtClaims `merge`
             [ ("https://auth.bu-ki.at/#userName", toJSON s.userName)
             ]
    where
      merge :: Value -> [(AKV.Key, Value)] -> Value
      merge (Object o) kvs = Object $ o `AKV.union` AKV.fromList kvs
      merge x _ = x

mkJOSEAssertion :: JOSE.ClaimsSet -> IdentityAssertion -> JOSEAssertion
mkJOSEAssertion claimsSet identityAssertion =
  let fullClaimsSet = claimsSet
       & JOSE.claimSub ?~ JOSE.string # identityAssertion.office365Id
       & JOSE.claimJti ?~ T.pack (show identityAssertion.assertionId)
  in JOSEAssertion{ jwtClaims = fullClaimsSet
                  , userName = identityAssertion.name
                  }

fromJOSEAssertion :: forall m e. (Monad m, MonadError e m, JOSE.AsJWTError e) => JOSEAssertion -> m IdentityAssertion
fromJOSEAssertion JOSEAssertion{ jwtClaims, userName } = do
  assertionId <- need "jti" $ jwtClaims ^? JOSE.claimJti . _Just >>= UUID.fromText
  office365Id <- need "sub" $ jwtClaims ^? (JOSE.claimSub . _Just . JOSE.string)
  pure $ IdentityAssertion{ assertionId = assertionId, name = userName, office365Id = office365Id }

generateIdentityAssertion :: JOSE.JWK -> NominalDiffTime -> JOSE.URI -> IdentityAssertion -> IO (Either JOSE.JWTError JOSE.SignedJWT)
generateIdentityAssertion key expiryDuration uri assertion = JOSE.runJOSE $ generateIdentityAssertion_ key expiryDuration uri assertion

generateIdentityAssertion' :: JOSE.JWK -> NominalDiffTime -> JOSE.URI -> IdentityAssertion -> IO (Either JOSE.JWTError ByteString)
generateIdentityAssertion' key expiryDuration uri assertion = JOSE.runJOSE $ do
  jwt <- generateIdentityAssertion_ key expiryDuration uri assertion
  pure $ JOSE.encodeCompact jwt

generateIdentityAssertion_ :: (Monad m, MonadTime m, MonadError e m, JOSE.AsError e, JOSE.AsJWTError e, JOSE.MonadRandom m) => JOSE.JWK -> NominalDiffTime -> JOSE.URI -> IdentityAssertion -> m JOSE.SignedJWT
generateIdentityAssertion_ jwk expiryDuration audience a = do
  now <- currentTime
  let expiry = addUTCTime expiryDuration now
  let baseClaims = JOSE.emptyClaimsSet
       & JOSE.claimIss ?~ "competences-auth"
       & JOSE.claimAud ?~ JOSE.Audience [JOSE.uri # audience]
       & JOSE.claimExp ?~ JOSE.NumericDate expiry
       & JOSE.claimIat ?~ JOSE.NumericDate now
  alg <- JOSE.bestJWSAlg jwk
  JOSE.signJWT jwk (JOSE.newJWSHeader (JOSE.RequiredProtection, alg))
    $ mkJOSEAssertion baseClaims a

validateIdentityAssertion :: JOSE.JWK -> NominalDiffTime -> JOSE.URI -> JOSE.SignedJWT -> IO (Either JOSE.JWTError (IdentityAssertion, UTCTime))
validateIdentityAssertion key allowedExpirySkewDuration uri token = JOSE.runJOSE $ validateIdentityAssertion_ key allowedExpirySkewDuration uri token

validateIdentityAssertion' :: JOSE.JWK -> NominalDiffTime -> JOSE.URI -> ByteString -> IO (Either JOSE.JWTError (IdentityAssertion, UTCTime))
validateIdentityAssertion' key allowedExpirySkewDuration uri encoded = JOSE.runJOSE $ do
  token <- JOSE.decodeCompact encoded
  validateIdentityAssertion_ key allowedExpirySkewDuration uri token

validateIdentityAssertion_ :: forall m e. (Monad m, MonadTime m, MonadError e m, JOSE.AsError e, JOSE.AsJWTError e) => JOSE.JWK -> NominalDiffTime -> JOSE.URI -> JOSE.SignedJWT -> m (IdentityAssertion, UTCTime)
validateIdentityAssertion_ jwk allowedExpirySkewDuration expectedAudience token = do
  alg <- JOSE.bestJWSAlg jwk
  let validationSettings =
        JOSE.defaultJWTValidationSettings
          (== JOSE.uri # expectedAudience)
          & JOSE.jwtValidationSettingsIssuerPredicate .~ (== "competences-auth")
          & JOSE.jwtValidationSettingsAllowedSkew .~ allowedExpirySkewDuration
          & JOSE.validationSettingsAlgorithms .~ Set.fromList [alg]
  joseAssertion <- JOSE.verifyJWT validationSettings jwk token
  identityAssertion <- fromJOSEAssertion joseAssertion
  expiryTime <- need "Tokens must have an expiry time!"
                  $ joseAssertion ^? JOSE.claimsSet . JOSE.claimExp . _Just
  pure (identityAssertion, fromNumericDate expiryTime)
  where
    fromNumericDate (JOSE.NumericDate t) =
      addUTCTime allowedExpirySkewDuration t

need :: forall m e a. (Monad m, MonadError e m, JOSE.AsJWTError e) => String -> Maybe a -> m a
need _ (Just a) = pure a
need e _ = throwError $ JOSE._JWTClaimsSetDecodeError @e # ("Invalid claim field '" <> e <> "'!")
        
