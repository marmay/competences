module Competences.Backend.API.Auth (AuthAPI, authAPI) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API (Get, QueryParam, (:>))

type AuthAPI =
  "o365"
    :> "redeem"
    :> QueryParam "code" Text
    :> Get '[] ()

authAPI :: Proxy AuthAPI
authAPI = Proxy
