module Competences.API.Auth () where

type AuthAPI =
  "o365"
    :> ( ( "login"
             :> Get '[HTML] ()
         )
           :<|> ( "redeem"
                    :> QueryParam "code" Text
                    :> Get '[HTML] ()
                )
       )

authAPI :: Proxy AuthAPI
authAPI = Proxy
