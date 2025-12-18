{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Competences.Backend.HTTP
  ( AppAPI
  , appAPI
  , server
  )
where

import Competences.Backend.Auth
  ( JWTSecret
  , OAuth2Config
  , Office365User (..)
  , exchangeCodeForToken
  , generateJWT
  , getAuthorizationUrl
  , getUserInfo
  )
import Competences.Backend.State (AppState, getDocument, updateDocument)
import Competences.Command (Command (..), EntityCommand (..))
import Competences.Document (Document (..),User (..))
import Competences.Document.Id (mkId)
import Competences.Document.User (Office365Id (..), UserRole (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as BL
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Servant
  ( (:<|>) (..)
  , (:>)
  , Get
  , Handler
  , Header
  , Headers
  , Proxy (..)
  , QueryParam
  , Raw
  , Server
  , ServerError (..)
  , err302
  , err400
  , err500
  , errHeaders
  , serveDirectoryWebApp
  , throwError
  )
import Servant.HTML.Blaze (HTML)
import Servant.API (NoContent (..))
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

type AppAPI =
  -- OAuth initiation - redirect to Office365
  Get '[HTML] (Headers '[Header "Location" Text] NoContent)
    -- OAuth callback - exchange code for token and serve frontend
    :<|> "oauth" :> "callback" :> QueryParam "code" Text :> Get '[HTML] Html
    -- Static files
    :<|> "static" :> Raw

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: AppState -> OAuth2Config -> JWTSecret -> FilePath -> Servant.Server AppAPI
server state oauth2Config jwtSecret staticDir =
  oauthInitHandler oauth2Config
    :<|> oauthCallbackHandler state oauth2Config jwtSecret
    :<|> serveDirectoryWebApp staticDir

-- | Redirect to Office365 login
oauthInitHandler :: OAuth2Config -> Handler (Headers '[Header "Location" Text] NoContent)
oauthInitHandler config = do
  let authUrl = getAuthorizationUrl config
  throwError err302 {errHeaders = [("Location", encodeUtf8 authUrl)]}

-- | OAuth callback - exchange code for token and serve frontend with JWT
oauthCallbackHandler :: AppState -> OAuth2Config -> JWTSecret -> Maybe Text -> Handler Html
oauthCallbackHandler state oauth2Config jwtSecret maybeCode = do
  code <- case maybeCode of
    Nothing -> throwError err400 {errBody = "Missing authorization code"}
    Just c -> pure c

  -- Exchange code for access token
  tokenResult <- liftIO $ exchangeCodeForToken oauth2Config code
  accessToken <- case tokenResult of
    Left err -> throwError err500 {errBody = BL.fromStrict $ encodeUtf8 $ T.pack err}
    Right token -> pure token

  -- Get user info from Microsoft Graph
  userInfoResult <- liftIO $ getUserInfo accessToken
  o365User <- case userInfoResult of
    Left err -> throwError err500 {errBody = BL.fromStrict $ encodeUtf8 $ T.pack err}
    Right info -> pure info

  -- Find or create user in document
  user <- liftIO $ findOrCreateUser state o365User

  -- Generate JWT
  jwt <- liftIO $ generateJWT jwtSecret user

  -- Serve frontend HTML with JWT embedded
  pure $ renderFrontendHTML jwt

-- | Find existing user by Office365 ID or create new one
findOrCreateUser :: AppState -> Office365User -> IO User
findOrCreateUser state o365User = do
  doc <- getDocument state
  let o365Id = Office365Id o365User.o365Id
  let existingUser = Ix.getOne $ doc.users Ix.@= (Just o365Id)

  case existingUser of
    Just user -> pure user
    Nothing -> do
      -- Create new user
      let userId = case mkId o365User.o365Id of
            Just uid -> uid
            Nothing -> error $ "Failed to create user ID from Office365 ID: " <> T.unpack o365User.o365Id

      let newUser = User
            { id = userId
            , name = o365User.displayName
            , role = Student -- Default role, can be changed manually
            , office365Id = Just o365Id
            }

      -- Add user to document
      result <- updateDocument state userId (OnUsers $ Create newUser)
      case result of
        Left err -> error $ "Failed to create user: " <> T.unpack err
        Right _ -> pure newUser

-- | Render frontend HTML with JWT embedded
renderFrontendHTML :: Text -> Html
renderFrontendHTML jwt = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.title "Competences"
    H.script $ H.toHtml $
      "// JWT token for WebSocket authentication\n\
      \window.COMPETENCES_JWT = '" <> jwt <> "';"
  H.body $
    H.script ! A.src "/static/index.js" ! A.type_ "module" $ ""
