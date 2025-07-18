module Main (main) where

import Competences.Frontend.App (mkApp, runApp)
import Competences.Frontend.State (mkState)
import Competences.Frontend.Translate (loadTranslations)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Language.Javascript.JSaddle.Warp (run)
import Options.Applicative
import Competences.Model.User (UserRole (..), User (..), UserId)
import Competences.Model.Id (mkId, nilId)
import qualified Data.Text as T

data Options = Options
  { port :: !Int
  , jwtToken :: !Text
  , translationsPath :: !FilePath
  , userId :: !UserId
  , userName :: !Text
  , userRole :: !UserRole
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port to run the server on"
          <> showDefault
          <> value 3000
          <> metavar "PORT"
      )
    <*> strOption
      ( long "jwt-token"
          <> short 't'
          <> help "JWT token to use for authentication"
          <> showDefault
          <> value "no-token"
          <> metavar "TOKEN"
      )
    <*> strOption
      ( long "translations"
          <> short 'l'
          <> help "Path to the translations file"
          <> showDefault
          <> value "res/translations-de.json"
          <> metavar "PATH"
      )
    <*> option
      (maybeReader $ mkId . T.pack)
      ( long "user-id"
          <> short 'u'
          <> help "User ID to use for authentication"
          <> showDefault
          <> value nilId
          <> metavar "USER_ID"
      )
    <*> strOption
      ( long "user-name"
          <> short 'n'
          <> help "User name to use for authentication"
          <> showDefault
          <> value "Test User"
          <> metavar "USER_NAME"
      )
    <*> option
      auto
      ( long "user-role"
          <> short 'r'
          <> help "User role to use for authentication"
          <> showDefault
          <> value Teacher
          <> metavar "USER_ROLE"
      )

main :: IO ()
main = do
  opt <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Run the frontend server")
  translationData <- loadTranslations opt.translationsPath
  run opt.port $
    runApp $
      mkApp $
        mkState
          (User opt.userId opt.userName opt.userRole)
          (encodeUtf8 opt.jwtToken)
          translationData
