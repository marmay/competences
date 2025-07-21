module Main (main) where

import Competences.Frontend.Common.Translate (loadTranslations)
import Competences.Frontend.Grid.App (mkApp, runApp)
import Competences.Frontend.Grid.State (mkState)
import Competences.Model.Id (mkId, nilId)
import Competences.Model.User (User (..), UserId, UserRole (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Language.Javascript.JSaddle.Warp (run)
import Options.Applicative

data Options = Options
  { port :: !Int
  , jwtToken :: !Text
  , translationsPath :: !FilePath
  , userId :: !UserId
  , userName :: !Text
  , userRole :: !UserRole
  , randomSeed :: !Int
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
    <*> option
      auto
      ( long "random-seed"
          <> short 's'
          <> help "Random seed to use for generating random data"
          <> showDefault
          <> value 42
          <> metavar "SEED"
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
          opt.randomSeed
