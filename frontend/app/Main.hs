{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Competences.Frontend.App (mkApp, runApp)
import Competences.Frontend.State (mkState)
import Competences.Frontend.Translate (loadTranslations)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Language.Javascript.JSaddle.Warp (run)
import Options.Applicative

data Options = Options
  { port :: !Int
  , jwtToken :: !Text
  , translationsPath :: !FilePath
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

main :: IO ()
main = do
  opt <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Run the frontend server")
  translationData <- loadTranslations opt.translationsPath
  run opt.port $ runApp $ mkApp $ mkState (encodeUtf8 opt.jwtToken) translationData
