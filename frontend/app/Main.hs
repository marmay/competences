{-# LANGUAGE CPP #-}

module Main (main) where

import Competences.Frontend.App (mkApp, runApp, withTailwindPlay)

#ifdef WASM

import Competences.Frontend.App (run)
import Competences.Frontend.SyncDocument
  ( SyncDocument (..)
  , SyncDocumentRef
  , mkSyncDocument
  , mkSyncDocument'
  , readSyncDocument
  )
import Competences.Frontend.App.State (mkState)
import Competences.Frontend.Common.Translate (defaultTranslationData)
import Competences.Document (User(..))
import Competences.Document.Id (nilId)
import Competences.Document.User (UserRole(..))

main :: IO ()
main = do
  document <- mkSyncDocument
  run $ do
    app <- mkApp document $
      mkState
        (User nilId "Test User" Teacher)
        ("")
        defaultTranslationData
        42
    runApp app

foreign export javascript "hs_start" main :: IO ()

#else

import Competences.Document.Id (mkId, nilId)
import Competences.Document.User (User (..), UserId, UserRole (..))
import Competences.Frontend.App.State (mkState)
import Competences.Frontend.Common.Translate (loadTranslations)
import Competences.Frontend.SyncDocument
  ( SyncDocument (..)
  , SyncDocumentRef
  , mkSyncDocument
  , mkSyncDocument'
  , readSyncDocument
  )
import Control.Exception (bracket)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

import Language.Javascript.JSaddle.Warp (run)
import Options.Applicative

data Options = Options
  { port :: !Int
  , jwtToken :: !Text
  , translationsPath :: !FilePath
  , inputDocumentPath :: !(Maybe FilePath)
  , outputDocumentPath :: !(Maybe FilePath)
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
    <*> optional
      ( strOption
          ( long "input-document"
              <> short 'i'
              <> help "Path to the document file"
              <> metavar "INPUT_PATH"
          )
      )
    <*> optional
      ( strOption
          ( long "output-document"
              <> short 'o'
              <> help "Path to the document file"
              <> metavar "OUTPUT_PATH"
          )
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
  let user = User opt.userId opt.userName opt.userRole
  bracket (readDocument opt.inputDocumentPath) (writeDocument opt.outputDocumentPath) $ \document -> do
    run opt.port $ do
      let app = withTailwindPlay $ mkApp document user
      runApp app

readDocument :: Maybe FilePath -> IO SyncDocumentRef
readDocument (Just p) = do
  f <- B.readFile p
  case eitherDecode f of
    Left err -> error $ "Could not read file " <> p <> ": " <> err
    Right d -> mkSyncDocument' d
readDocument Nothing = mkSyncDocument

writeDocument :: Maybe FilePath -> SyncDocumentRef -> IO ()
writeDocument (Just p) r = do
  local <- (.localDocument) <$> readSyncDocument r
  B.writeFile p $ encode local
writeDocument Nothing _ = pure ()
#endif
