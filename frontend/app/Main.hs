{-# LANGUAGE CPP #-}

module Main (main, startServer) where

import Competences.Frontend.App (mkApp, runApp, withTailwindPlay)




























import Competences.Document.Id (mkId, nilId)
import Competences.Document.User (User (..), UserId, UserRole (..))
import Competences.Frontend.SyncDocument
  ( SyncDocument (..)
  , SyncDocumentRef
  , mkSyncDocument
  , mkSyncDocument'
  , readSyncDocument, modifySyncDocument, SyncDocumentEnv, mkSyncDocumentEnv
  )
import Control.Exception (bracket)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as B
import Data.Text (Text)
import Data.Text qualified as T

import Language.Javascript.JSaddle.Warp (run)
import Options.Applicative
import Competences.Command (Command(..))
import Control.Concurrent (MVar, ThreadId, newEmptyMVar, tryTakeMVar, killThread, forkIO, putMVar)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen)

data Options = Options
  { port :: !Int
  , jwtToken :: !Text
  , translationsPath :: !FilePath
  , inputDocumentPath :: !(Maybe FilePath)
  , outputDocumentPath :: !(Maybe FilePath)
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

main :: IO ()
main = do
  opt <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Run the frontend server")
  let user = User opt.userId opt.userName opt.userRole
  env <- mkSyncDocumentEnv user
  bracket (readDocument env opt.inputDocumentPath) (writeDocument opt.outputDocumentPath) $ \document -> do
    run opt.port $ do
      modifySyncDocument document $ AddUser user
      runApp $ withTailwindPlay $ mkApp document

readDocument :: SyncDocumentEnv -> Maybe FilePath -> IO SyncDocumentRef
readDocument e (Just p) = do
  f <- B.readFile p
  case eitherDecode f of
    Left err -> error $ "Could not read file " <> p <> ": " <> err
    Right d -> mkSyncDocument' e (mkStdGen 42) d
readDocument e Nothing = mkSyncDocument e

writeDocument :: Maybe FilePath -> SyncDocumentRef -> IO ()
writeDocument (Just p) r = do
  local <- (.localDocument) <$> readSyncDocument r
  B.writeFile p $ encode local
writeDocument Nothing _ = pure ()

currentThread :: MVar ThreadId
currentThread = unsafePerformIO newEmptyMVar
{-# NOINLINE currentThread #-}

startServer :: IO ()
startServer = do
  tryTakeMVar currentThread >>= mapM_ killThread
  tid <- forkIO main
  putMVar currentThread tid
