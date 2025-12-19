{-# LANGUAGE CPP #-}

module Main (main) where

import Competences.Frontend.App (mkApp, runApp, withTailwindPlay)

#ifdef WASM

import Competences.Frontend.SyncDocument
  ( mkSyncDocument
  , mkSyncDocumentEnv
  , modifySyncDocument
  , setSyncDocument
  )
import Competences.Frontend.WebSocket
  ( getJWTToken
  , connectWebSocket
  )
import Language.Javascript.JSaddle.Wasm (run)
import Language.Javascript.JSaddle (jsg, valToText, (!))
import Competences.Command (Command(..), EntityCommand (..))
import Competences.Document (User(..), UserRole(..))
import Competences.Document.Id (nilId)
import Competences.Protocol (ServerMessage(..))
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T

main :: IO ()
main = do
  run $ do
    -- Get JWT token from window.COMPETENCES_JWT
    maybeToken <- getJWTToken
    case maybeToken of
      Nothing -> do
        liftIO $ putStrLn "ERROR: No JWT token found in window.COMPETENCES_JWT"
        -- Fallback: use test user
        let user = User nilId "Test User" Teacher Nothing
        env <- mkSyncDocumentEnv user
        document <- mkSyncDocument env
        modifySyncDocument document $ OnUsers (Create user)
        runApp $ withTailwindPlay $ mkApp document

      Just jwtToken -> do
        liftIO $ putStrLn $ "Found JWT token: " <> T.unpack (T.take 20 jwtToken) <> "..."

        -- Create temporary environment with placeholder user
        -- (we'll get the real user from InitialSnapshot)
        let tempUser = User nilId "Loading..." Student Nothing
        env <- mkSyncDocumentEnv tempUser
        document <- mkSyncDocument env

        -- Determine WebSocket URL from current location
        location <- jsg ("window" :: T.Text) ! ("location" :: T.Text)
        protocol <- location ! ("protocol" :: T.Text) >>= valToText
        host <- location ! ("host" :: T.Text) >>= valToText
        let wsProtocol = if T.isPrefixOf "https:" protocol then "wss://" else "ws://"
        let wsUrl = wsProtocol <> host <> "/"

        _ <- connectWebSocket wsUrl jwtToken $ \serverMsg -> do
          case serverMsg of
            InitialSnapshot doc -> do
              liftIO $ putStrLn "Received InitialSnapshot"
              -- Set the document to the initial snapshot
              setSyncDocument document doc

            ApplyCommand cmd -> do
              liftIO $ putStrLn $ "Received ApplyCommand: " <> show cmd
              -- TODO: Apply to remote document and replay local changes
              pure ()

            CommandRejected cmd err -> do
              liftIO $ putStrLn $ "Command rejected: " <> show cmd <> " - " <> T.unpack err
              -- TODO: Remove from local changes
              pure ()

            KeepAliveResponse -> do
              -- Acknowledge keep-alive
              pure ()

        -- Start the app
        runApp $ withTailwindPlay $ mkApp document

foreign export javascript "hs_start" main :: IO ()

#else

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
import Competences.Command (Command(..), EntityCommand (..))
import System.Random (newStdGen)

data Options = Options
  { port :: !Int
  -- , jwtToken :: !Text
  -- , translationsPath :: !FilePath
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
    -- <*> strOption
    --   ( long "jwt-token"
    --       <> short 't'
    --       <> help "JWT token to use for authentication"
    --       <> showDefault
    --       <> value "no-token"
    --       <> metavar "TOKEN"
    --   )
    -- <*> strOption
    --   ( long "translations"
    --       <> short 'l'
    --       <> help "Path to the translations file"
    --       <> showDefault
    --       <> value "res/translations-de.json"
    --       <> metavar "PATH"
    --   )
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
  let user = User opt.userId opt.userName opt.userRole Nothing
  env <- mkSyncDocumentEnv user
  bracket (readDocument env opt.inputDocumentPath) (writeDocument opt.outputDocumentPath) $ \document -> do
    run opt.port $ do
      modifySyncDocument document $ OnUsers (Create user)
      runApp $ withTailwindPlay $ mkApp document

readDocument :: SyncDocumentEnv -> Maybe FilePath -> IO SyncDocumentRef
readDocument e (Just p) = do
  f <- B.readFile p
  g <- newStdGen
  case eitherDecode f of
    Left err -> error $ "Could not read file " <> p <> ": " <> err
    Right d -> mkSyncDocument' e g d
readDocument e Nothing = mkSyncDocument e

writeDocument :: Maybe FilePath -> SyncDocumentRef -> IO ()
writeDocument (Just p) r = do
  local <- (.localDocument) <$> readSyncDocument r
  B.writeFile p $ encode local
writeDocument Nothing _ = pure ()
#endif
