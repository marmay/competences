{-# LANGUAGE CPP #-}

module Main (main) where

import Competences.Frontend.App (mkApp, runApp, withTailwindPlay)

#ifdef WASM

import Competences.Frontend.SyncDocument
  ( mkSyncDocument
  , mkSyncDocumentEnv
  , modifySyncDocument
  , setSyncDocument
  , setWebSocket
  , applyRemoteCommand
  , rejectCommand
  )
import Competences.Frontend.WebSocket
  ( getJWTToken
  , connectWebSocket
  )
import Language.Javascript.JSaddle.Wasm (run)
import Language.Javascript.JSaddle (jsg, valToText, (!))
import Competences.Command (Command(..), EntityCommand (..), UsersCommand (..))
import Competences.Document (User(..), UserRole(..))
import Competences.Document.Id (nilId)
import Competences.Document.User (Office365Id(..))
import Competences.Protocol (ServerMessage(..))
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, tryTakeMVar, tryReadMVar)

main :: IO ()
main = do
  run $ do
    -- Get JWT token from window.COMPETENCES_JWT
    maybeToken <- getJWTToken
    case maybeToken of
      Nothing -> do
        liftIO $ putStrLn "ERROR: No JWT token found in window.COMPETENCES_JWT"
        -- Fallback: use test user
        let user = User nilId "Test User" Teacher (Office365Id "")
        env <- mkSyncDocumentEnv user
        document <- mkSyncDocument env
        modifySyncDocument document $ Users $ OnUsers $ Create user
        runApp $ withTailwindPlay $ mkApp document

      Just jwtToken -> do
        liftIO $ putStrLn $ "Found JWT token: " <> T.unpack (T.take 20 jwtToken) <> "..."

        -- Determine WebSocket URL from current location
        location <- jsg ("window" :: T.Text) ! ("location" :: T.Text)
        protocol <- location ! ("protocol" :: T.Text) >>= valToText
        host <- location ! ("host" :: T.Text) >>= valToText
        let wsProtocol = if T.isPrefixOf "https:" protocol then "wss://" else "ws://"
        let wsUrl = wsProtocol <> host <> "/"

        -- MVar that will be filled when we receive the first InitialSnapshot
        initialState <- liftIO newEmptyMVar

        ws <- connectWebSocket wsUrl jwtToken $ \serverMsg -> do
          case serverMsg of
            InitialSnapshot doc user -> do
              maybeState <- liftIO $ tryTakeMVar initialState

              case maybeState of
                Nothing -> do
                  -- First connection: create document with authenticated user and signal ready
                  liftIO $ putStrLn $ "Initial connection for user: " <> T.unpack user.name <> " (" <> show user.id <> ")"
                  env <- mkSyncDocumentEnv user
                  document <- mkSyncDocument env
                  setSyncDocument document doc
                  liftIO $ putMVar initialState (document, user)

                Just (existingDoc, existingUser) -> do
                  -- Reconnection: verify user matches and update document
                  liftIO $ putMVar initialState (existingDoc, existingUser)  -- Put it back
                  if user.id /= existingUser.id
                    then do
                      liftIO $ putStrLn $ "ERROR: User mismatch on reconnect! Expected "
                        <> T.unpack existingUser.name <> " (" <> show existingUser.id
                        <> ") but got " <> T.unpack user.name <> " (" <> show user.id <> ")"
                      -- This should never happen - indicates serious auth problem
                      -- TODO: Show error message to user and/or reload page
                    else do
                      liftIO $ putStrLn "Reconnection: updating document"
                      setSyncDocument existingDoc doc

            ApplyCommand cmd -> do
              maybeState <- liftIO $ tryReadMVar initialState
              case maybeState of
                Just (document, _) -> applyRemoteCommand document cmd
                Nothing -> liftIO $ putStrLn "WARNING: Received ApplyCommand before initialization"

            CommandRejected cmd err -> do
              liftIO $ putStrLn $ "Command rejected: " <> show cmd <> " - " <> T.unpack err
              maybeState <- liftIO $ tryReadMVar initialState
              case maybeState of
                Just (document, _) -> rejectCommand document cmd
                Nothing -> liftIO $ putStrLn "WARNING: Received CommandRejected before initialization"

            KeepAliveResponse -> do
              -- Acknowledge keep-alive
              pure ()

        -- Wait for first InitialSnapshot to arrive, then start app
        (document, user) <- liftIO $ readMVar initialState
        liftIO $ putStrLn $ "Starting app for user: " <> T.unpack user.name
        -- Set WebSocket connection after document is created
        setWebSocket document ws
        runApp $ withTailwindPlay $ mkApp document

foreign export javascript "hs_start" main :: IO ()

#else

import Competences.Document.Id (mkId, nilId)
import Competences.Document.User (User (..), UserId, UserRole (..), Office365Id (..))
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
import Competences.Command (Command(..), EntityCommand (..), UsersCommand (..))
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
  let user = User opt.userId opt.userName opt.userRole (Office365Id "")
  env <- mkSyncDocumentEnv user
  bracket (readDocument env opt.inputDocumentPath) (writeDocument opt.outputDocumentPath) $ \document -> do
    run opt.port $ do
      modifySyncDocument document $ Users $ OnUsers $ Create user
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
