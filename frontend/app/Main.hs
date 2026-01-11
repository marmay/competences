{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef WASM

import Competences.Command (Command (..), EntityCommand (..), UsersCommand (..))
import Competences.Document (User (..), UserRole (..), emptyDocument)
import Competences.Document.Id (nilId)
import Competences.Document.User (Office365Id (..))
import Competences.Frontend.App (mkApp, runApp, withTailwindPlay)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.SyncContext
  ( mkSyncDocument
  , mkSyncDocumentEnv
  , modifySyncDocument
  , setSyncDocument
  )
import Competences.Frontend.WebSocket (getJWTToken)
import Competences.Frontend.WebSocket.CommandSender (mkCommandSender)
import Competences.Frontend.WebSocket.Handlers (mkInitialHandler, mkReconnectHandler)
import Competences.Frontend.WebSocket.Protocol (AuthenticationException (..), withWebSocket)
import Control.Concurrent (forkIO)
import Control.Exception (catch)
import Control.Monad (void)
import Data.Text qualified as T
import Miso qualified as M
import Miso.DSL (jsg, fromJSVal, (!), setField)
import Miso.Run (run)

main :: IO ()
main = do
  M.consoleLog "App loaded."
  run $ do
    -- Get JWT token from window.COMPETENCES_JWT
    maybeToken <- getJWTToken
    case maybeToken of
      Nothing -> do
        M.consoleError "No JWT token found in window.COMPETENCES_JWT"
        -- Fallback: use test user with disconnected CommandSender
        let user = User nilId "Test User" Teacher (Office365Id "")
        sender <- mkCommandSender  -- Creates disconnected sender (commands won't send)
        env <- mkSyncDocumentEnv user sender
        ref <- mkSyncDocument env
        setSyncDocument ref emptyDocument
        modifySyncDocument ref $ Users $ OnUsers $ Create user
        runApp $ withTailwindPlay $ mkApp ref

      Just jwtToken -> do
        M.consoleLog $ M.ms $ "Found JWT token: " <> T.unpack (T.take 20 jwtToken) <> "..."

        -- Determine WebSocket URL from current location
        location <- jsg "window" ! "location"
        (Just protocol) <- location ! "protocol" >>= fromJSVal @T.Text
        (Just host) <- location ! "host" >>= fromJSVal @T.Text
        let wsProtocol = if T.isPrefixOf "https:" protocol then "wss://" else "ws://"
        let wsUrl = wsProtocol <> host <> "/"

        -- Fork action that starts the Miso app
        let forkApp ref = void $ forkIO $ do
              -- Set window title with localized text
              htmlDoc <- jsg "document"
              setField htmlDoc "title" (C.translate' C.LblPageTitle)
              runApp $ withTailwindPlay $ mkApp ref

        -- Connect and run with automatic reconnection
        M.consoleLog "Connecting to server..."
        let initial = mkInitialHandler jwtToken forkApp
            reconnect = mkReconnectHandler jwtToken

        withWebSocket wsUrl initial reconnect
          `catch` handleAuthFailure location

-- | Handle authentication failure by redirecting to login
handleAuthFailure :: M.JSVal -> AuthenticationException -> IO ()
handleAuthFailure location (AuthenticationException reason) = do
  M.consoleError $ M.ms $ "Authentication failed: " <> T.unpack reason
  setField location "href" ("/" :: T.Text)


foreign export javascript "hs_start" main :: IO ()

#else

-- import Competences.Document.Id (mkId, nilId)
-- import Competences.Document.User (User (..), UserId, UserRole (..), Office365Id (..))
-- import Competences.Frontend.SyncContext
--   ( SyncDocument (..)
--   , SyncDocumentRef
--   , mkSyncDocument
--   , mkSyncDocument'
--   , readSyncDocument, modifySyncDocument, SyncDocumentEnv, mkSyncDocumentEnv
--   )
-- import Control.Exception (bracket)
-- import Data.Aeson (eitherDecode, encode)
-- import Data.ByteString.Lazy qualified as B
-- import Data.Text (Text)
-- import Data.Text qualified as T
-- 
-- import Language.Javascript.JSaddle.Warp (run)
-- import Options.Applicative
-- import Competences.Command (Command(..), EntityCommand (..), UsersCommand (..))
-- import System.Random (newStdGen)
-- 
-- data Options = Options
--   { port :: !Int
--   -- , jwtToken :: !Text
--   -- , translationsPath :: !FilePath
--   , inputDocumentPath :: !(Maybe FilePath)
--   , outputDocumentPath :: !(Maybe FilePath)
--   , userId :: !UserId
--   , userName :: !Text
--   , userRole :: !UserRole
--   }
-- 
-- options :: Parser Options
-- options =
--   Options
--     <$> option
--       auto
--       ( long "port"
--           <> short 'p'
--           <> help "Port to run the server on"
--           <> showDefault
--           <> value 3000
--           <> metavar "PORT"
--       )
--     -- <*> strOption
--     --   ( long "jwt-token"
--     --       <> short 't'
--     --       <> help "JWT token to use for authentication"
--     --       <> showDefault
--     --       <> value "no-token"
--     --       <> metavar "TOKEN"
--     --   )
--     -- <*> strOption
--     --   ( long "translations"
--     --       <> short 'l'
--     --       <> help "Path to the translations file"
--     --       <> showDefault
--     --       <> value "res/translations-de.json"
--     --       <> metavar "PATH"
--     --   )
--     <*> optional
--       ( strOption
--           ( long "input-document"
--               <> short 'i'
--               <> help "Path to the document file"
--               <> metavar "INPUT_PATH"
--           )
--       )
--     <*> optional
--       ( strOption
--           ( long "output-document"
--               <> short 'o'
--               <> help "Path to the document file"
--               <> metavar "OUTPUT_PATH"
--           )
--       )
--     <*> option
--       (maybeReader $ mkId . T.pack)
--       ( long "user-id"
--           <> short 'u'
--           <> help "User ID to use for authentication"
--           <> showDefault
--           <> value nilId
--           <> metavar "USER_ID"
--       )
--     <*> strOption
--       ( long "user-name"
--           <> short 'n'
--           <> help "User name to use for authentication"
--           <> showDefault
--           <> value "Test User"
--           <> metavar "USER_NAME"
--       )
--     <*> option
--       auto
--       ( long "user-role"
--           <> short 'r'
--           <> help "User role to use for authentication"
--           <> showDefault
--           <> value Teacher
--           <> metavar "USER_ROLE"
--       )

main :: IO ()
main = do
  error "jsaddle has been removed. The frontend-only development server does not work right now."
  -- opt <- execParser $ info (options <**> helper) (fullDesc <> progDesc "Run the frontend server")
  -- let user = User opt.userId opt.userName opt.userRole (Office365Id "")
  -- env <- mkSyncDocumentEnv user
  -- bracket (readDocument env opt.inputDocumentPath) (writeDocument opt.outputDocumentPath) $ \document -> do
  --   run opt.port $ do
  --     modifySyncDocument document $ Users $ OnUsers $ Create user
  --     runApp $ withTailwindPlay $ mkApp document

-- readDocument :: SyncDocumentEnv -> Maybe FilePath -> IO SyncDocumentRef
-- readDocument e (Just p) = do
--   f <- B.readFile p
--   g <- newStdGen
--   case eitherDecode f of
--     Left err -> error $ "Could not read file " <> p <> ": " <> err
--     Right d -> mkSyncDocument' e g d
-- readDocument e Nothing = mkSyncDocument e
-- 
-- writeDocument :: Maybe FilePath -> SyncDocumentRef -> IO ()
-- writeDocument (Just p) r = do
--   local <- (.localDocument) <$> readSyncDocument r
--   B.writeFile p $ encode local
-- writeDocument Nothing _ = pure ()
#endif
