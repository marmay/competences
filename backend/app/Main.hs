module Main where

import Competences.Backend.Auth (JWTSecret (..), OAuth2Config (..))
import Competences.Backend.HTTP (appAPI, server)
import Competences.Backend.State (AppState, loadAppState, saveAppState)
import Competences.Backend.WebSocket (wsHandler)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (finally)
import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  -- Parse command line arguments
  let usage = "Usage: competences-backend <port> <data-file> <jwt-secret> <client-id> <client-secret> <redirect-uri> <tenant-id> <static-dir>"
  args <- getArgs
  (port, dataPath, jwtSecret, oauth2Config, staticDir) <- case args of
    [portStr, path, secret, clientId, clientSecret, redirectUri, tenantId, static] -> case reads portStr of
      [(p, "")] -> pure
        ( p
        , path
        , JWTSecret $ T.pack secret
        , OAuth2Config
            { clientId = T.pack clientId
            , clientSecret = T.pack clientSecret
            , redirectUri = T.pack redirectUri
            , tenantId = T.pack tenantId
            }
        , static
        )
      _ -> die usage
    _ -> die usage

  putStrLn $ "Competences Backend Server"
  putStrLn $ "=========================="
  putStrLn $ "Port: " <> show port
  putStrLn $ "Data file: " <> dataPath
  putStrLn ""

  -- Load or initialize application state
  putStrLn "Loading application state..."
  state <- loadAppState dataPath

  -- Set up graceful shutdown
  shutdown <- newEmptyMVar

  -- Save state periodically and on shutdown
  _ <- forkIO $ saveLoop state dataPath shutdown

  putStrLn $ "Starting WebSocket server on port " <> show port
  putStrLn "Press Ctrl+C to stop"
  hFlush stdout

  -- Run server with graceful shutdown
  flip finally (gracefulShutdown state dataPath shutdown) $ do
    let httpApp = serve appAPI (server state oauth2Config jwtSecret staticDir)
    run port $ websocketsOr
      defaultConnectionOptions
      (wsHandler state dataPath jwtSecret)
      httpApp

-- | Save state periodically (every 60 seconds)
saveLoop :: AppState -> FilePath -> MVar () -> IO ()
saveLoop state dataPath _shutdown = go
  where
    go = do
      threadDelay 60000000 -- 60 seconds
      putStrLn "Auto-saving application state..."
      saveAppState dataPath state
      go

-- | Graceful shutdown: save state one last time
gracefulShutdown :: AppState -> FilePath -> MVar () -> IO ()
gracefulShutdown state dataPath shutdown = do
  putStrLn "\nShutting down..."
  putStrLn "Saving application state..."
  saveAppState dataPath state
  putStrLn "Goodbye!"
  putMVar shutdown ()
