module Main where

import Competences.Backend.State (AppState, initAppState, loadAppState, saveAppState)
import Competences.Backend.WebSocket (wsHandler)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Data.ByteString.Lazy.Char8 qualified as BL
import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  -- Parse command line arguments
  args <- getArgs
  (port, dataPath) <- case args of
    [portStr, path] -> case reads portStr of
      [(p, "")] -> pure (p, path)
      _ -> die "Usage: competences-backend <port> <data-file>"
    _ -> die "Usage: competences-backend <port> <data-file>"

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
    run port $ websocketsOr
      defaultConnectionOptions
      (wsHandler state dataPath)
      notFoundApp

-- | Save state periodically (every 60 seconds)
saveLoop :: AppState -> FilePath -> MVar () -> IO ()
saveLoop state dataPath shutdown = go
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

-- | Fallback application for non-WebSocket requests
notFoundApp :: Application
notFoundApp _req respond =
  respond $ responseLBS status400 [] (BL.pack "This server only supports WebSocket connections")
