-- | Permission-checked JSON config loading, shared by the auth
-- service and its consumers. Refuses secret files readable by
-- others and config directories writable by others.
module Competences.Auth.ConfigFile
  ( forceLoadConfigFile
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Bits ((.&.))
import System.FilePath (takeDirectory)
import System.Posix.Files (getFileStatus, fileMode)
import System.Posix.Types (FileMode)
import System.Exit (die)

forceLoadConfigFile :: forall a. FromJSON a => FilePath -> IO a
forceLoadConfigFile path = do
  ensureSecretFileMode path
  ensureSecretDirMode $ takeDirectory path
  (loadResult :: Either String a) <- eitherDecodeFileStrict path
  case loadResult of
    Left err -> die $ "Failed to parse security config file "
                       <> path <> ": "<> err
    Right cfg -> pure cfg

ensureSecretFileMode :: FilePath -> IO ()
ensureSecretFileMode path = do
  mode <- getFileMode path
  when (mode .&. 0o077 /= 0) $ do
    die $ "Security config file " <> path
          <> " must not be accessible by others"

ensureSecretDirMode :: FilePath -> IO ()
ensureSecretDirMode path = do
  mode <- getFileMode path
  when (mode .&. 0o022 /= 0) $ do
    die $ "Security config directory " <> path
          <> " must not be writable by others!"

getFileMode :: FilePath -> IO FileMode
getFileMode p = fileMode <$> getFileStatus p
