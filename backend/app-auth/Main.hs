module Main
  ( main
  ) where

import qualified Options.Applicative as Opt
import Competences.Auth.SecurityConfig (loadSecurityConfig, SecurityConfig (laxReturnUrlCheck))
import Competences.Auth.HTTP (authServer, authAPI)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Control.Monad (when)

data Options = Options
  { port :: !Int
  , securityConfigPath :: !FilePath
  } deriving (Eq, Show)

optionsParser :: Opt.ParserInfo Options
optionsParser =
  let options = Options
                <$> Opt.option
                    Opt.auto
                    ( Opt.long "port"
                      <> Opt.short 'p'
                      <> Opt.metavar "PORT"
                      <> Opt.help "TCP port the authentication server will listen on." )
                <*> Opt.strOption
                    ( Opt.long "config"
                      <> Opt.short 'c'
                      <> Opt.metavar "CONFIG"
                      <> Opt.help "Configuration file (JSON) containing secrets." )
  in 
    Opt.info (options Opt.<**> Opt.helper)
      (Opt.fullDesc
        <> Opt.progDesc "Competences Authentication Server"
        <> Opt.header "competences-auth - Authentication server for competences-backend."
      )

main :: IO ()
main = do
  opts <- Opt.execParser optionsParser

  putStrLn $ "Loading security configuration from: " <> opts.securityConfigPath
  securityConfig <- loadSecurityConfig opts.securityConfigPath

  when securityConfig.laxReturnUrlCheck $ do
    putStrLn "WARNING: Lax Return URL check is enabled. This is for development only!"

  putStrLn $ "Starting to listen on port " <> show opts.port <> "."
  tlsManager <- newTlsManager
  run opts.port $
    serve authAPI (authServer tlsManager securityConfig)

