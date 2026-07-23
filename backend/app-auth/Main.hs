module Main
  ( main
  ) where

import qualified Options.Applicative as Opt

data Options = Options
  { port :: !Int
  , config :: !FilePath
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
  _opts <- Opt.execParser optionsParser
  pure ()

