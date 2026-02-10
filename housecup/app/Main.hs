module Main (main) where

import Competences.HouseCup.Config (resolveHouseConfig)
import Competences.HouseCup.Database (loadDocumentAt)
import Competences.HouseCup.Scoring (ScoreTable, computePoints)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (Day)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Options.Applicative

data Options = Options
  { database :: !String
  , startDate :: !Day
  , endDate :: !Day
  , configFile :: !FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "database"
          <> short 'd'
          <> metavar "CONNSTR"
          <> help "PostgreSQL connection string"
      )
    <*> option auto
      ( long "start-date"
          <> short 's'
          <> metavar "YYYY-MM-DD"
          <> help "Start date (inclusive)"
      )
    <*> option auto
      ( long "end-date"
          <> short 'e'
          <> metavar "YYYY-MM-DD"
          <> help "End date (inclusive)"
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "Path to house config JSON file"
      )

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper) (fullDesc <> progDesc "House Cup scoring CLI")

  -- Read & parse house config
  configBytes <- BS.readFile opts.configFile
  houseConfig <- case Aeson.eitherDecodeStrict' configBytes of
    Left err -> fail $ "Failed to parse house config: " <> err
    Right cfg -> pure cfg

  -- Connect to database
  conn <- connectPostgreSQL (BS.pack opts.database)

  -- Reconstruct documents at both points in time
  TIO.putStrLn $ "Loading document at " <> T.pack (show opts.startDate) <> "..."
  docBefore <- loadDocumentAt conn opts.startDate

  TIO.putStrLn $ "Loading document at " <> T.pack (show opts.endDate) <> "..."
  docAfter <- loadDocumentAt conn opts.endDate

  -- Resolve house config against end-date document
  resolvedConfig <- resolveHouseConfig docAfter houseConfig

  -- Compute scores
  let scores = computePoints resolvedConfig docBefore docAfter

  -- Print results
  TIO.putStrLn ""
  TIO.putStrLn "House Cup Scores"
  TIO.putStrLn "================"
  printScoreTable (sortOn (Down . snd) scores)

printScoreTable :: ScoreTable -> IO ()
printScoreTable [] = TIO.putStrLn "(no houses configured)"
printScoreTable scores = do
  let maxNameLen = maximum $ map (T.length . fst) scores
  mapM_ (printRow maxNameLen) scores
  where
    printRow nameLen (name, points) = do
      let padding = T.replicate (nameLen - T.length name) " "
      TIO.putStrLn $ name <> padding <> "  " <> T.pack (show points)
