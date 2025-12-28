{-# LANGUAGE OverloadedLabels #-}

module Main
  ( main
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Evidence (..)
  , Level (..)
  , Order
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Observation (..)
  , ObservationId
  , SocialForm (..)
  )
import Competences.Document.Order (orderPos)
import Competences.Document.User (User (..), UserId)
import Control.Exception (Exception, catch, throwIO)
import Control.Exception.Base (throw)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.Extra (foldM, replicateM, unless, whenM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Attoparsec.Text qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.Csv
  ( DecodeOptions (..)
  , FromNamedRecord (..)
  , ToNamedRecord (..)
  , decodeByNameWith
  , defaultDecodeOptions
  , encodeByName
  , namedRecord
  , (.:)
  , (.=)
  )
import Data.Either.Extra (maybeToEither)
import Data.Foldable (maximumBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Optics.Core ((%~), (&))
import Options.Applicative qualified as O
import System.Directory.Extra (doesFileExist)
import System.Random (randomIO)
import System.Random.Stateful (globalStdGen, randomM)

data Options = Options
  { csvInput :: !FilePath
  , csvOutput :: !FilePath
  , documentInput :: !FilePath
  , documentOutput :: !FilePath
  }
  deriving (Eq, Show)

options :: O.Parser Options
options =
  Options
    <$> O.strOption
      ( O.long "csv-input"
          <> O.short 'c'
          <> O.help "Path to the CSV file."
          <> O.metavar "CSV_PATH"
      )
    <*> O.strOption
      ( O.long "csv-failed"
          <> O.short 'f'
          <> O.help "Path to the CSV file the records that could not be handled."
          <> O.metavar "CSV_PATH"
      )
    <*> O.strOption
      ( O.long "document-input"
          <> O.short 'd'
          <> O.help "Path to the input document file."
          <> O.metavar "DOCUMENT_PATH"
      )
    <*> O.strOption
      ( O.long "document-output"
          <> O.short 'o'
          <> O.help "Path to the output document file."
          <> O.metavar "COMMANDS_PATH"
      )

data CsvCommand = CsvCommand
  { date :: !Text
  , student :: !Text
  , exercises :: !Text
  , competences :: !Text
  }
  deriving (Eq, Show)

instance FromNamedRecord CsvCommand where
  parseNamedRecord r =
    CsvCommand
      <$> r .: "date"
      <*> r .: "student"
      <*> r .: "exercises"
      <*> r .: "competences"

instance ToNamedRecord CsvCommand where
  toNamedRecord (CsvCommand date student exercises competences) =
    namedRecord
      [ "date" .= date
      , "student" .= student
      , "exercises" .= exercises
      , "competences" .= competences
      ]

newtype OutputFileExists = OutputFileExists FilePath
  deriving (Eq, Show)

instance Exception OutputFileExists

data ParseError = ParseError !FilePath !String
  deriving (Eq, Show)

instance Exception ParseError

warnExistingFile :: FilePath -> IO ()
warnExistingFile p =
  whenM (doesFileExist p) $ do
    putStrLn $ "Warning: " <> p <> " already exists."
    putStrLn "Do you want to continue? (y/n)"
    answer <- getLine
    unless (answer == "y") $ throwIO $ OutputFileExists p

throwLeft :: (Exception e) => (a -> e) -> Either a b -> b
throwLeft f = either (throw . f) id

main :: IO ()
main = handleExceptions $ do
  opts <-
    O.execParser $
      O.info (options O.<**> O.helper) (O.fullDesc <> O.progDesc "Create commands from CSV data.")

  warnExistingFile opts.documentOutput
  warnExistingFile opts.csvOutput
  (header, csv) <-
    throwLeft (ParseError opts.csvInput)
      . decodeByNameWith (defaultDecodeOptions {decDelimiter = fromIntegral (ord ';')})
      <$> BL.readFile opts.csvInput
  document :: Document <-
    throwLeft (ParseError opts.documentInput) . eitherDecode <$> BL.readFile opts.documentInput

  (document', failedCommands) <- foldM handleCsvCommand (document, []) csv

  BL.writeFile opts.documentOutput $ encode document'
  BL.writeFile opts.csvOutput $ encodeByName header failedCommands

  pure ()
  where
    handleExceptions :: IO () -> IO ()
    handleExceptions a =
      a
        `catch` (\(OutputFileExists p) -> putStrLn $ "Error: " <> p <> " already exists.")
        `catch` (\(ParseError p e) -> putStrLn $ "Error: " <> p <> " could not be parsed: " <> e)

handleCsvCommand :: (Document, [CsvCommand]) -> CsvCommand -> IO (Document, [CsvCommand])
handleCsvCommand (document, failed) command = do
  h <- runExceptT $ handleCsvCommand' document command
  case h of
    Right document' -> do
      -- putStrLn $ "Successfully applied " <> show command <> "."
      pure (document', failed)
    Left err -> do
      putStrLn $ "Failed to apply " <> show command <> ": " <> err
      pure (document, command : failed)

handleCsvCommand' :: Document -> CsvCommand -> ExceptT String IO Document
handleCsvCommand' document command = do
  day :: Day <-
    liftEither $
      maybeToEither "Could not parse date!" $
        parseTimeM True defaultTimeLocale "%-d.%-m.%Y" (T.unpack command.date)
  studentId <- liftEither $ matchStudent document command.student
  competences <- liftEither $ mapM (parseCompetence document) (T.words command.competences)
  let activityTasks = command.exercises
  foldM (addCompetences studentId day activityTasks) document (groupByActivityType competences)

addCompetences
  :: UserId
  -> Day
  -> Text
  -> Document
  -> (ActivityType, [(CompetenceLevelId, SocialForm, Ability)])
  -> ExceptT String IO Document
addCompetences userId day activityTasks document (activityType, observationData) = do
  observationIds <- liftIO $ replicateM (length observationData) $ randomM globalStdGen
  let observations = zipWith mkObservation observationIds observationData
  evidenceId <- liftIO randomIO
  let evidence =
        Evidence
          { id = evidenceId
          , userIds = Set.fromList [userId]
          , activityType = activityType
          , date = day
          , oldTasks = activityTasks
          , tasks = []
          , observations = Ix.fromList observations
          }
  unless (Ix.null $ document.evidences Ix.@= evidenceId) $
    liftEither $
      Left $
        "Evidence with id " <> show evidenceId <> " already exists!"
  pure $ document & #evidences %~ Ix.insert evidence

mkObservation :: ObservationId -> (CompetenceLevelId, SocialForm, Ability) -> Observation
mkObservation observationid (competenceLevelid, socialForm, abililty) =
  Observation
    { id = observationid
    , competenceLevelId = competenceLevelid
    , socialForm = socialForm
    , ability = abililty
    }

groupByActivityType
  :: [(ActivityType, CompetenceLevelId, SocialForm, Ability)]
  -> [(ActivityType, [(CompetenceLevelId, SocialForm, Ability)])]
groupByActivityType =
  Map.toList
    . Map.fromListWith (<>)
    . map
      ( \(activityType, competenceLevelId, socialForm, ability) -> (activityType, [(competenceLevelId, socialForm, ability)])
      )

matchStudent :: Document -> Text -> Either String UserId
matchStudent d t = case Ix.getOne (d.users Ix.@= t) of
  Just u -> pure u.id
  Nothing -> fuzzyMatchStudent d t

fuzzyMatchStudent :: Document -> Text -> Either String UserId
fuzzyMatchStudent d t =
  case nonEmpty $ filter ((>= 0.8) . snd) $ map (fuzzyMatchScore t) $ Ix.toList d.users of
    Just cs -> pure $ fst $ maximumBy (comparing snd) cs
    Nothing -> Left $ "Could not find matching student for '" <> T.unpack t <> "'."

fuzzyMatchScore :: Text -> User -> (UserId, Double)
fuzzyMatchScore _t u = (u.id, 0)

parseCompetence
  :: Document -> Text -> Either String (ActivityType, CompetenceLevelId, SocialForm, Ability)
parseCompetence d t = do
  (activityDescriptionChar, competenceGridChar, competenceNumber, competenceLevelNumber, abilityChar) <-
    parseCompetenceText t
  (activityType, socialForm) <- resolveActivityAndSocialForm activityDescriptionChar
  competenceLevelId <-
    resolveCompetenceLevel d competenceGridChar competenceNumber competenceLevelNumber
  ability <- resolveAbility abilityChar
  pure (activityType, competenceLevelId, socialForm, ability)

competenceTextParser :: A.Parser (Char, Char, Int, Int, Char)
competenceTextParser = do
  competenceGridText <- A.anyChar
  competenceNumber <- A.decimal
  _ <- A.char '.'
  competenceLevelNumber <- A.decimal
  _ <- A.char ':'
  activityDescription <- A.anyChar
  ability <- A.anyChar
  pure (activityDescription, competenceGridText, competenceNumber, competenceLevelNumber, ability)

parseCompetenceText :: Text -> Either String (Char, Char, Int, Int, Char)
parseCompetenceText t =
  first (\s -> "Could not parse competence '" <> T.unpack t <> "': " <> s) $
    A.parseOnly competenceTextParser t

resolveActivityAndSocialForm :: Char -> Either String (ActivityType, SocialForm)
resolveActivityAndSocialForm 'H' = pure (HomeExercise, Individual)
resolveActivityAndSocialForm 'A' = pure (SchoolExercise, Individual)
resolveActivityAndSocialForm 'G' = pure (SchoolExercise, Group)
resolveActivityAndSocialForm c = Left $ "Unrecognized activity description: " <> [c]

resolveCompetenceLevel :: Document -> Char -> Int -> Int -> Either String CompetenceLevelId
resolveCompetenceLevel d competenceGridChar competenceNumber levelNumber = do
  competenceGridOrder <- mapToCompetenceGridOrder competenceGridChar
  competenceOrder <- mapToCompetenceOrder competenceNumber
  level <- mapToLevel levelNumber
  competenceGrid <-
    maybeToEither ("Can't find competence grid at position " <> show competenceGridOrder <> "!") $
      Ix.getOne (d.competenceGrids Ix.@= competenceGridOrder)
  competence <-
    maybeToEither
      ( "Can't find competence at position "
          <> show competenceOrder
          <> " within competence grid "
          <> show competenceGrid.id
          <> "!"
      )
      $ Ix.getOne (d.competences Ix.@= competenceGrid.id Ix.@= competenceOrder)
  unless (level `Map.member` competence.levelDescriptions) $
    Left ("Level " <> show level <> " does not exist within competence " <> show competence.id <> "!")
  pure (competence.id, level)

mapToCompetenceGridOrder :: Char -> Either String Order
mapToCompetenceGridOrder 'K' = pure $ orderPos 0
mapToCompetenceGridOrder 'L' = pure $ orderPos 1
mapToCompetenceGridOrder 'M' = pure $ orderPos 2
mapToCompetenceGridOrder c = Left $ "Cannot map " <> [c] <> " to competence grid order!"

mapToCompetenceOrder :: Int -> Either String Order
mapToCompetenceOrder i
  | i >= 1 = pure $ orderPos (i - 1)
  | otherwise = Left "Competence number must be >= 1!"

mapToLevel :: Int -> Either String Level
mapToLevel 1 = pure BasicLevel
mapToLevel 2 = pure IntermediateLevel
mapToLevel 3 = pure AdvancedLevel
mapToLevel x = Left $ "Cannot map '" <> show x <> "' to level!"

resolveAbility :: Char -> Either String Ability
resolveAbility '+' = pure SelfReliant
resolveAbility '~' = pure SelfReliantWithSillyMistakes
resolveAbility '-' = pure NotYet
resolveAbility c = Left $ "Can't resolve ability '" <> [c] <> "'!"
