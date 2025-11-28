{-# LANGUAGE OverloadedLabels #-}

module Main
  ( main
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Competence, Document (..), Level, Template (..), TemplateExercise, TemplateName (..), User (..))
import Competences.Document.Evidence (Ability, ActivityType (..), SocialForm (..))
import Control.Applicative (Alternative (..))
import Control.Exception (Exception, catch, throw)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, execStateT, get, lift, put)
import Data.Aeson (eitherDecode, encode)
import Data.Attoparsec.Text qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import Optics.Core ((&), (^.))
import Options.Applicative qualified as O
import System.Console.Haskeline qualified as H
import System.Random (randomIO)
import Data.Either.Extra (maybeToEither)

newtype Options = Options
  { document :: FilePath
  }
  deriving (Eq, Show)

data ParseError = ParseError !FilePath !String
  deriving (Eq, Show)

instance Exception ParseError

throwLeft :: (Exception e) => (a -> e) -> Either a b -> b
throwLeft f = either (throw . f) id

options :: O.Parser Options
options =
  Options
    <$> O.strOption
      ( O.long "document"
          <> O.short 'd'
          <> O.help "Path to the document file."
          <> O.metavar "DOCUMENT_PATH"
      )

main :: IO ()
main = handleExceptions $ do
  opts <- O.execParser (O.info (options O.<**> O.helper) O.fullDesc)
  document <- throwLeft (ParseError opts.document) . eitherDecode <$> BL.readFile opts.document
  (document', _) <-
    execStateT (H.runInputT (H.defaultSettings & H.setComplete complete) loop) (document, Init)
  BL.writeFile opts.document $ encode document'
  where
    handleExceptions :: IO () -> IO ()
    handleExceptions a =
      a
        `catch` (\(ParseError p e) -> putStrLn $ "Error: " <> p <> " could not be parsed: " <> e)

loop :: (MonadIO m, MonadMask m) => H.InputT (StateT S m) ()
loop = do
  (document, state) <- lift get
  line <- H.getInputLine (display state)
  case line of
    (Just line') -> do
      (document', state') <- handleInput document state line'
      lift $ put (document', state')
      loop
    Nothing -> pure ()

complete :: (MonadIO m) => H.CompletionFunc (StateT S m)
complete t = do
  (document, state) <- get
  pure $ complete' t document state

complete' :: (String, String) -> Document -> RunState -> (String, [H.Completion])
complete' (_, _) _ _ = ("", [])

data InitCommand
  = ApplyTemplate !Text !Text
  | EditTemplate !Text
  deriving (Eq, Show)

data EditCommand
  = EditCommit
  | EditCancel
  | EditShow
  | EditSetName !Text
  | EditSetDate !Text
  | EditSetActivityType !ActivityType
  | EditSetSocialForm ![SocialForm]
  | EditAddTask !Text
  deriving (Eq, Show)

initCommandParser :: A.Parser InitCommand
initCommandParser =
  applyTemplateParser <|> editTemplateParser
  where
    applyTemplateParser = do
      _ <- A.string "apply"
      _ <- A.many1 A.space
      templateName <- identifier
      _ <- A.char '@'
      _ <- A.many1 A.space
      userName <- identifier
      pure $ ApplyTemplate (T.strip templateName) (T.strip userName)
    editTemplateParser = EditTemplate <$> (A.string "edit" *> A.many1 A.space *> identifier)
    identifier = T.pack <$> A.many1 (A.letter <|> A.digit <|> A.char ' ')

handleInitCmd :: (MonadIO m) => Document -> String -> ExceptT String (H.InputT m) (Document, RunState)
handleInitCmd d cmd = liftEither (A.parseOnly initCommandParser (T.pack cmd)) >>= handleInitCmd' d

findTemplate :: Document -> Text -> Either String Template
findTemplate d t =
  maybeToEither "Could not find template!" $ Ix.getOne ((d ^. #templates) Ix.@= TemplateName t)

findUser :: Document -> Text -> Either String User
findUser d u =
  maybeToEither "Could not find user!" $ Ix.getOne ((d ^. #users) Ix.@= u)
  
handleInitCmd' :: (MonadIO m) => Document -> InitCommand -> ExceptT String (H.InputT m) (Document, RunState)
handleInitCmd' d (ApplyTemplate t u) = do
  template <- liftEither $ findTemplate d t
  user <- liftEither $ findUser d u
  pure (d, Apply template user (ApplyState [] template.exercises))
handleInitCmd' d (EditTemplate t) = do
  case Ix.getOne ((d ^. #templates) Ix.@= TemplateName t) of
    Just t' -> pure (d, Edit t' EditInit)
    Nothing -> do
      t' <- mkTemplate t
      pure (d, Edit t' EditInit)
  where
    mkTemplate t' = do
      id' <- liftIO randomIO
      date <- utctDay <$> liftIO getCurrentTime
      pure $ Template id' (TemplateName t') date Exam [Individual] []

data ApplyCommand
  = Rate !Int !Bool
  | Summarize
  | Commit
  | Abort

applyCommandParser :: A.Parser ApplyCommand
applyCommandParser =
  summarizeParser <|> commitParser <|> abortParser <|> rateParser
  where
    summarizeParser = Summarize <$ A.string "summarize"
    commitParser = Commit <$ A.string "commit"
    abortParser = Abort <$ A.string "abort"
    rateParser = do
      level <- A.decimal
      withoutSillyMistakes <- A.option True (False <$ A.char '?')
      pure $ Rate level withoutSillyMistakes

handleApplyCmd :: (MonadIO m) => Template -> User -> ApplyState -> Document -> String -> ExceptT String (H.InputT m) (Document, RunState)
handleApplyCmd t u s d cmd = liftEither (A.parseOnly applyCommandParser (T.pack cmd)) >>= handleApplyCmd' t u s d

handleApplyCmd' :: (MonadIO m) => Template -> User -> ApplyState -> Document -> ApplyCommand -> ExceptT String (H.InputT m) (Document, RunState)
handleApplyCmd' _ _ (ApplyState rs (t:ts)) d (Rate level withoutSillyMistakes) = do
  competence <- Ix.getOne ((d ^. #competences) Ix.@= t.competence)

handle :: (MonadIO m) => RunState -> Document -> String -> ExceptT String (H.InputT m) (Document, RunState)
handle Init = handleInitCmd
handle (Apply t u s) = handleApplyCmd t u s
handle _ = \_ _ -> liftEither $ Left "Not implemented yet."

handleInput :: (MonadIO m) => Document -> RunState -> String -> H.InputT m (Document, RunState)
handleInput d s cmd = do
  runExceptT (handle s d cmd) >>= \case
    Right res -> pure res
    Left err -> do
      liftIO $ putStrLn ("Error: " <> err)
      pure (d, s)

display :: RunState -> String
display Init = ">"
display (Apply t u _) = "apply " <> T.unpack t.name.unTemplateName <> " @ " <> T.unpack u.name <> " >"
display (Edit t _) = "edit " <> T.unpack t.name.unTemplateName <> " >"

type S = (Document, RunState)

data RunState
  = Init
  | Apply Template User ApplyState
  | Edit Template EditState

data ApplyState = ApplyState
  { assessments :: ![(Text, Competence, Level, Ability)]
  , toAssess :: ![TemplateExercise]
  }
  deriving (Eq, Show)

data EditState
  = EditInit
