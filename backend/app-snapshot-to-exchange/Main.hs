{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Competences.Document (Document)
import Competences.Exchange.Types (ExchangeAttachment(..))
import Competences.Exchange.Build (documentExchange)
import Competences.Exchange.Query (exchangeDocAttachments)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Types qualified as A
import Data.Text.IO qualified as T
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Options.Applicative qualified as O

data Config = Config
  { snapshotPath :: !FilePath
    -- ^ Path to the snapshot, the data is extracted from.
  , exchangeYamlPath :: !FilePath
    -- ^ Extracted data is written to this path.
  , casHashesPath :: !FilePath
    -- ^ All CAS hashes that are referenced in the output
    -- data are written to this path.
  } deriving (Eq, Show)

optionsP :: O.Parser Config
optionsP = Config
  <$> O.strOption
      ( O.long "snapshot" 
      <> O.metavar "SNAPSHOT"
      <> O.help "Path of the snapshot, the data is extracted from."
      )
  <*> O.strOption
      ( O.long "exchange-yaml"
      <> O.metavar "EXCHANGE_YAML"
      <> O.help "Path, the exchange yaml output is written to."
      )
  <*> O.strOption
      ( O.long "cas-hashes"
      <> O.metavar "CAS_HASHES"
      <> O.help "Path, the CAS hashes list is written to."
      )

progDesc :: O.ParserInfo Config
progDesc = O.info optionsP desc
  where
    desc =
      O.progDesc
        ( "Reads a document snapshot from SNAPSHOT and extracts non-student data. "
          <> "Data is written in exchange format (suitable for partial or complete "
          <> "import) into EXCHANGE_YAML. All hashes that are require dfrom the "
          <> "CAS to import those contents are written to CAS_HASHES, one hash "
          <> "per line."
        )
        <> O.header "Extracts teaching materials from document snapshots."

main :: IO ()
main = O.execParser progDesc >>= run

run :: Config -> IO ()
run Config{ .. } = do
  snapshot <- readSnapshot snapshotPath
  let exchangeDoc = documentExchange snapshot
      casHashes = map (.sha256) $ exchangeDocAttachments exchangeDoc
  Y.encodeFile exchangeYamlPath exchangeDoc
  T.writeFile casHashesPath (T.unlines casHashes)

readSnapshot :: FilePath -> IO Document
readSnapshot p = do
  v <- either error pure =<< A.eitherDecodeFileStrict p
  let inner = case v of
        A.Object o | Just payload <- A.lookup "payload" o -> payload
        _ -> v
  either error pure $ A.parseEither A.parseJSON inner
