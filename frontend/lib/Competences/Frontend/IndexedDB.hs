{-# LANGUAGE CPP #-}

-- | IndexedDB storage for checkpoint documents.
--
-- Stores Binary-encoded projected documents in IndexedDB for incremental sync
-- on WebSocket reconnection. Uses the browser's IndexedDB API via JS FFI.
module Competences.Frontend.IndexedDB
  ( IndexedDB
  , CheckpointData (..)
  , openDatabase
  , storeCheckpoint
  , loadCheckpoint
  , clearCheckpoint
  , computeDocumentChecksum
  )
where

import Competences.Document (Document)
import Competences.Protocol (CommandId)
import Data.Text (Text)

#ifdef WASM

import Competences.Frontend.BinaryFFI (arrayBufferToByteString, byteStringToUint8Array)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Miso.DSL (JSVal, ToJSVal (..), fromJSVal)

-- | Handle to the IndexedDB database.
newtype IndexedDB = IndexedDB JSVal

-- | Checkpoint data stored in IndexedDB.
data CheckpointData = CheckpointData
  { document :: !Document
  , commandId :: !CommandId
  , checksum :: !Text
  }

-- Compute SHA-256 hash of a Uint8Array using Web Crypto API.
-- Returns lowercase hex string.
foreign import javascript safe
  "await crypto.subtle.digest('SHA-256', $1).then(buf => Array.from(new Uint8Array(buf)).map(b => b.toString(16).padStart(2, '0')).join(''))"
  js_sha256 :: JSVal -> IO JSVal

-- | Compute a SHA-256 checksum of a document's Binary encoding.
-- Matches the backend's 'computeDocumentChecksum' (crypton SHA-256 of Binary.encode).
computeDocumentChecksum :: Document -> IO Text
computeDocumentChecksum doc = do
  let bytes = BL.toStrict (Bin.encode doc)
  uint8 <- byteStringToUint8Array bytes
  hashVal <- js_sha256 uint8
  mText <- fromJSVal hashVal
  pure $ maybe T.empty id mText

-- Open (or create) the 'competences-checkpoints' database with object store.
-- Version 1 creates the 'checkpoints' object store.
foreign import javascript safe
  "await new Promise((resolve, reject) => { const req = indexedDB.open('competences-checkpoints', 1); req.onupgradeneeded = (e) => { const db = e.target.result; if (!db.objectStoreNames.contains('checkpoints')) { db.createObjectStore('checkpoints'); } }; req.onsuccess = (e) => resolve(e.target.result); req.onerror = (e) => reject(e.target.error); })"
  js_openDatabase :: IO JSVal

-- Store a value (Uint8Array) at a key in the 'checkpoints' store.
foreign import javascript safe
  "await new Promise((resolve, reject) => { const tx = $1.transaction(['checkpoints'], 'readwrite'); const store = tx.objectStore('checkpoints'); store.put($3, $2); tx.oncomplete = () => resolve(); tx.onerror = (e) => reject(e.target.error); })"
  js_put :: JSVal -> JSVal -> JSVal -> IO ()

-- Get a value from the 'checkpoints' store. Returns null if not found.
foreign import javascript safe
  "await new Promise((resolve, reject) => { const tx = $1.transaction(['checkpoints'], 'readonly'); const store = tx.objectStore('checkpoints'); const req = store.get($2); req.onsuccess = () => resolve(req.result); req.onerror = (e) => reject(e.target.error); })"
  js_get :: JSVal -> JSVal -> IO JSVal

-- Delete a key from the 'checkpoints' store.
foreign import javascript safe
  "await new Promise((resolve, reject) => { const tx = $1.transaction(['checkpoints'], 'readwrite'); const store = tx.objectStore('checkpoints'); store.delete($2); tx.oncomplete = () => resolve(); tx.onerror = (e) => reject(e.target.error); })"
  js_delete :: JSVal -> JSVal -> IO ()

-- Check if a value is null or undefined.
foreign import javascript unsafe "$1 == null"
  js_isNull :: JSVal -> IO Bool

-- | Open the IndexedDB database for checkpoint storage.
openDatabase :: IO IndexedDB
openDatabase = IndexedDB <$> js_openDatabase

-- | Current checkpoint encoding version. Bump when the Binary encoding changes.
-- Old checkpoints with a different version (or no version prefix) will fail
-- decodeOrFail and be automatically deleted.
checkpointVersion :: Int
checkpointVersion = 2

-- | Store checkpoint data in IndexedDB.
-- Key: "checkpoint:<userId>" to account for impersonation.
-- Encodes with a version prefix for forward compatibility.
storeCheckpoint :: IndexedDB -> Text -> CheckpointData -> IO ()
storeCheckpoint (IndexedDB db) key cpData = do
  let bytes = BL.toStrict $ Bin.encode (checkpointVersion, cpData.document, cpData.commandId, cpData.checksum)
  uint8 <- byteStringToUint8Array bytes
  jsKey <- toJSVal key
  js_put db jsKey uint8

-- | Load checkpoint data from IndexedDB. Returns Nothing if not found.
-- Rejects and deletes entries with wrong version or corrupted data.
loadCheckpoint :: IndexedDB -> Text -> IO (Maybe CheckpointData)
loadCheckpoint (IndexedDB db) key = do
  jsKey <- toJSVal key
  val <- js_get db jsKey
  isNull <- js_isNull val
  if isNull
    then pure Nothing
    else do
      bs <- arrayBufferToByteString val
      case Bin.decodeOrFail (BL.fromStrict bs) of
        Left _ -> do
          -- Corrupted or incompatible binary data — remove stale entry
          js_delete db jsKey
          pure Nothing
        Right (_, _, (version :: Int, doc, cmdId, checksum))
          | version /= checkpointVersion -> do
              -- Old checkpoint version — remove stale entry
              js_delete db jsKey
              pure Nothing
          | otherwise ->
              pure $ Just CheckpointData
                { document = doc
                , commandId = cmdId
                , checksum = checksum
                }

-- | Clear a stored checkpoint from IndexedDB.
clearCheckpoint :: IndexedDB -> Text -> IO ()
clearCheckpoint (IndexedDB db) key = do
  jsKey <- toJSVal key
  js_delete db jsKey

#else

-- | Stub handle for non-WASM mode.
data IndexedDB = IndexedDB

-- | Checkpoint data stored in IndexedDB.
data CheckpointData = CheckpointData
  { document :: !Document
  , commandId :: !CommandId
  , checksum :: !Text
  }

-- | Non-WASM stubs.
computeDocumentChecksum :: Document -> IO Text
computeDocumentChecksum _ = pure ""

openDatabase :: IO IndexedDB
openDatabase = pure IndexedDB

storeCheckpoint :: IndexedDB -> Text -> CheckpointData -> IO ()
storeCheckpoint _ _ _ = pure ()

loadCheckpoint :: IndexedDB -> Text -> IO (Maybe CheckpointData)
loadCheckpoint _ _ = pure Nothing

clearCheckpoint :: IndexedDB -> Text -> IO ()
clearCheckpoint _ _ = pure ()

#endif
