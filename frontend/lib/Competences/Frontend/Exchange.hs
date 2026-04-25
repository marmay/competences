{-# LANGUAGE CPP #-}

-- | Frontend HTTP client for the @/api/exchange@ codec endpoints.
-- The backend is a pure translator; everything else (matching, preview,
-- apply) stays here in IO and operates on the local 'Document'.
module Competences.Frontend.Exchange
  ( encodeExchangeYaml
  , decodeExchangeYaml
  )
where

#ifdef WASM

import Competences.Exchange.Types (ExchangeDoc)
import Competences.Frontend.BinaryFFI (arrayBufferToByteString, byteStringToUint8Array)
import Data.Binary qualified as Bin
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Miso.DSL (JSVal, fromJSVal)
import Miso.Fetch (CONTENT_TYPE (..), Response (..), fetch)
import Miso.String (MisoString, fromMisoString)

-- | Encode an 'ExchangeDoc' to YAML by POSTing it (Binary) to
-- @/api/exchange/encode@. The callback fires with the YAML body on
-- success or a short error message on failure.
encodeExchangeYaml :: ExchangeDoc -> (Either Text Text -> IO ()) -> IO ()
encodeExchangeYaml xdoc callback = do
  bodyVal <- byteStringToUint8Array (BL.toStrict (Bin.encode xdoc))
  fetch
    "/api/exchange/encode"
    "POST"
    (Just bodyVal)
    [("Content-Type", "application/octet-stream"), ("Accept", "text/plain")]
    (\resp -> callback (Right (T.pack (fromMisoString (resp.body :: MisoString)))))
    (errResponseHandler callback)
    TEXT

-- | Decode YAML to an 'ExchangeDoc' by POSTing it (UTF-8 bytes) to
-- @/api/exchange/decode@. The server returns the binary encoding on
-- success or a 4xx with the parser error in the body.
decodeExchangeYaml :: Text -> (Either Text ExchangeDoc -> IO ()) -> IO ()
decodeExchangeYaml yaml callback = do
  bodyVal <- byteStringToUint8Array (encodeUtf8 yaml)
  fetch
    "/api/exchange/decode"
    "POST"
    (Just bodyVal)
    [("Content-Type", "application/octet-stream"), ("Accept", "application/octet-stream")]
    (\resp -> do
        bytes <- arrayBufferToByteString (resp.body :: JSVal)
        case Bin.decodeOrFail (BL.fromStrict bytes) of
          Left (_, _, err) -> callback (Left ("Invalid binary response: " <> T.pack err))
          Right (_, _, xdoc) -> callback (Right xdoc))
    (errResponseHandler callback)
    ARRAY_BUFFER

errResponseHandler :: (Either Text a -> IO ()) -> Response JSVal -> IO ()
errResponseHandler callback resp = do
  let prefix = "exchange request failed"
      withStatus = case resp.status of
        Just s -> prefix <> " (HTTP " <> T.pack (show s) <> ")"
        Nothing -> prefix
      reason = case resp.errorMessage of
        Just msg -> withStatus <> ": " <> T.pack (fromMisoString msg)
        Nothing -> do
          -- Body may carry a plain-text error from Servant's err400.
          withStatus
  -- Try to pull a text body if the server returned one.
  bodyText <- fromJSVal (resp.body :: JSVal) :: IO (Maybe MisoString)
  let detail = case bodyText of
        Just t | not (T.null (T.pack (fromMisoString t))) ->
          reason <> ": " <> T.pack (fromMisoString t)
        _ -> reason
  callback (Left detail)

#else

import Competences.Exchange.Types (ExchangeDoc)
import Data.Text (Text)

encodeExchangeYaml :: ExchangeDoc -> (Either Text Text -> IO ()) -> IO ()
encodeExchangeYaml _ _ = error "Exchange HTTP client: WASM-only"

decodeExchangeYaml :: Text -> (Either Text ExchangeDoc -> IO ()) -> IO ()
decodeExchangeYaml _ _ = error "Exchange HTTP client: WASM-only"

#endif
