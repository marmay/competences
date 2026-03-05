{-# LANGUAGE CPP #-}

module Competences.Frontend.BinaryFFI
  ( arrayBufferToByteString
  , byteStringToUint8Array
  , triggerDownload
  )
where

#ifdef WASM

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text (Text)
import Foreign.Ptr (Ptr, castPtr)
import Miso.DSL (JSVal)
import Miso.String (MisoString, ms)

foreign import javascript unsafe "new Uint8Array($1)"
  js_arrayBufferToUint8Array :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.byteLength"
  js_uint8ArrayLen :: JSVal -> IO Int

-- | Copy Uint8Array bytes into WASM linear memory.
-- Same pattern as Miso's js_from_buf in Data.JSString.
foreign import javascript unsafe
  "(new Uint8Array(__exports.memory.buffer, $2, $1.byteLength)).set($1)"
  js_copyFromUint8Array :: JSVal -> Ptr a -> IO ()

-- | Create a Uint8Array by copying from WASM linear memory.
-- The outer new Uint8Array(...) creates a copy that survives after pinned memory is freed.
foreign import javascript unsafe
  "new Uint8Array(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_copyToUint8Array :: Ptr a -> Int -> IO JSVal

-- | Convert a JavaScript ArrayBuffer to a Haskell ByteString.
-- Allocates a new ByteString and copies the ArrayBuffer data into it.
arrayBufferToByteString :: JSVal -> IO ByteString
arrayBufferToByteString ab = do
  uint8 <- js_arrayBufferToUint8Array ab
  len <- js_uint8ArrayLen uint8
  BSI.create len $ \ptr -> js_copyFromUint8Array uint8 ptr

-- | Convert a Haskell ByteString to a JavaScript Uint8Array.
-- Pins the ByteString data and copies it to a new Uint8Array.
byteStringToUint8Array :: ByteString -> IO JSVal
byteStringToUint8Array bs =
  BS.useAsCStringLen bs $ \(ptr, len) ->
    js_copyToUint8Array (castPtr ptr) len

-- | Trigger a browser file download from a Uint8Array.
-- Creates a temporary Blob URL, clicks a hidden <a> element, then cleans up.
foreign import javascript unsafe
  "(() => { \
  \  const blob = new Blob([$1], { type: $2 }); \
  \  const url = URL.createObjectURL(blob); \
  \  const a = document.createElement('a'); \
  \  a.href = url; a.download = $3; \
  \  document.body.appendChild(a); a.click(); \
  \  document.body.removeChild(a); \
  \  URL.revokeObjectURL(url); \
  \})()"
  js_triggerDownload :: JSVal -> MisoString -> MisoString -> IO ()

-- | Trigger a file download in the browser from in-memory data.
triggerDownload :: ByteString -> Text -> Text -> IO ()
triggerDownload contents mimeType fileName = do
  uint8 <- byteStringToUint8Array contents
  js_triggerDownload uint8 (ms mimeType) (ms fileName)

#else

import Data.ByteString (ByteString)
import Data.Text (Text)
import Miso.DSL (JSVal)

-- | Non-WASM stubs (JSaddle mode is not currently supported).
arrayBufferToByteString :: JSVal -> IO ByteString
arrayBufferToByteString = error "BinaryFFI: not available outside WASM"

byteStringToUint8Array :: ByteString -> IO JSVal
byteStringToUint8Array = error "BinaryFFI: not available outside WASM"

triggerDownload :: ByteString -> Text -> Text -> IO ()
triggerDownload _ _ _ = error "BinaryFFI: not available outside WASM"

#endif
