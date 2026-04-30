-- | URL parser/pusher pair for list selectors.
--
-- Used by 'listSelectorComponent' to optionally synchronise its
-- selection with the URL: 'extract' parses an incoming URI into the
-- entity ID, 'push' writes the URL when the user picks an item.
-- Build with 'pageBinding' so parser and serializer stay in sync.
module Competences.Frontend.Component.Selector.UriBinding
  ( UriBinding (..)
  , pageBinding
  , popstateSub
  )
where

import Competences.Frontend.Page (Page)
import Miso qualified as M
import Miso.DSL (jsg)
import Miso.FFI qualified as FFI
import Miso.Router qualified as M
import Miso.Subscription.History (getURI)
import Miso.Subscription.Util (createSub)

data UriBinding id = UriBinding
  { extract :: M.URI -> Maybe (Maybe id)
  -- ^ Outer 'Just' means the URI is for this selector's page; the
  -- inner 'Maybe id' is 'Just id' for "with id" or 'Nothing' for
  -- "base path, no id". Outer 'Nothing' means the URI is for a
  -- different page entirely — leave the selector alone.
  , push :: id -> IO ()
  -- ^ Push a URI carrying this id (typically 'M.pushURI' on the
  -- corresponding 'Page' constructor with @Just id@).
  }

-- | Build a 'UriBinding' from a 'Page'-prism. The constructor takes
-- 'Maybe id' so it covers both "with id" and "base path"; the
-- extractor pattern-matches on the same shape.
pageBinding :: (Maybe id -> Page) -> (Page -> Maybe (Maybe id)) -> UriBinding id
pageBinding intoPage fromPage =
  UriBinding
    { extract = \uri -> case M.route uri of
        Right p -> fromPage p
        Left _ -> Nothing
    , push = M.pushURI . M.toURI . intoPage . Just
    }

-- | Subscription for @popstate@ events only. Unlike 'M.uriSub' it
-- does not listen on Miso's global chan, which is single-consumer
-- ('takeMVar'-backed) and so starves additional subscribers on
-- programmatic 'pushURI'. Selectors only need browser-driven URL
-- changes (back/forward, deep links) — their own 'Pick' handler
-- updates state before pushing, so the chan event is redundant.
popstateSub :: (M.URI -> action) -> M.Sub action
popstateSub f sink = createSub acquire release sink
  where
    release cb = do
      win <- jsg "window"
      FFI.removeEventListener win "popstate" cb
    acquire = do
      win <- jsg "window"
      FFI.addEventListener win "popstate" $ \_ ->
        sink . f =<< getURI
