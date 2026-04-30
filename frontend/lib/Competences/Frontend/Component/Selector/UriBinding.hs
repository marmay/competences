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
  { extract :: M.URI -> Maybe id
  , push :: id -> IO ()
  }

-- | Build a 'UriBinding' from a 'Page'-prism: how to embed the entity
-- ID into a 'Page', and how to extract one back. Pairing them in one
-- constructor keeps parser and serializer in sync.
pageBinding :: (id -> Page) -> (Page -> Maybe id) -> UriBinding id
pageBinding intoPage fromPage =
  UriBinding
    { extract = \uri -> case M.route uri of
        Right p -> fromPage p
        Left _ -> Nothing
    , push = M.pushURI . M.toURI . intoPage
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
