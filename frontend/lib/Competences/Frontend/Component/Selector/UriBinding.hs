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

-- | Subscription that fires only on browser-driven URL changes
-- (back/forward, deep-link load) — i.e. @popstate@ events.
--
-- Unlike Miso's 'M.uriSub', this does NOT subscribe to the global
-- chan that 'M.pushURI' notifies. That chan is single-consumer
-- ('takeMVar'-backed), so multiple 'M.uriSub' subscribers starve
-- each other on every programmatic push: each push wakes exactly
-- one waiter. With the App's 'M.uriSub' and a per-selector
-- subscription both registered, the App's @SetURI@ handler would
-- only fire on every other push, leaving 'm.uri' stale and
-- breaking page navigation after a click.
--
-- Selectors don't need the chan side anyway: programmatic pushes
-- come from their own 'Pick' handlers, which already updated state
-- before pushing. They only need to react to /external/ URL changes
-- (browser back/forward, fresh deep link), and that's exactly what
-- the @popstate@ DOM event covers — broadcast to every listener.
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
