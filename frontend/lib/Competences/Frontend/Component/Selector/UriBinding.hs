-- | URL parser/pusher pair for entity selectors.
--
-- Used by 'entitySelectorComponent' to optionally synchronise its
-- selection with the URL: 'extract' parses an incoming URI into the
-- entity ID, 'push' writes the URL when the user picks an item.
-- Build with 'pageBinding' so parser and serializer stay in sync.
module Competences.Frontend.Component.Selector.UriBinding
  ( UriBinding (..)
  , pageBinding
  )
where

import Competences.Frontend.Page (Page)
import Miso qualified as M
import Miso.Router qualified as M

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
