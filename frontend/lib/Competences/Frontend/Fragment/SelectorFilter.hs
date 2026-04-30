-- | Pluggable filter for entity selectors.
--
-- A 'FilterFragment' bundles its own state, action, view, pure update,
-- and a function that turns its state plus the host's projection into
-- the visible item list. The generic 'listSelectorComponent' embeds
-- one fragment per call site; entity-specific filters live alongside
-- their entity types and are passed in via the selector's config.
--
-- The view emits the fragment's own action; the host lifts it into its
-- own action enum via 'fmap' (Miso's 'View' is a 'Functor').
{-# LANGUAGE RankNTypes #-}
module Competences.Frontend.Fragment.SelectorFilter
  ( FilterFragment (..)
  , searchOnlyFilter
  , noopFilter
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Competences.Frontend.View.SelectorList qualified as SL
import Miso qualified as M
import Miso.String (fromMisoString, ms)

-- | Entity-agnostic filter widget bundle.
--
-- Type parameters:
--   * @projection@ — the host's projection type; the filter applies to it.
--   * @state@      — internal state of the filter.
--   * @action@     — internal action type emitted by the view.
--   * @selected@   — the entity item type the filter produces.
--
-- 'apply' takes the projection (so projection-aware filters can read
-- precomputed metadata) and the entity collection in display order, and
-- returns the visible items. Pure search filters can ignore the
-- projection.
data FilterFragment projection state action selected = FilterFragment
  { initialState :: !state
  , update :: action -> state -> state
  , view :: forall p. state -> projection -> M.View p action
  -- ^ The projection is supplied so projection-aware filters can
  -- render UI that depends on it (e.g. a role-aware enum dropdown
  -- whose mode list reflects whether a focused user is set). Pure
  -- search filters can ignore it.
  , apply :: state -> projection -> [selected] -> [selected]
  }

-- | Filter fragment that supports text search only — used by the simpler
-- entity selectors (Task, Resource).
--
-- The first argument is the placeholder text for the search input; the
-- second extracts the searchable text from each entity. 'apply'
-- ignores the projection and case-insensitively matches the query as
-- a substring of the displayed text.
searchOnlyFilter
  :: M.MisoString
  -> (selected -> Text)
  -> FilterFragment projection Text Text selected
searchOnlyFilter placeholder displayText =
  FilterFragment
    { initialState = ""
    , update = \new _old -> new
    , view = \q _proj ->
        SL.selectorSearchField (ms q) placeholder (fromMisoString :: M.MisoString -> Text)
    , apply = \q _proj items ->
        if T.null q
          then items
          else
            let lq = T.toLower q
             in filter (\s -> lq `T.isInfixOf` T.toLower (displayText s)) items
    }

-- | Filter fragment that does nothing — no UI, all items pass. Use
-- when a list selector wants no filtering (e.g. a small fixed list
-- where search would be noise).
noopFilter :: FilterFragment projection () () selected
noopFilter =
  FilterFragment
    { initialState = ()
    , update = \_act s -> s
    , view = \_st _proj -> M.text ""
    , apply = \_st _proj items -> items
    }
