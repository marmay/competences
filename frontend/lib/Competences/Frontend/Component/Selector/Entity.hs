-- | Generic entity selector component.
--
-- Subscribes to a per-entity 'project'ion of the document, renders the
-- entity collection as a search-/filter-driven list with optional
-- create buttons, and propagates the selected entity to a parent slot
-- via a Miso binding. Optionally synchronises the selection with the
-- URL via 'UriBinding'.
--
-- Per-entity selectors (Task, Resource, Assignment) are thin config
-- builders over this component.
{-# LANGUAGE RankNTypes #-}
module Competences.Frontend.Component.Selector.Entity
  ( EntitySelectorConfig (..)
  , CreateAction (..)
  , ItemRenderer (..)
  , Action (..)
  , Model
  , entitySelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document, User)
import Competences.Frontend.Component.Selector.UriBinding (UriBinding (..))
import Competences.Frontend.Fragment.SelectorFilter (FilterFragment (..))
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

-- | A create button's payload: icon, label, and an effect that
-- materialises a new entity (typically by issuing a
-- 'modifySyncDocument' command and returning the value to be stashed
-- in the slot's 'pending' bucket).
data CreateAction selected = CreateAction
  { icon :: !Icon.Icon
  , label :: !MisoString
  , run :: !(SyncContext -> IO selected)
  }

-- | RankN newtype carrying a polymorphic per-item view. The wrapper
-- exists because record-dot access (and 'HasField') can't see through
-- a 'forall' field; the body uses @case ... of ItemRenderer iv -> iv ...@
-- to pull out the function.
newtype ItemRenderer selected projection fAction
  = ItemRenderer
      ( forall m
         . selected
        -> Bool
        -> M.View m (Action selected projection fAction)
      )

-- | Per-host configuration. Each entity selector builds one of these.
--
-- Type parameters:
--
--   * @p@          — the parent component's model type (for the slot
--                    binding).
--   * @selected@   — the entity item type (e.g. @WithOrigin Task@,
--                    plain @Resource@).
--   * @projection@ — the host's projection of 'Document'; for simple
--                    selectors this is just the entity 'IxSet', for
--                    richer selectors it carries precomputed metadata.
--   * @ixs@        — the 'IxSet' indices for @selected@.
--   * @id@         — the entity's identifier; used by 'UriBinding'.
--   * @fState@,
--     @fAction@    — the 'FilterFragment'-internal types.
data EntitySelectorConfig p selected projection ixs id fState fAction = EntitySelectorConfig
  { title :: !MisoString
  , project :: !(Document -> Maybe User -> projection)
  , entitiesOf :: !(projection -> Ix.IxSet ixs selected)
  , itemsInOrder :: !(Ix.IxSet ixs selected -> [selected])
  , idOf :: !(selected -> id)
  , lookupBy :: !(Ix.IxSet ixs selected -> id -> Maybe selected)
  , itemView :: !(ItemRenderer selected projection fAction)
  , createActions :: ![CreateAction selected]
  , uriBinding :: !(Maybe (UriBinding id))
  , initialPick :: !(Maybe (Ix.IxSet ixs selected -> Maybe selected))
  , filter :: !(FilterFragment projection fState fAction selected)
  , parentLens :: !(Lens' p (Maybe selected))
  }

data Action selected projection fAction
  = Pick !selected
  | StashPending !selected
  | UriArrived !M.URI
  | FilterAct !fAction
  | CreateAct !Int
  | ToggleDropdown
  | CloseDropdown
  | ProjectionChanged !(ProjectedChange projection)
  deriving (Generic)

deriving instance (Eq selected, Eq projection, Eq fAction) => Eq (Action selected projection fAction)
deriving instance (Show selected, Show projection, Show fAction) => Show (Action selected projection fAction)

-- ---------------------------------------------------------------------------
-- Internal model
-- ---------------------------------------------------------------------------

-- 'selected' is the validated, in-document slot bound to the parent.
-- 'pending' is a freshly-created entity awaiting the document
-- round-trip; promoted to 'selected' by 'ProjectionChanged' once it
-- appears in the projection.
data Model selected projection fState = Model
  { projection :: !(Maybe projection)
  , selected :: !(Maybe selected)
  , pending :: !(Maybe selected)
  , filterState :: !fState
  , dropdownOpen :: !Bool
  }
  deriving (Generic)

deriving instance (Eq selected, Eq projection, Eq fState) => Eq (Model selected projection fState)
deriving instance (Show selected, Show projection, Show fState) => Show (Model selected projection fState)

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

entitySelectorComponent
  :: forall p selected projection ixs id fState fAction.
     ( Eq selected
     , Eq projection
     , Eq fState
     , Eq fAction
     , Eq id
     )
  => SyncContext
  -> EntitySelectorConfig p selected projection ixs id fState fAction
  -> M.Component p (Model selected projection fState) (Action selected projection fAction)
entitySelectorComponent r cfg =
  (M.component initial update view')
    { M.bindings = [toLensVL cfg.parentLens M.<--- toLensVL #selected]
    , M.subs =
        subscribeWithProjection r cfg.project ProjectionChanged
          : [M.uriSub UriArrived | Just _ <- [cfg.uriBinding]]
    }
  where
    initial =
      Model
        { projection = Nothing
        , selected = Nothing
        , pending = Nothing
        , filterState = cfg.filter.initialState
        , dropdownOpen = False
        }

    update (Pick sel) = do
      M.modify $ \m -> m & (#selected ?~ sel) & (#pending .~ Nothing)
      case cfg.uriBinding of
        Just b -> M.io_ (b.push (cfg.idOf sel))
        Nothing -> pure ()

    update (StashPending sel) = do
      M.modify $ \m -> m & (#pending ?~ sel)
      case cfg.uriBinding of
        Just b -> M.io_ (b.push (cfg.idOf sel))
        Nothing -> pure ()

    update (UriArrived uri) = do
      m <- M.get
      case (cfg.uriBinding, m.projection) of
        (Just b, Just proj)
          | Just newId <- b.extract uri
          , (cfg.idOf <$> m.selected) /= Just newId
          , Just sel <- cfg.lookupBy (cfg.entitiesOf proj) newId ->
              M.modify $ \mm -> mm & (#selected ?~ sel)
        _ -> pure ()

    update (FilterAct fa) =
      M.modify $ \m -> m{filterState = cfg.filter.update fa m.filterState}

    update (CreateAct ix) = M.withSink $ \sink -> do
      case cfg.createActions !? ix of
        Just ca -> do
          newSel <- ca.run r
          sink CloseDropdown
          sink (StashPending newSel)
        Nothing -> pure ()

    update ToggleDropdown =
      M.modify $ \m -> m{dropdownOpen = not m.dropdownOpen}

    update CloseDropdown =
      M.modify $ \m -> m{dropdownOpen = False}

    update (ProjectionChanged pc) =
      M.modify $ \m ->
        let proj = pc.projection
            xs = cfg.entitiesOf proj
            -- Re-validate 'selected' against the new projection.
            validatedSelected =
              m.selected >>= \s -> cfg.lookupBy xs (cfg.idOf s)
            -- If 'pending' resolves now, promote it; otherwise keep
            -- it parked. (Modelled on AssignmentSelector's
            -- pending-promotion logic.)
            (selected', pending') = case m.pending of
              Just p -> case cfg.lookupBy xs (cfg.idOf p) of
                Just promoted -> (Just promoted, Nothing)
                Nothing -> (validatedSelected, Just p)
              Nothing -> (validatedSelected, Nothing)
            -- On the first snapshot, apply the configured initial
            -- pick if nothing is selected yet.
            selected'' = case (pc.changeInfo, selected', cfg.initialPick) of
              (InitialSnapshot, Nothing, Just f) -> f xs
              _ -> selected'
         in m
              { projection = Just proj
              , selected = selected''
              , pending = pending'
              }

    -- ----- view -----

    view' m = case cfg.filter of
      FilterFragment{view = filterView} ->
        M.div_
          [class_ "h-full"]
          [ Layout.vFlow
              (Layout.gapS <> Layout.hFull)
              [ header m
              , FilterAct <$> filterView m.filterState
              , viewItems m
              ]
          ]

    header m =
      if null cfg.createActions
        then SL.selectorHeader cfg.title Nothing
        else
          SL.selectorHeaderWithDropdown
            cfg.title
            m.dropdownOpen
            ToggleDropdown
            (zipWith dropdownEntry [0 ..] cfg.createActions)

    dropdownEntry i ca =
      SL.dropdownItem ca.icon ca.label (CreateAct i)

    viewItems m =
      let proj = m.projection
          allItems = case proj of
            Just p -> cfg.itemsInOrder (cfg.entitiesOf p)
            Nothing -> []
          shown = case proj of
            Just p -> cfg.filter.apply m.filterState p allItems
            Nothing -> []
          isSelected s =
            (cfg.idOf <$> m.selected) == Just (cfg.idOf s)
              || (cfg.idOf <$> m.pending) == Just (cfg.idOf s)
       in case cfg.itemView of
            ItemRenderer iv ->
              SL.selectorList [iv s (isSelected s) | s <- shown]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Total list indexing. Returns 'Nothing' on out-of-range.
(!?) :: [a] -> Int -> Maybe a
xs !? i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      [] -> Nothing
      (x : _) -> Just x
