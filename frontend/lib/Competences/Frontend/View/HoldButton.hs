-- | Reusable hold-to-delete button: press and hold for 2 seconds to confirm deletion.
--
-- Shows a white fill animation while held; releasing cancels the delete.
-- Touch-device friendly (handles touch events alongside mouse events).
--
-- On short click (release before 2s completes), a tooltip appears for 2s
-- explaining that the button must be held. Starting a new hold hides it.
--
-- Usage:
--
-- 1. Add @HoldAction id@ to your action type (wrapped in a constructor)
-- 2. Add @HoldState id@ to your model (initialise with 'emptyHoldState')
-- 3. Delegate in update: @HoldButton.handleHoldAction #holdField doDelete WrapCtor ha@
-- 4. Render: @HoldButton.holdButton WrapCtor holdState thisId@
module Competences.Frontend.View.HoldButton
  ( HoldAction (..)
  , HoldState (..)
  , emptyHoldState
  , handleHoldAction
  , holdButton
  , isHoldingId
  )
where

import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Control.Concurrent (threadDelay)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH
import Optics.Core (Lens', (&), (.~), (^.))

-- | Tracks which item is being held, the hold session generation, and
-- whether to show a short-click hint tooltip.
data HoldState id = HoldState
  { holdId :: !(Maybe id)
  , holdGen :: !Int
  , showHint :: !Bool
  }
  deriving (Eq, Show)

-- | Initial hold state (nothing held, generation 0, no hint).
emptyHoldState :: HoldState id
emptyHoldState = HoldState Nothing 0 False

-- | Check if a specific id is currently being held.
isHoldingId :: (Eq id) => HoldState id -> id -> Bool
isHoldingId hs eid = hs.holdId == Just eid

-- | Actions for a hold-to-delete interaction.
data HoldAction id
  = StartHold !id
  | ReleaseHold
  | ExecuteHold !id !Int
  | HideHint
  deriving (Eq, Show)

-- | Handle a 'HoldAction' inside a Miso component update function.
--
-- @holdLens@: lens to the @HoldState id@ field tracking the hold.
-- @onExecute@: IO action to run when the hold completes (e.g. send a delete command).
-- @wrap@: constructor to wrap 'HoldAction' back into the parent action type.
handleHoldAction
  :: (Eq id)
  => Lens' model (HoldState id)
  -> (id -> IO ())
  -> (HoldAction id -> action)
  -> HoldAction id
  -> M.Effect parent model action
handleHoldAction holdLens _onExecute wrap (StartHold eid) = do
  m <- M.get
  let gen = (m ^. holdLens).holdGen + 1
  M.modify $ \m' -> m' & holdLens .~ HoldState (Just eid) gen False
  M.io $ do
    threadDelay 2_000_000
    pure (wrap (ExecuteHold eid gen))
handleHoldAction holdLens _onExecute wrap ReleaseHold = do
  m <- M.get
  let hs = m ^. holdLens
  case hs.holdId of
    Just _ -> do
      M.modify $ \m' -> m' & holdLens .~ HoldState Nothing hs.holdGen True
      M.io $ do
        threadDelay 2_000_000
        pure (wrap HideHint)
    Nothing ->
      pure ()
handleHoldAction holdLens onExecute _wrap (ExecuteHold eid gen) = do
  m <- M.get
  let hs = m ^. holdLens
  case hs.holdId of
    Just eid' | eid == eid' && hs.holdGen == gen -> do
      M.io_ $ onExecute eid
      M.modify $ \m' -> m' & holdLens .~ HoldState Nothing hs.holdGen False
    _ -> pure ()
handleHoldAction holdLens _onExecute _wrap HideHint =
  M.modify $ \m -> m & holdLens .~ (m ^. holdLens){showHint = False}

-- | Render a hold-to-delete button (trash icon with 2s fill animation).
--
-- During the hold, a full-screen transparent overlay captures mouseup
-- anywhere on the page, so dragging outside the button and releasing
-- reliably cancels the hold.
--
-- When 'showHint' is set (after a short click), a tooltip appears above
-- the button explaining the hold-to-activate interaction.
holdButton
  :: (Eq id)
  => (HoldAction id -> action)
  -> HoldState id
  -> id
  -> M.View m action
holdButton wrap hs eid =
  let isHolding = isHoldingId hs eid
   in MH.div_
        [class_ "relative inline-flex"]
        [ -- Full-screen overlay to capture mouseup anywhere during hold.
          -- Always rendered to keep stable DOM child indices (avoids Miso
          -- virtual-DOM diffing issues that break CSS transitions).
          MH.div_
            [ class_ $ "fixed inset-0 z-[10000]" <> if isHolding then "" else " hidden"
            , MH.onMouseUp (wrap ReleaseHold)
            , MH.onMouseEnter (wrap ReleaseHold)
            , MH.onTouchEnd (wrap ReleaseHold)
            , MH.onTouchCancel (wrap ReleaseHold)
            ]
            []
        , -- Tooltip hint (shown after short click, hidden during hold)
          MH.div_
            [ class_ $
                "absolute bottom-full left-1/2 -translate-x-1/2 mb-1 px-2 py-1 \
                \bg-primary text-primary-foreground text-xs rounded-md \
                \whitespace-nowrap pointer-events-none z-50"
                  <> if hs.showHint && not isHolding then "" else " hidden"
            ]
            [M.text $ C.translate' C.LblHoldToDelete]
        , -- Button with progress animation
          MH.div_
            [ class_ "relative z-[10001] inline-flex overflow-hidden rounded-md"
            , MH.onMouseDown (wrap (StartHold eid))
            , MH.onMouseUp (wrap ReleaseHold)
            , MH.onMouseLeave (wrap ReleaseHold)
            , MH.onTouchStart (wrap (StartHold eid))
            , MH.onTouchEnd (wrap ReleaseHold)
            , MH.onTouchCancel (wrap ReleaseHold)
            ]
            [ -- Button
              MH.button_
                [class_ "btn-sm-icon-destructive"]
                [Icon.iconS Icon.Small Icon.IcnDelete]
            , -- Progress fill overlay
              MH.div_
                [ class_ $ "absolute inset-0 bg-white/30 rounded-md pointer-events-none"
                    <> if isHolding then " w-full" else " w-0"
                , MC.style_ $
                    [("transition-property", "width")]
                      <> [("transition-duration", "2s") | isHolding]
                      <> [("transition-timing-function", "linear") | isHolding]
                ]
                []
            ]
        ]
