-- | Reusable hold-to-delete button: press and hold for 2 seconds to confirm deletion.
--
-- Shows a red fill animation while held; releasing cancels the delete.
-- Touch-device friendly (handles touch events alongside mouse events).
--
-- Usage:
--
-- 1. Add @HoldAction id@ to your action type (wrapped in a constructor)
-- 2. Add @Maybe id@ to your model
-- 3. Delegate in update: @HoldButton.handleHoldAction #holdField doDelete WrapCtor ha@
-- 4. Render: @HoldButton.holdButton WrapCtor (holding == Just thisId) thisId@
module Competences.Frontend.View.HoldButton
  ( HoldAction (..)
  , handleHoldAction
  , holdButton
  )
where

import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Control.Concurrent (threadDelay)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH
import Optics.Core (Lens', (&), (.~), (^.))

-- | Actions for a hold-to-delete interaction.
data HoldAction id
  = StartHold !id
  | ReleaseHold
  | ExecuteHold !id
  deriving (Eq, Show)

-- | Handle a 'HoldAction' inside a Miso component update function.
--
-- @holdLens@: lens to the @Maybe id@ field tracking which item is being held.
-- @onExecute@: IO action to run when the hold completes (e.g. send a delete command).
-- @wrap@: constructor to wrap 'HoldAction' back into the parent action type.
handleHoldAction
  :: (Eq id)
  => Lens' model (Maybe id)
  -> (id -> IO ())
  -> (HoldAction id -> action)
  -> HoldAction id
  -> M.Effect parent model action
handleHoldAction holdLens _onExecute wrap (StartHold eid) = do
  M.modify $ \m -> m & holdLens .~ Just eid
  M.io $ do
    threadDelay 2_000_000
    pure (wrap (ExecuteHold eid))
handleHoldAction holdLens _onExecute _wrap ReleaseHold =
  M.modify $ \m -> m & holdLens .~ Nothing
handleHoldAction holdLens onExecute _wrap (ExecuteHold eid) = do
  m <- M.get
  case m ^. holdLens of
    Just eid' | eid == eid' -> do
      M.io_ $ onExecute eid
      M.modify $ \m' -> m' & holdLens .~ Nothing
    _ -> pure ()

-- | Render a hold-to-delete button (trash icon with 2s fill animation).
holdButton
  :: (HoldAction id -> action)
  -> Bool
  -> id
  -> M.View m action
holdButton wrap isHolding eid =
  MH.div_
    [ class_ "relative inline-flex overflow-hidden rounded-md"
    , MH.onMouseDown (wrap (StartHold eid))
    , MH.onMouseUp (wrap ReleaseHold)
    , MH.onMouseLeave (wrap ReleaseHold)
    , MH.onTouchStart (wrap (StartHold eid))
    , MH.onTouchEnd (wrap ReleaseHold)
    , MH.onTouchCancel (wrap ReleaseHold)
    ]
    [ -- Button (rendered first, sits below overlay)
      MH.button_
        [class_ "btn-sm-icon-destructive"]
        [Icon.iconS Icon.Small Icon.IcnDelete]
    , -- Progress fill overlay (on top, pointer-events-none so clicks pass through)
      MH.div_
        [ class_ $ "absolute inset-0 bg-destructive/10 rounded-md pointer-events-none"
            <> if isHolding then " w-full" else " w-0"
        , MC.style_ $
            [("transition-property", "width")]
              <> [("transition-duration", "2s") | isHolding]
              <> [("transition-timing-function", "linear") | isHolding]
        ]
        []
    ]
