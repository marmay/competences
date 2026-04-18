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
-- 3. Delegate in update: @liftEffect #holdField WrapCtor (updateHold doDelete ha)@
-- 4. Render: @HoldButton.holdDeleteButtonSm WrapCtor holdState thisId@
module Competences.Frontend.View.HoldButton
  ( HoldAction (..)
  , HoldState (..)
  , emptyHoldState
  , updateHold
  , holdFragmentDef
  , holdButton
  , holdDeleteButton
  , holdDeleteButtonSm
  , isHoldingId
  )
where

import Competences.Frontend.Common.Effect (FragmentDef (..), GEffect)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color.Status (Status (..), statusPalette)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Notification (notificationBanner)
import Competences.Frontend.View.Tailwind (class_)
import Control.Concurrent (threadDelay)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH

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

-- | Self-contained hold update. Operates on 'HoldState' directly.
-- Returns 'True' when 'ExecuteHold' actually fires the callback.
-- Use with 'liftEffect' to embed into a parent component:
--
-- @
-- liftEffect #holdField WrapCtor (updateHold doDelete ha)
-- @
updateHold
  :: (Eq id)
  => (id -> IO ())
  -> HoldAction id
  -> GEffect parent (HoldState id) (HoldAction id) Bool
updateHold _onExecute (StartHold eid) = do
  hs <- M.get
  let gen = hs.holdGen + 1
  M.put $ HoldState (Just eid) gen False
  M.io $ do
    threadDelay 2_000_000
    pure (ExecuteHold eid gen)
  pure False
updateHold _onExecute ReleaseHold = do
  hs <- M.get
  case hs.holdId of
    Just _ -> do
      M.put $ HoldState Nothing hs.holdGen True
      M.io $ do
        threadDelay 2_000_000
        pure HideHint
    Nothing ->
      pure ()
  pure False
updateHold onExecute (ExecuteHold eid gen) = do
  hs <- M.get
  case hs.holdId of
    Just eid' | eid == eid' && hs.holdGen == gen -> do
      M.io_ $ onExecute eid
      M.put $ HoldState Nothing hs.holdGen False
      pure True
    _ -> pure False
updateHold _onExecute HideHint = do
  M.modify $ \hs -> hs{showHint = False}
  pure False

-- | Bundle hold button as a 'FragmentDef'.
-- The view renders a destructive hold button for the given entity id.
holdFragmentDef
  :: (Eq id)
  => (id -> IO ())
  -> id
  -> Button.ButtonVariant
  -> Button.ButtonSize
  -> Button.ButtonContents
  -> FragmentDef parent (HoldState id) (HoldAction id) Bool ((HoldAction id -> a) -> M.View m a)
holdFragmentDef onExecute eid variant size contents = FragmentDef
  { initialModel = emptyHoldState
  , update = updateHold onExecute
  , view = \hs liftAction -> holdButton liftAction hs eid variant size contents
  , subs = []
  }

-- | Actions for a hold-to-delete interaction.
data HoldAction id
  = StartHold !id
  | ReleaseHold
  | ExecuteHold !id !Int
  | HideHint
  deriving (Eq, Show)

-- | Render a hold-to-activate button with configurable appearance.
holdButton
  :: (Eq id)
  => (HoldAction id -> action)
  -> HoldState id
  -> id
  -> Button.ButtonVariant
  -> Button.ButtonSize
  -> Button.ButtonContents
  -> M.View m action
holdButton wrap hs eid variant size contents =
  let isHolding = isHoldingId hs eid
   in MH.div_
        [class_ "relative inline-flex"]
        [ if hs.showHint && not isHolding
            then
              notificationBanner (statusPalette Pending)
                [ Icon.iconVS Icon.Primary Icon.Small Icon.IcnInfo
                , MH.span_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblHoldToDelete]
                ]
            else M.text ""
        , MH.div_
            [class_ "relative inline-flex isolate"]
            [ MH.div_
                [ class_ $ "fixed inset-0" <> if isHolding then "" else " hidden"
                , MH.onMouseUp (wrap ReleaseHold)
                , MH.onMouseEnter (wrap ReleaseHold)
                , MH.onTouchEnd (wrap ReleaseHold)
                , MH.onTouchCancel (wrap ReleaseHold)
                ]
                []
            , MH.div_
                [ class_ "relative inline-flex overflow-hidden rounded-md"
                , MH.onMouseDown (wrap (StartHold eid))
                , MH.onMouseUp (wrap ReleaseHold)
                , MH.onMouseLeave (wrap ReleaseHold)
                , MH.onTouchStart (wrap (StartHold eid))
                , MH.onTouchEnd (wrap ReleaseHold)
                , MH.onTouchCancel (wrap ReleaseHold)
                ]
                [ MH.button_
                    [class_ $ btnClass variant size contents]
                    [renderBtnContents size contents]
                , MH.div_
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
        ]

-- | Small icon-only destructive hold button.
holdDeleteButtonSm
  :: (Eq id)
  => (HoldAction id -> action)
  -> HoldState id
  -> id
  -> M.View m action
holdDeleteButtonSm wrap hs eid =
  holdButton wrap hs eid Button.Destructive Button.Small (Button.IconOnly Icon.IcnDelete)

-- | Regular destructive hold button with icon + "Delete" text.
holdDeleteButton
  :: (Eq id)
  => (HoldAction id -> action)
  -> HoldState id
  -> id
  -> M.View m action
holdDeleteButton wrap hs eid =
  holdButton wrap hs eid Button.Destructive Button.Regular
    (Button.IconText Icon.IcnDelete (C.translate' C.LblDelete))

btnClass :: Button.ButtonVariant -> Button.ButtonSize -> Button.ButtonContents -> Text
btnClass v s c =
  T.intercalate "-" $
    ["btn"]
      <> maybeToList (sizeClass s)
      <> maybeToList (iconClass c)
      <> maybeToList (variantClass v)
  where
    variantClass Button.Primary = Nothing
    variantClass Button.Secondary = Just "secondary"
    variantClass Button.Destructive = Just "destructive"
    variantClass Button.Ghost = Just "ghost"
    variantClass Button.Link = Just "link"
    variantClass Button.Outline = Just "outline"

    sizeClass Button.Small = Just "sm"
    sizeClass Button.Regular = Nothing
    sizeClass Button.Large = Just "lg"

    iconClass (Button.IconOnly _) = Just "icon"
    iconClass (Button.SizedIcon _ _) = Just "icon"
    iconClass _ = Nothing

renderBtnContents :: Button.ButtonSize -> Button.ButtonContents -> M.View m a
renderBtnContents _s (Button.TextOnly t) = M.text t
renderBtnContents s (Button.IconOnly i) = Icon.iconS (toIconSize s) i
renderBtnContents _s (Button.SizedIcon sz i) = Icon.iconS sz i
renderBtnContents s (Button.IconText i t) =
  Layout.hFlow
    (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
    [Icon.iconS (toIconSize s) i, MH.span_ [] [M.text t]]

toIconSize :: Button.ButtonSize -> Icon.Size
toIconSize Button.Small = Icon.Small
toIconSize Button.Regular = Icon.Regular
toIconSize Button.Large = Icon.Large
