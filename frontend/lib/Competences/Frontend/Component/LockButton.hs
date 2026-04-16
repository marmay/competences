-- | Lock-aware edit button component.
--
-- A self-contained component that shows the lock status of an entity
-- and provides actions to lock (click) or steal (hold). Subscribes to
-- document changes and connection status.
--
-- Usage:
--
-- @
-- lockButtonComponent r
--   (LockButtonConfig (EvidenceLock eid) (Evidences (OnEvidences (Modify eid Lock))) Button.IconOnlyS)
-- @
module Competences.Frontend.Component.LockButton
  ( LockButtonConfig (..)
  , Model
  , Action
  , lockButtonComponent
  )
where

import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Frontend.Common qualified as C
import Competences.Document (Document (..), User (..))
import Competences.Document.Lock (Lock, LockHolder (..))
import Competences.Frontend.SyncContext
  ( SyncContext
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , sendCommandOnly
  , subscribeRejections
  , syncDocumentEnv
  )
import Competences.Frontend.SyncContext.ProjectedSubscription
  ( ProjectedChange (..)
  , subscribeWithProjection
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip qualified as Tooltip
import Competences.Frontend.WebSocket.CommandSender
  ( ConnectionChange (..)
  , ConnectionState (..)
  , subscribeConnection
  )
import Control.Concurrent (threadDelay)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Subscription.Util (createSub)

-- | Configuration for a LockButton instance.
data LockButtonConfig = LockButtonConfig
  { lock :: !Lock
  , lockCommand :: !Command
  , style :: !Button.ButtonContentsStyle
  }

-- | Lock status derived from document state.
data LockStatus
  = Free
  | LockedByOther !Text
  -- ^ Display name of the lock holder
  | LockedBySelf
  | LockedByMe
  | StealPending
  deriving (Eq, Show, Generic)

-- | Projection of the document state relevant to this button.
data LockProjection = LockProjection
  { lockHolder :: !(Maybe LockHolder)
  , holderName :: !(Maybe Text)
  -- ^ Display name of the lock holder (if they exist in the document)
  }
  deriving (Eq, Show, Generic)

data Model = Model
  { lockStatus :: !LockStatus
  , holdState :: !(HoldButton.HoldState ())
  , stealError :: !(Maybe Text)
  , connected :: !Bool
  , lastProjection :: !LockProjection
  , stealGen :: !Int
  -- ^ Generation counter for steal timeout disambiguation
  }
  deriving (Eq, Show, Generic)

data Action
  = ProjectionChanged !(ProjectedChange LockProjection)
  | ConnectionChanged !ConnectionChange
  | Click
  | Hold !(HoldButton.HoldAction ())
  | StealRejected !Text
  | StealTimeout !Int
  | DismissError
  deriving (Eq, Show)

lockButtonComponent
  :: SyncContext
  -> LockButtonConfig
  -> M.Component p Model Action
lockButtonComponent r cfg =
  (M.component initModel update view)
    { M.subs =
        [ subscribeWithProjection r (lockProjection cfg.lock) ProjectionChanged
        , subscribeConnection env.commandSender ConnectionChanged
        , rejectionSub r cfg.lock
        ]
    }
  where
    env = syncDocumentEnv r

    initModel :: Model
    initModel = Model Free HoldButton.emptyHoldState Nothing True emptyProjection 0

    emptyProjection :: LockProjection
    emptyProjection = LockProjection Nothing Nothing

    lockProjection :: Lock -> Document -> Maybe User -> LockProjection
    lockProjection lock doc _mFocusedUser =
      let mHolder = Map.lookup lock doc.locks
          mName = do
            holder <- mHolder
            user <- Ix.getOne (doc.users Ix.@= holder.userId)
            pure user.name
       in LockProjection mHolder mName

    deriveLockStatus :: LockProjection -> LockStatus
    deriveLockStatus proj = case proj.lockHolder of
      Nothing -> Free
      Just holder
        | holder.userId == env.connectedUser.id
        , holder.sessionId == env.sessionId -> LockedByMe
        | holder.userId == env.connectedUser.id -> LockedBySelf
        | otherwise -> LockedByOther (fromMaybe "?" proj.holderName)

    update (ProjectionChanged change) =
      M.modify $ \m -> m
        { lockStatus = deriveLockStatus change.projection
        , lastProjection = change.projection
        , stealError = Nothing
        }

    update (ConnectionChanged change) =
      M.modify $ \m -> m { connected = change.state == Connected }

    update Click = do
      m <- M.get
      case m.lockStatus of
        Free | m.connected -> M.io_ $ modifySyncDocument r cfg.lockCommand
        _ -> pure ()

    update (Hold ha) = do
      m <- M.get
      let canSteal = m.connected && case m.lockStatus of
            LockedByOther _ -> True
            LockedBySelf -> True
            _ -> False
      if canSteal
        then HoldButton.handleHoldAction #holdState doSteal Hold ha
        else pure ()
      where
        doSteal () = do
          m <- M.get
          let newGen = m.stealGen + 1
          M.modify $ \m' -> m' { lockStatus = StealPending, stealGen = newGen }
          M.io_ $ do
            sendCommandOnly r (Unlock cfg.lock)
            sendCommandOnly r cfg.lockCommand
          M.io $ threadDelay 10_000_000 >> pure (StealTimeout newGen)

    update (StealRejected err) = do
      m <- M.get
      case m.lockStatus of
        StealPending ->
          let status = deriveLockStatus m.lastProjection
           in do
                M.modify $ \m' -> m' { lockStatus = status, stealError = Just err }
                M.io $ threadDelay 4_000_000 >> pure DismissError
        _ -> pure ()

    update (StealTimeout gen) = do
      m <- M.get
      case m.lockStatus of
        StealPending | m.stealGen == gen ->
          M.modify $ \m' -> m' { lockStatus = deriveLockStatus m.lastProjection }
        _ -> pure ()

    update DismissError =
      M.modify $ \m -> m { stealError = Nothing }

    view :: Model -> M.View Model Action
    view m = case m.lockStatus of
      LockedByMe -> M.text ""

      Free
        | m.connected -> editButton cfg.style
        | otherwise -> disabledIcon cfg.style (C.translate' C.LblDisconnected)

      StealPending -> pendingIcon cfg.style

      LockedByOther name
        | m.connected ->
            withError m.stealError $
              stealButton cfg.style m.holdState $ C.translate' (C.LblStealFrom name)
        | otherwise ->
            lockedIcon cfg.style $ C.translate' (C.LblLockedBy name)

      LockedBySelf
        | m.connected ->
            withError m.stealError $
              stealButton cfg.style m.holdState $ C.translate' C.LblStealFromOtherTab
        | otherwise ->
            lockedIcon cfg.style $ C.translate' C.LblLockedInOtherTab

-- | Subscription that pushes StealRejected when an Unlock for our Lock is rejected.
rejectionSub :: SyncContext -> Lock -> M.Sub Action
rejectionSub r lock sink = createSub acquire release sink
  where
    acquire = subscribeRejections r $ \cmd err ->
      case cmd of
        Unlock l | l == lock -> sink (StealRejected err)
        _ -> pure ()
    release = id

-- | Simple edit button (click to lock).
editButton :: Button.ButtonContentsStyle -> M.View Model Action
editButton s = Button.secondary (Button.button (s, Icon.IcnEdit, C.LblEdit) Click)

-- | Hold-to-steal button with tooltip.
stealButton :: Button.ButtonContentsStyle -> HoldButton.HoldState () -> M.MisoString -> M.View Model Action
stealButton s holdState tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    HoldButton.holdButton Hold holdState () Button.Secondary (styleToButtonSize s)
      (styleToContents s Icon.IcnLock)

-- | Locked icon with tooltip (disconnected, can't steal).
lockedIcon :: Button.ButtonContentsStyle -> M.MisoString -> M.View Model Action
lockedIcon s tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    MH.div_
      [class_ "inline-flex items-center justify-center p-1 text-stone-400"]
      [Icon.iconS (styleToIconSize s) Icon.IcnLock]

-- | Disabled edit icon with tooltip.
disabledIcon :: Button.ButtonContentsStyle -> M.MisoString -> M.View Model Action
disabledIcon s tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    MH.div_
      [class_ "inline-flex items-center justify-center p-1 text-stone-300"]
      [Icon.iconS (styleToIconSize s) Icon.IcnEdit]

-- | Pending steal indicator (pulsing lock).
pendingIcon :: Button.ButtonContentsStyle -> M.View Model Action
pendingIcon s =
  MH.div_
    [class_ "inline-flex items-center justify-center p-1 text-stone-400"]
    [Icon.iconFull Icon.Secondary (styleToIconSize s) Icon.Pulse Icon.IcnLock]

-- | Wrap a view with an error tooltip if present.
withError :: Maybe Text -> M.View m a -> M.View m a
withError Nothing v = v
withError (Just err) v =
  MH.div_ [class_ "relative"]
    [ v
    , MH.div_ [class_ "absolute bottom-full left-1/2 -translate-x-1/2 mb-1 px-2 py-1 bg-red-100 text-red-700 text-xs rounded whitespace-nowrap"]
        [M.text (M.ms err)]
    ]

styleToIconSize :: Button.ButtonContentsStyle -> Icon.Size
styleToIconSize Button.IconOnlyS = Icon.Small
styleToIconSize Button.IconTextS = Icon.Regular
styleToIconSize Button.TextOnlyS = Icon.Regular

styleToButtonSize :: Button.ButtonContentsStyle -> Button.ButtonSize
styleToButtonSize Button.IconOnlyS = Button.Small
styleToButtonSize _ = Button.Regular

styleToContents :: Button.ButtonContentsStyle -> Icon.Icon -> Button.ButtonContents
styleToContents Button.IconOnlyS icn = Button.IconOnly icn
styleToContents _ icn = Button.IconText icn (C.translate' C.LblTakeOver)
