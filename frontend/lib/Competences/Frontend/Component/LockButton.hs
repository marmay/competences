-- | Lock-aware edit button: as a fragment (embeddable) or component (standalone).
--
-- Shows lock status and provides click-to-lock or hold-to-steal.
-- Composes the hold-button fragment internally for the steal workflow.
module Competences.Frontend.Component.LockButton
  ( LockButtonConfig (..)
  , LockState (..)
  , LockAction (..)
  , LockStatus (..)
  , LockProjection (..)
  , lockFragmentDef
  , lockButtonComponent
  , lockView
  , initialLockState
  )
where

import Control.Monad (when)
import Competences.Command (Command (..))
import Competences.Frontend.Common.Effect (FragmentDef (..), GEffect, liftEffect, toComponent)
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

-- ============================================================================
-- Configuration
-- ============================================================================

data LockButtonConfig = LockButtonConfig
  { lock :: !Lock
  , lockCommand :: !Command
  , size :: !Button.ButtonSize
  }

-- ============================================================================
-- State and actions
-- ============================================================================

data LockStatus
  = Free
  | LockedByOther !Text
  | LockedBySelf
  | LockedByMe
  | StealPending
  deriving (Eq, Show, Generic)

data LockProjection = LockProjection
  { lockHolder :: !(Maybe LockHolder)
  , holderName :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

data LockState = LockState
  { lockStatus :: !LockStatus
  , holdState :: !(HoldButton.HoldState ())
  , stealError :: !(Maybe Text)
  , connected :: !Bool
  , lastProjection :: !LockProjection
  , stealGen :: !Int
  }
  deriving (Eq, Show, Generic)

data LockAction
  = ProjectionChanged !(ProjectedChange LockProjection)
  | ConnectionChanged !ConnectionChange
  | Click
  | Hold !(HoldButton.HoldAction ())
  | StealRejected !Text
  | StealTimeout !Int
  | DismissError
  deriving (Eq, Show)

-- ============================================================================
-- Fragment definition
-- ============================================================================

initialLockState :: LockState
initialLockState = LockState Free HoldButton.emptyHoldState Nothing True emptyProjection 0

lockFragmentDef
  :: SyncContext
  -> LockButtonConfig
  -> FragmentDef parent LockState LockAction () ((LockAction -> a) -> M.View m a)
lockFragmentDef r cfg = FragmentDef
  { initialModel = initialLockState
  , update = updateLock r cfg
  , view = lockView cfg.size
  , subs =
      [ subscribeWithProjection r (lockProjection cfg.lock) ProjectionChanged
      , subscribeConnection env.commandSender ConnectionChanged
      , rejectionSub r cfg.lock
      ]
  }
  where
    env = syncDocumentEnv r

-- | Standalone component wrapping the fragment.
lockButtonComponent :: SyncContext -> LockButtonConfig -> M.Component p LockState LockAction
lockButtonComponent r cfg = toComponent (lockFragmentDef r cfg)

-- ============================================================================
-- Update
-- ============================================================================

updateLock :: SyncContext -> LockButtonConfig -> LockAction -> GEffect parent LockState LockAction ()
updateLock r cfg = go
  where
    env = syncDocumentEnv r

    go (ProjectionChanged change) =
      M.modify $ \m -> m
        { lockStatus = deriveLockStatus change.projection
        , lastProjection = change.projection
        , stealError = Nothing
        }

    go (ConnectionChanged change) =
      M.modify $ \m -> m { connected = change.state == Connected }

    go Click = do
      m <- M.get
      case m.lockStatus of
        Free | m.connected -> M.io_ $ modifySyncDocument r cfg.lockCommand
        _ -> pure ()

    go (Hold ha) = do
      m <- M.get
      let canSteal = m.connected && case m.lockStatus of
            LockedByOther _ -> True
            LockedBySelf -> True
            _ -> False
      if canSteal
        then do
          executed <- liftEffect #holdState Hold $
            HoldButton.updateHold (\() -> do
              sendCommandOnly r (Unlock cfg.lock)
              sendCommandOnly r cfg.lockCommand
            ) ha
          when executed $ do
            let newGen = m.stealGen + 1
            M.modify $ \m' -> m' { lockStatus = StealPending, stealGen = newGen }
            M.io $ threadDelay 10_000_000 >> pure (StealTimeout newGen)
        else pure ()

    go (StealRejected err) = do
      m <- M.get
      case m.lockStatus of
        StealPending ->
          let status = deriveLockStatus m.lastProjection
           in do
                M.modify $ \m' -> m' { lockStatus = status, stealError = Just err }
                M.io $ threadDelay 4_000_000 >> pure DismissError
        _ -> pure ()

    go (StealTimeout gen) = do
      m <- M.get
      case m.lockStatus of
        StealPending | m.stealGen == gen ->
          M.modify $ \m' -> m' { lockStatus = deriveLockStatus m.lastProjection }
        _ -> pure ()

    go DismissError =
      M.modify $ \m -> m { stealError = Nothing }

    deriveLockStatus :: LockProjection -> LockStatus
    deriveLockStatus proj = case proj.lockHolder of
      Nothing -> Free
      Just holder
        | holder.userId == env.connectedUser.id
        , holder.sessionId == env.sessionId -> LockedByMe
        | holder.userId == env.connectedUser.id -> LockedBySelf
        | otherwise -> LockedByOther (fromMaybe "?" proj.holderName)

-- ============================================================================
-- View
-- ============================================================================

lockView :: Button.ButtonSize -> LockState -> (LockAction -> a) -> M.View m a
lockView sz m liftAction = case m.lockStatus of
  LockedByMe -> M.text ""

  Free
    | m.connected -> editButton sz liftAction
    | otherwise -> disabledIcon sz (C.translate' C.LblDisconnected)

  StealPending -> pendingIcon sz

  LockedByOther name
    | m.connected ->
        withError m.stealError $
          stealButton sz m.holdState liftAction $ C.translate' (C.LblStealFrom name)
    | otherwise ->
        lockedIcon sz $ C.translate' (C.LblLockedBy name)

  LockedBySelf
    | m.connected ->
        withError m.stealError $
          stealButton sz m.holdState liftAction $ C.translate' C.LblStealFromOtherTab
    | otherwise ->
        lockedIcon sz $ C.translate' C.LblLockedInOtherTab

-- ============================================================================
-- Helpers
-- ============================================================================

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

rejectionSub :: SyncContext -> Lock -> M.Sub LockAction
rejectionSub r lock sink = createSub acquire release sink
  where
    acquire = subscribeRejections r $ \cmd err ->
      case cmd of
        Unlock l | l == lock -> sink (StealRejected err)
        _ -> pure ()
    release = id

editButton :: Button.ButtonSize -> (LockAction -> a) -> M.View m a
editButton sz liftAction = Button.render Button.Secondary sz (Button.button (Icon.IcnEdit, C.LblEdit) (Just (liftAction Click)))

stealButton :: Button.ButtonSize -> HoldButton.HoldState () -> (LockAction -> a) -> M.MisoString -> M.View m a
stealButton sz holdState liftAction tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    HoldButton.holdButton (liftAction . Hold) holdState () Button.Secondary sz
      (Button.IconText Icon.IcnLock (C.translate' C.LblTakeOver))

lockedIcon :: Button.ButtonSize -> M.MisoString -> M.View m a
lockedIcon sz tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    Button.renderDisabled sz (Button.IconText Icon.IcnLock (C.translate' C.LblTakeOver))

disabledIcon :: Button.ButtonSize -> M.MisoString -> M.View m a
disabledIcon sz tooltip =
  Tooltip.withTooltip (Tooltip.PlainTooltip tooltip) $
    Button.renderDisabled sz (Button.IconText Icon.IcnEdit (C.translate' C.LblEdit))

pendingIcon :: Button.ButtonSize -> M.View m a
pendingIcon sz =
  Button.renderDisabledPulse sz (Button.IconText Icon.IcnLock (C.translate' C.LblTakeOver))

withError :: Maybe Text -> M.View m a -> M.View m a
withError Nothing v = v
withError (Just err) v =
  MH.div_ [class_ "relative"]
    [ v
    , MH.div_ [class_ "absolute bottom-full left-1/2 -translate-x-1/2 mb-1 px-2 py-1 bg-red-100 text-red-700 text-xs rounded whitespace-nowrap"]
        [M.text (M.ms err)]
    ]

