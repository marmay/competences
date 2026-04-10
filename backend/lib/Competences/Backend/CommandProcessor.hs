-- | Single-threaded command processor with per-client output queues.
--
-- Replaces the shared CommandLog. The processor applies commands to the
-- document TVar and pushes results to per-client TQueues in a single STM
-- transaction. This eliminates the race condition where senderThreads could
-- read a document state inconsistent with the commands they received.
--
-- Architecture:
--   Client receiveLoops → [input TBQueue] → Processor → per-client TQueues → senderThreads
--                                                ↓
--                                           DB persistence (after STM, optimistic)
module Competences.Backend.CommandProcessor
  ( ClientQueueItem (..)
  , CommandProcessor
  , ConnectionId (..)
  , startProcessor
  , submitCommand
  , registerClient
  , unregisterClient
  )
where

import Competences.Backend.Database qualified as DB
import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..), ModifyCommand (..), handleCommand)
import Competences.Document.Session (legacySessionId)
import Competences.Command.Audience (CommandAudience (..), commandAudience)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), UserRole (..))
import Competences.Document.Id (Id (..))
import Competences.Document.User (UserId)
import Competences.Protocol (CommandId)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TBQueue
  , TMVar
  , TQueue
  , TVar
  , atomically
  , newEmptyTMVarIO
  , newTBQueueIO
  , putTMVar
  , readTBQueue
  , readTVar
  , takeTMVar
  , writeTBQueue
  , writeTQueue
  , writeTVar
  )
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple (Connection)

-- | An item pushed to a client's output queue.
data ClientQueueItem = ClientQueueItem
  { commandId :: !CommandId
  , userId :: !UserId
  , command :: !Command
  , checkpoint :: !Bool
  -- ^ True when the client should compute and validate a checksum
  }

-- | Internal state for a registered client.
data ClientState = ClientState
  { role :: !UserRole
  , clientUserId :: !UserId
  , queue :: !(TQueue ClientQueueItem)
  , commandCounter :: !Int
  -- ^ Commands sent since last checkpoint
  }

-- | A request to the processor.
data ProcessorRequest
  = SubmitCommand !UserId !Command !(TMVar (Either Text CommandId))
  | RegisterClient !ConnectionId !UserRole !UserId !(TQueue ClientQueueItem)
  | UnregisterClient !ConnectionId

-- | Opaque connection ID.
newtype ConnectionId = ConnectionId Int
  deriving (Eq, Ord, Show)

-- | Opaque handle to the command processor.
data CommandProcessor = CommandProcessor
  { inputQueue :: !(TBQueue ProcessorRequest)
  }

-- | Number of commands between checkpoints.
checksumInterval :: Int
checksumInterval = 50

-- | Start the command processor. Spawns a background thread.
startProcessor :: TVar Document -> TVar Int64 -> Pool Connection -> IO CommandProcessor
startProcessor docVar genVar pool = do
  inputQ <- newTBQueueIO 256
  clientsRef <- newIORef Map.empty
  let proc = CommandProcessor inputQ
  _ <- forkIO $ processorLoop inputQ docVar genVar pool clientsRef
  pure proc

-- | Submit a command for processing. Blocks until the command is applied (or rejected).
-- Returns the CommandId on success.
submitCommand :: CommandProcessor -> UserId -> Command -> IO (Either Text CommandId)
submitCommand proc uid cmd = do
  responseVar <- newEmptyTMVarIO
  atomically $ writeTBQueue proc.inputQueue (SubmitCommand uid cmd responseVar)
  atomically $ takeTMVar responseVar

-- | Register a client's output queue with the processor.
registerClient :: CommandProcessor -> ConnectionId -> UserRole -> UserId -> TQueue ClientQueueItem -> IO ()
registerClient proc connId role' uid queue' =
  atomically $ writeTBQueue proc.inputQueue (RegisterClient connId role' uid queue')

-- | Unregister a client from the processor.
unregisterClient :: CommandProcessor -> ConnectionId -> IO ()
unregisterClient proc connId =
  atomically $ writeTBQueue proc.inputQueue (UnregisterClient connId)

-- | The main processor loop. Single-threaded: reads requests and processes them.
processorLoop
  :: TBQueue ProcessorRequest
  -> TVar Document
  -> TVar Int64
  -> Pool Connection
  -> IORef (Map ConnectionId ClientState)
  -> IO ()
processorLoop inputQ docVar genVar pool clientsRef = go
  where
    go = do
      req <- atomically $ readTBQueue inputQ
      case req of
        SubmitCommand uid cmd responseVar -> do
          handleSubmit uid cmd responseVar
          go
        RegisterClient connId role' uid queue' -> do
          let cs = ClientState
                { role = role'
                , clientUserId = uid
                , queue = queue'
                , commandCounter = 0
                }
          modifyIORef' clientsRef (Map.insert connId cs)
          go
        UnregisterClient connId -> do
          modifyIORef' clientsRef (Map.delete connId)
          go

    handleSubmit :: UserId -> Command -> TMVar (Either Text CommandId) -> IO ()
    handleSubmit uid cmd responseVar = do
      -- Pre-generate CommandId
      cmdUuid <- UUID.nextRandom
      let cmdId = Id cmdUuid

      -- Read current clients (safe: processor is single-threaded)
      clients <- readIORef clientsRef

      -- Determine audience and recipients
      let audience = commandAudience cmd

      -- Compute which clients should receive this command and their checkpoint flags.
      -- We do this BEFORE the STM transaction so we know exactly what to push.
      let clientActions = Map.mapMaybe
            (\cs ->
              if isVisibleTo cs.role cs.clientUserId audience
                then
                  let newCount = cs.commandCounter + 1
                      isCheckpoint = newCount >= checksumInterval
                  in Just (cs.queue, newCount, isCheckpoint, cs.clientUserId)
                else Nothing
            )
            clients

      -- Single STM transaction: apply command + push to all affected client queues.
      -- SenderThreads read queue + docVar atomically → always consistent.
      result <- atomically $ do
        doc <- readTVar docVar
        case handleCommand uid legacySessionId cmd doc of
          Left err -> do
            putTMVar responseVar (Left err)
            pure Nothing
          Right (doc', _affected) -> do
            writeTVar docVar doc'
            let cmdsFor = clientCommands doc cmd
                pushItem q ck c = writeTQueue q ClientQueueItem
                  { commandId = cmdId, userId = uid, command = c, checkpoint = ck }
            -- Push per-client commands; checkpoint flag goes on the last item
            mapM_
              (\(_connId, (q, _count, isCheckpoint, clientUid)) ->
                case cmdsFor clientUid of
                  [single] -> pushItem q isCheckpoint single
                  cmds -> do
                    mapM_ (pushItem q False) (init cmds)
                    pushItem q isCheckpoint (last cmds)
              )
              (Map.toList clientActions)
            putTMVar responseVar (Right cmdId)
            pure (Just ())

      -- Post-STM: update counters (single-threaded, no race)
      case result of
        Nothing -> pure ()
        Just () -> do
          -- Update command counters for clients that received the command
          let updateCounter connId cs = case Map.lookup connId clientActions of
                Just (_, newCount, isCheckpoint, _) ->
                  cs { commandCounter = if isCheckpoint then 0 else newCount }
                Nothing -> cs
          modifyIORef' clientsRef (Map.mapWithKey updateCounter)

          -- Persist to DB (optimistic, after responding to client)
          generation <- DB.saveCommandWithAudience pool cmdId uid cmd audience
          atomically $ writeTVar genVar generation

          -- Check snapshot
          shouldSnapshot <- DB.shouldTakeSnapshot pool generation
          when shouldSnapshot $ do
            putStrLn $ "Taking snapshot at generation " <> show generation
            doc'' <- atomically $ readTVar docVar
            DB.saveSnapshot pool doc'' generation

-- | Check if a command is visible to a specific user based on audience.
isVisibleTo :: UserRole -> UserId -> CommandAudience -> Bool
isVisibleTo Teacher uid aud = case aud of
  AudienceAll -> True
  AudienceTeachers -> True
  AudienceTeachersAnd _ -> True
  AudienceOnly recips -> uid `elem` recips
isVisibleTo Student uid aud = case aud of
  AudienceAll -> True
  AudienceTeachers -> False
  AudienceTeachersAnd recips -> uid `elem` recips
  AudienceOnly recips -> uid `elem` recips

-- | Compute the command list a specific client should receive.
-- When assignment studentIds change, newly added students get a supplementary
-- Create before the Modify (so the entity exists when the patch applies),
-- and removed students get a Delete after (to clean up the now-invisible entity).
clientCommands :: Document -> Command -> UserId -> [Command]
clientCommands oldDoc cmd@(Assignments (OnAssignments (Modify aid (Release patch)))) =
  case patch.studentIds of
    Just (oldIds, newIds)
      | oldIds /= newIds
      , Just preAssignment <- Ix.getOne (oldDoc.assignments Ix.@= aid) ->
          let added = Set.difference newIds oldIds
              removed = Set.difference oldIds newIds
           in \clientUid ->
                [Assignments (OnAssignments (Create preAssignment)) | clientUid `Set.member` added]
                  ++ [cmd]
                  ++ [Assignments (OnAssignments (Delete aid)) | clientUid `Set.member` removed]
    _ -> const [cmd]
clientCommands _ cmd = const [cmd]
