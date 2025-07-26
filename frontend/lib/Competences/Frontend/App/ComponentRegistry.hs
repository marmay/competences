module Competences.Frontend.App.ComponentRegistry
  ( ComponentRegistry (..)
  , ChannelId
  , LoadModelAction (..)
  , addComponent
  , handleChannelMessages
  , loadModel
  , mkComponentRegistry
  , updateModel
  , removeComponent
  )
where

import Competences.Model (Model)
import Control.Concurrent.STM (TChan)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Miso (ComponentId, Effect, io_, io)
import Optics.Core ((%~), (&), (.~))
import Control.Monad.IO.Class (liftIO)

type ChannelId = Int

data ComponentRegistry = ComponentRegistry
  { sender :: !(TChan Model)
  , channelMap :: !(M.Map ChannelId (TChan Model))
  , nextComponentId :: !ChannelId
  , currentModel :: !(Maybe Model)
  }
  deriving (Generic)

mkComponentRegistry :: IO ComponentRegistry
mkComponentRegistry = do
  sender <- STM.newBroadcastTChanIO
  pure $ ComponentRegistry sender M.empty 0 Nothing

addComponent :: IORef ComponentRegistry -> IO (ComponentId, Maybe Model)
addComponent reg = do
  reg' <- readIORef reg
  let nextId = reg'.nextComponentId
  let componentId = nextId
  chan <- STM.atomically $ STM.dupTChan reg'.sender
  writeIORef reg $
    reg'
      & (#nextComponentId %~ (+ 1))
      & (#channelMap %~ M.insert componentId chan)
  pure (componentId, reg'.currentModel)

removeComponent :: IORef ComponentRegistry -> ChannelId -> IO ()
removeComponent reg channelId =
  modifyIORef' reg $
    (#channelMap %~ M.delete channelId)

updateModel :: IORef ComponentRegistry -> Model -> IO [ChannelId]
updateModel reg model = do
  reg' <- readIORef reg
  STM.atomically $ STM.writeTChan reg'.sender model
  writeIORef reg $ reg' & (#currentModel .~ Just model)
  pure $ M.keys reg'.channelMap

loadModel :: (LoadModelAction a) => IORef ComponentRegistry -> ChannelId -> Effect m a
loadModel reg chanId = io $ liftIO $ do
  reg' <- readIORef reg
  let chan = reg'.channelMap M.! chanId
  model <- STM.atomically $ STM.readTChan chan
  pure $ updateModelAction model

data ComponentRegistryMessage
  = ModelUpdated !ChannelId
  deriving (Eq, Show, Generic)

instance ToJSON ComponentRegistryMessage

instance FromJSON ComponentRegistryMessage

class LoadModelAction a where
  loadModelAction :: ChannelId -> a
  updateModelAction :: Model -> a

handleChannelMessages
  :: (LoadModelAction a) => Value -> Maybe a
handleChannelMessages v =
  case fromJSON v of
    Error _ -> Nothing
    Success (ModelUpdated chan) ->
      Just $ loadModelAction chan
