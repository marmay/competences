module Competences.Frontend.App.Topics
  ( changeApplicationStateTopic
  , componentAction
  )
where

import Competences.Frontend.App.Action (ComponentAction)
import Miso (Effect, Topic, topic, publish, io_, consoleLog)
import Miso.String (ms)

changeApplicationStateTopic :: Topic ComponentAction
changeApplicationStateTopic = topic "changeApplicationState"

componentAction :: ComponentAction -> Effect m a
componentAction a = do
  io_ $ consoleLog $ ms $ show a
  publish changeApplicationStateTopic a
