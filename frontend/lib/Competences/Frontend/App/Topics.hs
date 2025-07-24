module Competences.Frontend.App.Topics
  ( changeApplicationStateTopic
  , modelChangedTopic
  )
where

import Competences.Command (Command)
import Competences.Frontend.App.Action (Action)
import Data.Text (Text)
import Miso (Topic, topic)

changeApplicationStateTopic :: Topic Action
changeApplicationStateTopic = topic "changeApplicationState"

modelChangedTopic :: Topic (Command, Text)
modelChangedTopic = topic "modelChanged"
