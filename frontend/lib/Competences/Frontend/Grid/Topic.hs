module Competences.Frontend.Grid.Topic
  ( modelCommandTopic
  )
where

import Competences.Command (Command)
import Miso (Topic, topic)

modelCommandTopic :: Topic Command
modelCommandTopic = topic "model-command"
