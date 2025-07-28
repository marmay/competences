module Competences.Frontend.App.Topics
  ( changeUiTopic
  , uiAction
  )
where

import Competences.Frontend.App.Action (UiAction)
import Miso (Effect, Topic, topic, publish)

changeUiTopic :: Topic UiAction
changeUiTopic = topic "changeUiTopic"

uiAction :: UiAction -> Effect m a
uiAction = publish changeUiTopic
