module Competences.Frontend.App.State
  ( UiState (..)
  , LocalModelState (..)
  , ModelState (..)
  , State (..)
  )
where

import Competences.Command (Command)
import Competences.Model (Model)
import Miso (SomeComponent)

data UiState = UiState
  { modal :: !(Maybe SomeComponent)
  , grid :: !(Maybe SomeComponent)
  , sidebar :: !(Maybe SomeComponent)
  }

data LocalModelState = LocalModelState
  { localModel :: !Model
  , localChanges :: ![Command]
  }
  deriving (Eq, Show)

data ModelState = ModelState
  { localModelState :: !(Maybe LocalModelState)
  , remoteModel :: !(Maybe Model)
  }
  deriving (Eq, Show)

data State = State
  { uiState :: !UiState
  , modelState :: !ModelState
  }
