module Competences.Frontend.App.State
  ( UiState (..)
  , LocalModelState (..)
  , ModelState (..)
  , SessionState (..)
  , State (..)
  , mkState
  )
where

import Competences.Command (Command)
import Competences.Frontend.App.RegisteredComponent (RegisteredComponent)
import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Model (Model, emptyModel)
import Competences.Model.User (User)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Miso.String (MisoString)
import System.Random (StdGen)
import System.Random.Stateful (mkStdGen)

type Key = MisoString

type Token = ByteString

data UiState = UiState
  { modal :: ![(Key, RegisteredComponent, StdGen)]
  , main :: !(Maybe (Key, RegisteredComponent, StdGen))
  , nextKeyIn :: !Int
  }
  deriving (Eq, Show, Generic)

data LocalModelState = LocalModelState
  { localModel :: !Model
  , localChanges :: ![Command]
  }
  deriving (Eq, Show, Generic)

data ModelState = ModelState
  { localModelState :: !(Maybe LocalModelState)
  , remoteModel :: !Model
  }
  deriving (Eq, Show, Generic)

data SessionState = SessionState
  { user :: !User
  , jwtToken :: !Token
  , translationData :: !TranslationData
  , random :: !StdGen
  }
  deriving (Eq, Show, Generic)

data State = State
  { uiState :: !UiState
  , modelState :: !ModelState
  , sessionState :: !SessionState
  }
  deriving (Eq, Show, Generic)

mkState :: User -> Token -> TranslationData -> Int -> State
mkState user jwtToken translationData random =
  State
    { uiState = UiState {modal = [], main = Nothing, nextKeyIn = 0}
    , modelState = ModelState {localModelState = Nothing, remoteModel = emptyModel}
    , sessionState =
        SessionState {user = user, jwtToken = jwtToken, translationData = translationData, random = mkStdGen random}
    }
