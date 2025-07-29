module Competences.Frontend.App.State
  ( UiState (..)
  , SessionState (..)
  , State (..)
  , mkState
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent)
import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Document.User (User)
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

data SessionState = SessionState
  { user :: !User
  , jwtToken :: !Token
  , translationData :: !TranslationData
  , random :: !StdGen
  }
  deriving (Eq, Show, Generic)

data State = State
  { uiState :: !UiState
  , sessionState :: !SessionState
  }
  deriving (Eq, Show, Generic)

mkState :: User -> Token -> TranslationData -> Int -> State
mkState user jwtToken translationData random =
  State
    { uiState = UiState {modal = [], main = Nothing, nextKeyIn = 0}
    , sessionState =
        SessionState {user = user, jwtToken = jwtToken, translationData = translationData, random = mkStdGen random}
    }
