module Competences.Frontend.Grid.State
  ( State (..)
  , mkState
  )
where

import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Model (Model, emptyModel)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (User)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Miso.String (MisoString)
import System.Random (StdGen)

data State = State
  { user :: !User
  , translationData :: !TranslationData
  , model :: !Model
  , editFields :: !(M.Map ChangableField MisoString)
  , randomGen :: !StdGen
  }
  deriving (Eq, Generic, Show)

mkState :: User -> TranslationData -> StdGen -> State
mkState user translationData stdGen =
  State
    { user = user
    , translationData = translationData
    , model = emptyModel
    , editFields = M.empty
    , randomGen = stdGen
    }
