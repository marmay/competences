module Competences.Frontend.Grid.State
  ( State (..)
  , NewCompetenceData (..)
  , emptyNewCompetenceData
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
  , newCompetenceData :: !(Maybe NewCompetenceData)
  , random :: !StdGen
  }
  deriving (Eq, Generic, Show)

data NewCompetenceData = NewCompetenceData
  { description :: !MisoString
  , basicLevelDescription :: !MisoString
  , intermediateLevelDescription :: !MisoString
  , advancedLevelDescription :: !MisoString
  }
  deriving (Eq, Generic, Show)

emptyNewCompetenceData :: NewCompetenceData
emptyNewCompetenceData = NewCompetenceData "" "" "" ""

mkState :: User -> TranslationData -> StdGen -> State
mkState user translationData stdGen =
  State
    { user = user
    , translationData = translationData
    , model = emptyModel
    , editFields = M.empty
    , newCompetenceData = Nothing
    , random = stdGen
    }
