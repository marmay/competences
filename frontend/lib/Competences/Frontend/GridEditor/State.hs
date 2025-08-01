module Competences.Frontend.GridEditor.State
  ( State (..)
  , mkState
  )
where

import Competences.Document (Document, emptyDocument)
import Competences.Document.ChangableField (ChangableField)
import Competences.Document.User (User)
import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Frontend.Component.CompetenceEditor qualified as CE
import Data.Map qualified as M
import GHC.Generics (Generic)
import Miso.String (MisoString)
import System.Random (StdGen)

data State = State
  { user :: !User
  , translationData :: !TranslationData
  , model :: !Document
  , editFields :: !(M.Map ChangableField MisoString)
  , random :: !StdGen
  , newCompetenceEditor :: !(Maybe CE.Model)
  }
  deriving (Eq, Generic, Show)

mkState :: User -> TranslationData -> StdGen -> State
mkState user translationData stdGen =
  State
    { user = user
    , translationData = translationData
    , model = emptyDocument
    , editFields = M.empty
    , random = stdGen
    , newCompetenceEditor = Nothing
    }
