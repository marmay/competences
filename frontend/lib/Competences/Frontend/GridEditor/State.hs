module Competences.Frontend.GridEditor.State
  ( State (..)
  , mkState
  )
where

import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Document (Document, emptyDocument)
import Competences.Document.ChangableField (ChangableField)
import Competences.Document.User (User)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Miso.String (MisoString)
import System.Random (StdGen)
import Competences.Document.Competence (Competence)
import Miso (ComponentId)

data State = State
  { user :: !User
  , translationData :: !TranslationData
  , model :: !Document
  , editFields :: !(M.Map ChangableField MisoString)
  , newCompetence :: !(Maybe Competence)
  , random :: !StdGen
  , componentId :: !(Maybe ComponentId)
  }
  deriving (Eq, Generic, Show)

mkState :: User -> TranslationData -> StdGen -> State
mkState user translationData stdGen =
  State
    { user = user
    , translationData = translationData
    , model = emptyDocument
    , editFields = M.empty
    , newCompetence = Nothing
    , random = stdGen
    , componentId = Nothing
    }
