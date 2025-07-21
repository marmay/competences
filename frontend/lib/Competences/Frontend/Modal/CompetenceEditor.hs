module Competences.Frontend.Modal.CompetenceEditor
  ( competenceEditor
  )
where

import Competences.Model.Competence (Competence (..), Level (..))
import Miso (Component, Effect, component, get, text)
import Miso.Html
import Miso.State (modify)
import Miso.String (MisoString)
import Miso.Types (View)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Action
  = SetDescription !MisoString
  | SetBasicLevelDescription !MisoString
  | SetIntermediateLevelDescription !MisoString
  | SetAdvancedLevelDescription !MisoString
  | CreateCompetence
  | CancelCreation
  deriving (Eq, Show)

data State = State
  { description_ :: !MisoString
  , basicLevelDescription :: !MisoString
  , intermediateLevelDescription :: !MisoString
  , advancedLevelDescription :: !MisoString
  }
  deriving (Eq, Show)

stateFromCompetence :: Competence -> State
stateFromCompetence c =
  State { description_ = c.description
        , basicLevelDescription = fromMaybe "" $ Map.lookup BasicLevel c.levelDescriptions
        , intermediateLevelDescription = fromMaybe "" $ Map.lookup IntermediateLevel c.levelDescriptions
        , advancedLevelDescription = fromMaybe "" $ Map.lookup AdvancedLevel c.levelDescriptions
        }

competenceEditor
  :: Competence
  -> (Maybe Competence -> Effect State Action)
  -> Component State Action
competenceEditor competence onFinish =
  component (stateFromCompetence competence) update view
  where
    update :: Action -> Effect State Action
    update (SetDescription d) = modify $ \s -> s {description_ = d}
    update (SetBasicLevelDescription d) = modify $ \s -> s {basicLevelDescription = d}
    update (SetIntermediateLevelDescription d) = modify $ \s -> s {intermediateLevelDescription = d}
    update (SetAdvancedLevelDescription d) = modify $ \s -> s {advancedLevelDescription = d}
    update CreateCompetence = do
      s <- get
      onFinish $
        Just $
          Competence
            { id = competence.id
            , competenceGridId = competence.competenceGridId
            , description = s.description_
            , levelDescriptions = undefined
            }
    update CancelCreation = onFinish Nothing

    view :: State -> View Action
    view s =
      div_
        []
        [ h1_ [] [text "Add a new competence"]
        , input_ [placeholder_ "Title", value_ s.description_, onInput SetDescription]
        , input_
            [placeholder_ "Basic description", value_ s.basicLevelDescription, onInput SetBasicLevelDescription]
        , input_
            [ placeholder_ "Intermediate description"
            , value_ s.intermediateLevelDescription
            , onInput SetIntermediateLevelDescription
            ]
        , input_
            [ placeholder_ "Advanced description"
            , value_ s.advancedLevelDescription
            , onInput SetAdvancedLevelDescription
            ]
        , button_ [onClick CreateCompetence] [text "Add competence"]
        , button_ [onClick CancelCreation] [text "Cancel"]
        ]
