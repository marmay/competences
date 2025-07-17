module Competences.Frontend.State
  ( State (..)
  , mkState
  , modelOf
  )
where

import Competences.Event (ChangableField, Event, EventId)
import Competences.Frontend.Translate (TranslationData)
import Competences.Model (Model)
import Competences.Model.Id (nilId)
import Competences.Model.User (UserId)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Map qualified as M
import Miso.String (MisoString)

data State = State
  { myUserId :: !UserId
  , localModel :: !(Maybe Model)
  , localEvents :: !(M.Map EventId Event)
  , serverModel :: !(Maybe Model)
  , jwtToken :: !ByteString
  , translationData :: !TranslationData
  , editFields :: !(M.Map ChangableField MisoString)
  }
  deriving (Eq, Show)

modelOf :: State -> Maybe Model
modelOf state = state.localModel <|> state.serverModel

mkState :: ByteString -> TranslationData -> State
mkState jwtToken translationData =
  State
    { localModel = Nothing
    , localEvents = M.empty
    , serverModel = Nothing
    , myUserId = nilId
    , jwtToken = jwtToken
    , translationData = translationData
    , editFields = M.empty
    }
