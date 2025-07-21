module Competences.Frontend.Grid.State
  ( State (..)
  , mkState
  , modelOf
  )
where

import Competences.Command (Command, CommandId)
import Competences.Frontend.Common.Translate (TranslationData)
import Competences.Model (Model)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (User)
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Map qualified as M
import Miso.String (MisoString)
import System.Random (StdGen, mkStdGen)

data State = State
  { user :: !User
  , localModel :: !(Maybe Model)
  , localEvents :: !(M.Map CommandId Command)
  , serverModel :: !(Maybe Model)
  , jwtToken :: !ByteString
  , translationData :: !TranslationData
  , editFields :: !(M.Map ChangableField MisoString)
  , randomGen :: !StdGen
  }
  deriving (Eq, Show)

modelOf :: State -> Maybe Model
modelOf state = state.localModel <|> state.serverModel

mkState :: User -> ByteString -> TranslationData -> Int -> State
mkState user jwtToken translationData seed =
  State
    { user = user
    , localModel = Nothing
    , localEvents = M.empty
    , serverModel = Nothing
    , jwtToken = jwtToken
    , translationData = translationData
    , editFields = M.empty
    , randomGen = mkStdGen seed
    }
