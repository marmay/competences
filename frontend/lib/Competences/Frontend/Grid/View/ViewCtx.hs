module Competences.Frontend.Grid.View.ViewCtx
  ( ViewCtx (..)
  , ViewM
  , mkViewCtx
  , runViewM
  , withViewCtx
  )
where

import Competences.Frontend.Common.Translate (Label, translate)
import Competences.Frontend.Grid.State (State (..), modelOf)
import Competences.Model (Model)
import Control.Monad.Reader (Reader, ask, runReader)
import Miso.String (MisoString)

data ViewCtx = ViewCtx
  { renderModel :: !Model
  , state :: !State
  , translate :: !(Label -> MisoString)
  }

type ViewM = Reader ViewCtx

runViewM :: ViewCtx -> ViewM a -> a
runViewM ctx a = runReader a ctx

withViewCtx :: (ViewCtx -> ViewM a) -> ViewM a
withViewCtx f = ask >>= f

mkViewCtx :: State -> Maybe ViewCtx
mkViewCtx s = do
  renderModel <- modelOf s
  pure $ ViewCtx {renderModel = renderModel, state = s, translate = translate s.translationData}
