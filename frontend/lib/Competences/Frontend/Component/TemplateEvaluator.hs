module Competences.Frontend.Component.TemplateEvaluator
  ( templateEvaluatorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Template, TemplateEvaluation (..), TemplateName)
import Competences.Document.Evidence (Ability)
import Competences.Document.Template (TemplateAspect)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((%~), (.~), (?~))

data Model = Model
  { templates :: ![Template]
  , evaluation :: !(Maybe TemplateEvaluation)
  }
  deriving (Eq, Generic, Show)

data Action
  = SetTemplate !Template
  | Assess !Int !(Maybe Ability)
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

templateEvaluatorComponent :: SyncContext -> M.Component p Model Action
templateEvaluatorComponent r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model [] Nothing

    update :: Action -> M.Effect p Model Action
    update (SetTemplate t) = M.modify $ #evaluation ?~ evaluateFromTemplate t
    update (Assess i a) = M.modify $ #evaluation %~ fmap (assessAt i a)
    update (UpdateDocument (DocumentChange newDocument _)) =
      M.modify $ #templates .~ Ix.toAscList (Proxy @TemplateName) newDocument.templates

    view _m = V.empty

evaluateFromTemplate :: Template -> TemplateEvaluation
evaluateFromTemplate = undefined

assessAt :: Int -> Maybe Ability -> TemplateEvaluation -> TemplateEvaluation
assessAt i a = #assessments %~ assessAt' i
  where
    assessAt' :: Int -> [(TemplateAspect, Maybe Ability)] -> [(TemplateAspect, Maybe Ability)]
    assessAt' 0 ((t, _) : ts) = (t, a) : ts
    assessAt' n (t : ts)
      | n > 0 = t : assessAt' (n - 1) ts
      | otherwise = t : ts
    assessAt' _ [] = []
