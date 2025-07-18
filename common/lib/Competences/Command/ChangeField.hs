module Competences.Command.ChangeField
  ( lockField
  , releaseField
  )
where

import Competences.Command.Common (UpdateResult, AffectedUsers (..))
import Competences.Model (Model(..), fieldATraversal)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (UserId)
import Data.Text (Text)
import Optics.Core ((&), (%~), matching)
import qualified Data.Map as M
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)

lockField :: Model -> ChangableField -> UserId -> Text -> UpdateResult
lockField model field userId expectedText = do
  when (field `M.member` model.lockedFields) $
    Left "field already locked"
  let current = matching (fieldATraversal field) model
  when (isLeft current) $
    Left "field no longer exists"
  when (current /= Right expectedText) $
    Left "field has changed in the meantime"

  let model' = model & #lockedFields %~ M.insert field userId
  pure (model', AllUsers)

releaseField :: Model -> ChangableField -> Maybe Text -> UpdateResult
releaseField model field text = do
  when (field `M.notMember` model.lockedFields) $
    Left "field not locked"

  let model' = model & #lockedFields %~ M.delete field
                     & fieldATraversal field %~ flip fromMaybe text
  pure (model', AllUsers)
