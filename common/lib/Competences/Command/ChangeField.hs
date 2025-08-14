module Competences.Command.ChangeField
  ( canChangeField
  , lockField
  , releaseField
  )
where

import Competences.Command.Common (AffectedUsers (..), UpdateResult)
import Competences.Document (Document (..), PartialChecksumId (..), updateChecksums)
import Competences.Document.ChangableField (ChangableField (..))
import Competences.Document.User (UserId, UserRole (..))
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Optics.Core (matching, ix, (%~), (&))

-- | Whether a given role is allowed to change a given field.
canChangeField :: ChangableField -> UserRole -> Bool
canChangeField _ Teacher = True
canChangeField _ _ = False

-- | Locks a field with a given content for changes by a given
-- user.
lockField :: Document -> ChangableField -> UserId -> Text -> UpdateResult
lockField model field userId expectedText = do
  when (field `M.member` model.lockedFields) $
    Left "field already locked"
  let current = matching (ix field) model
  when (isLeft current) $
    Left "field no longer exists"
  when (current /= Right expectedText) $
    Left "field has changed in the meantime"

  let model' = model & #lockedFields %~ M.insert field userId
  pure (updateChecksums model' [PC_LockedFields], AllUsers)

-- | Releases a locked field and changes its content.
releaseField :: Document -> ChangableField -> Maybe Text -> UpdateResult
releaseField model field text = do
  when (field `M.notMember` model.lockedFields) $
    Left "field not locked"

  let model' =
        model
          & (#lockedFields %~ M.delete field)
          & (ix field %~ flip fromMaybe text)
  pure (updateChecksums model' (partialChecksumIdsForChangableField field), AllUsers)

partialChecksumIdsForChangableField :: ChangableField -> [PartialChecksumId]
partialChecksumIdsForChangableField CompetenceGridTitle = [PC_CompetenceGrid]
partialChecksumIdsForChangableField CompetenceGridDescription = [PC_CompetenceGrid]
partialChecksumIdsForChangableField (CompetenceDescription _) = [PC_Competences]
partialChecksumIdsForChangableField (CompetenceLevelDescription _) = [PC_Competences]
