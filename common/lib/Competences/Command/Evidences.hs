{-# LANGUAGE CPP #-}

module Competences.Command.Evidences
  ( EvidencesCommand (..)
  , EvidencePatch (..)
  , handleEvidencesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, CommandContext (..), EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Common.IxSet qualified as Ix
import Data.Default (Default (..))
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (UserId)
import Competences.Document.Evidence
  ( ActivityType
  , Evidence (..)
  , Observation
  , ObservationIxs
  , TaskEvaluations
  , TaskRemark
  )
import Competences.Document.Task (TaskId)
import Data.Map.Strict (Map)
import Data.Set (Set)
#ifdef WITH_AESON
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Object, ToJSON, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (^.))
import Control.Monad ((>=>))

-- | Patch for modifying an Evidence (only editable fields)
data EvidencePatch = EvidencePatch
  { userId :: !(Change (Maybe UserId))
    -- ^ Change userId from old to new value
  , activityType :: !(Change ActivityType)
    -- ^ Change activityType from old to new value
  , date :: !(Change Day)
    -- ^ Change date from old to new value
  , tasks :: !(Change (Map TaskId TaskEvaluations))
    -- ^ Change tasks (with per-competence evaluations) from old to new value
  , oldTasks :: !(Change Text)
    -- ^ Change oldTasks from old to new value
  , observations :: !(Change (Ix.IxSet ObservationIxs Observation))
    -- ^ Change observations from old to new value
  , taskRemarks :: !(Change (Map TaskId (Set TaskRemark)))
    -- ^ Change taskRemarks from old to new value
  , assignmentId :: !(Change (Maybe AssignmentId))
    -- ^ Change assignmentId from old to new value
  , lessonId :: !(Change (Maybe LessonId))
    -- ^ Change lessonId from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Evidences context
data EvidencesCommand
  = OnEvidences !(EntityCommand Evidence EvidencePatch)
  deriving (Eq, Generic, Show)

instance Binary EvidencePatch
#ifdef WITH_AESON
instance FromJSON EvidencePatch where
  parseJSON = withObject "EvidencePatch" $ \v -> do
    tasksChange <- parseTasksChange v
    EvidencePatch
      <$> v .: "userId"
      <*> v .: "activityType"
      <*> v .: "date"
      <*> pure tasksChange
      <*> v .:? "oldTasks" .!= Nothing
      <*> v .: "observations"
      <*> v .:? "taskRemarks" .!= Nothing
      <*> v .:? "assignmentId" .!= Nothing
      <*> v .:? "lessonId" .!= Nothing

-- | Parse the tasks Change field, handling both old and new formats.
-- Old format: Change [TaskId] (array of task ID lists)
-- New format: Change (Map TaskId TaskEvaluations) (map with evaluation data)
parseTasksChange :: Object -> Parser (Change (Map TaskId TaskEvaluations))
parseTasksChange v = (v .: "tasks") <|> parseOldTasksChange v

-- | Convert old tasks format: Change [TaskId] → Change (Map TaskId TaskEvaluations)
parseOldTasksChange :: Object -> Parser (Change (Map TaskId TaskEvaluations))
parseOldTasksChange v = do
  old <- v .: "tasks" :: Parser (Change [TaskId])
  pure $ fmap (\(before, after) ->
    ( Map.fromList [(tid, Map.empty) | tid <- before]
    , Map.fromList [(tid, Map.empty) | tid <- after]
    )) old

instance ToJSON EvidencePatch
#endif

instance Binary EvidencesCommand
#ifdef WITH_AESON
instance FromJSON EvidencesCommand
instance ToJSON EvidencesCommand
#endif

-- Default instances
instance Default EvidencePatch where
  def =
    EvidencePatch
      { userId = Nothing
      , activityType = Nothing
      , date = Nothing
      , tasks = Nothing
      , oldTasks = Nothing
      , observations = Nothing
      , taskRemarks = Nothing
      , assignmentId = Nothing
      , lessonId = Nothing
      }

-- | Apply a patch to an Evidence, checking for conflicts
applyEvidencePatch :: Evidence -> EvidencePatch -> Either Text Evidence
applyEvidencePatch evidence patch =
  inContext "Evidence" evidence $
    patchField' @"userId" patch
      >=> patchField' @"activityType" patch
      >=> patchField' @"date" patch
      >=> patchField' @"tasks" patch
      >=> patchField' @"oldTasks" patch
      >=> patchField' @"observations" patch
      >=> patchField' @"taskRemarks" patch
      >=> patchField' @"assignmentId" patch
      >=> patchField' @"lessonId" patch

-- | Handle an Evidences context command
handleEvidencesCommand :: CommandContext -> EvidencesCommand -> Document -> UpdateResult
handleEvidencesCommand cmdCtx (OnEvidences c) = interpretEntityCommand evidenceContext cmdCtx c
  where
    evidenceContext =
      mkEntityCommandContext
        #evidences
        #id
        EvidenceLock
        applyEvidencePatch
        (\e d' -> allTeachersAnd d' (maybeToList e.userId))
    allTeachersAnd d' us =
      AffectedUsers $
        map (.id) $
          IxSet.toList (d' ^. #users) & filter (\u -> u.id `elem` us || u.role == Teacher)
