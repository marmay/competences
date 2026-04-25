{-# LANGUAGE OverloadedLabels #-}

-- | Pure builders that project a 'Document' (or part of it) into the
-- structured 'ExchangeDoc' IR. Lives in @common@ so both the frontend
-- (which constructs the IR for export) and any tooling that wants
-- to round-trip a Document programmatically can use it.
module Competences.Exchange.Build
  ( -- * Single-entity entry points
    assignmentExchange
  , taskExchange
  , resourceExchange
  , lessonExchange
  , competenceGridExchange
  , competenceGridWithContentExchange
    -- * Lower-level pieces (re-exported for the matcher)
  , taskToExchange
  , solutionToExchange
  , attachmentToExchange
  , competenceRef
  , resourceToExchange
  , assignmentToExchange
  , lessonToExchange
  , competenceGridToExchange
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridId
  , Document (..)
  , Lesson (..)
  , Resource (..)
  , Solution (..)
  , Task (..)
  )
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId, LevelInfo (..))
import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Document.Lesson
  ( LessonItem (..)
  , LessonItemContent (..)
  , LessonPhase (..)
  )
import Competences.Document.Resource (ResourceContent (..), ResourceId, ResourceIdentifier (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Exchange.Types
  ( ExchangeAssignment (..)
  , ExchangeAttachment (..)
  , ExchangeCompetence (..)
  , ExchangeCompetenceGrid (..)
  , ExchangeCompetenceRef (..)
  , ExchangeDoc (..)
  , ExchangeLesson (..)
  , ExchangeLessonItem (..)
  , ExchangeLessonItemKind (..)
  , ExchangeLessonPhase (..)
  , ExchangeResource (..)
  , ExchangeResourceContent (..)
  , ExchangeSolution (..)
  , ExchangeTask (..)
  , emptyExchangeDoc
  )
import Competences.TaskContent.RichContent (toRawText)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Optics.Core ((&), (.~))

-- ============================================================================
-- Single-entity entry points
-- ============================================================================

-- | Export a single assignment. The 'Bool' selects draft vs published
-- pool — the resulting document populates the matching list and one
-- task list.
assignmentExchange :: Document -> Bool -> Assignment -> ExchangeDoc
assignmentExchange doc isDraft a =
  let xa = assignmentToExchange doc a
      taskList = mapMaybe (lookupTask doc) a.tasks
      xts = map (taskToExchange doc) taskList
   in if isDraft
        then emptyExchangeDoc & #draftAssignments .~ [xa] & #draftTasks .~ xts
        else emptyExchangeDoc & #assignments .~ [xa] & #tasks .~ xts

-- | Export a single task. The 'Bool' selects draft vs published pool.
taskExchange :: Document -> Bool -> Task -> ExchangeDoc
taskExchange doc isDraft t =
  let xt = taskToExchange doc t
   in if isDraft
        then emptyExchangeDoc & #draftTasks .~ [xt]
        else emptyExchangeDoc & #tasks .~ [xt]

-- | Export a single resource.
resourceExchange :: Document -> Resource -> ExchangeDoc
resourceExchange doc r =
  emptyExchangeDoc & #resources .~ [resourceToExchange doc r]

-- | Export a competence grid on its own — schema only, no referenced
-- tasks or resources.
competenceGridExchange :: Document -> CompetenceGrid -> ExchangeDoc
competenceGridExchange doc grid =
  emptyExchangeDoc & #competenceGrids .~ [competenceGridToExchange doc grid]

-- | Export a competence grid plus every task and resource that
-- references one of its competences. Useful for sharing a complete
-- teaching unit (schema + exemplary content).
competenceGridWithContentExchange :: Document -> CompetenceGrid -> ExchangeDoc
competenceGridWithContentExchange doc grid =
  let gridTasks =
        filter (taskReferencesGrid doc grid.id) (Ix.toList doc.tasks)
      gridResources =
        filter (resourceReferencesGrid doc grid.id) (Ix.toList doc.resources)
   in emptyExchangeDoc
        & #competenceGrids .~ [competenceGridToExchange doc grid]
        & #tasks .~ map (taskToExchange doc) gridTasks
        & #resources .~ map (resourceToExchange doc) gridResources

taskReferencesGrid :: Document -> CompetenceGridId -> Task -> Bool
taskReferencesGrid doc gridId t =
  any (referencesGrid doc gridId) (t.primary <> t.secondary)

resourceReferencesGrid :: Document -> CompetenceGridId -> Resource -> Bool
resourceReferencesGrid doc gridId r =
  any (referencesGrid doc gridId) r.competenceLevels

referencesGrid :: Document -> CompetenceGridId -> CompetenceLevelId -> Bool
referencesGrid doc gridId (cid, _) =
  case Ix.getOne (doc.competences Ix.@= cid) of
    Just c -> c.competenceGridId == gridId
    Nothing -> False

-- | Export a lesson, inlining every assignment, resource, and any
-- task referenced by phase or supplemental items but not already
-- carried by an embedded assignment.
lessonExchange :: Document -> Lesson -> ExchangeDoc
lessonExchange doc l =
  let inlinedAssignments = mapMaybe (lookupAssignment doc) l.assignments
      assignmentTaskIds = concatMap (.tasks) inlinedAssignments
      standaloneTaskIds =
        filter (`notElem` assignmentTaskIds) (referencedTasks l)
      assignmentTasks = mapMaybe (lookupTask doc) assignmentTaskIds
      standaloneTasks = mapMaybe (lookupTask doc) standaloneTaskIds
      inlinedResources =
        mapMaybe (lookupResource doc) (l.resources <> referencedResources l)
   in emptyExchangeDoc
        & #lessons .~ [lessonToExchange doc l]
        & #assignments .~ map (assignmentToExchange doc) inlinedAssignments
        & #tasks .~ map (taskToExchange doc) (assignmentTasks <> standaloneTasks)
        & #resources .~ map (resourceToExchange doc) inlinedResources

-- ============================================================================
-- Lower-level builders
-- ============================================================================

assignmentToExchange :: Document -> Assignment -> ExchangeAssignment
assignmentToExchange doc a =
  let AssignmentName name = a.name
      refs = mapMaybe (fmap taskIdentText . lookupTask doc) a.tasks
   in ExchangeAssignment
        { name = name
        , replaces = Nothing
        , description = toRawText a.description
        , assignmentDate = a.assignmentDate
        , activityType = a.activityType
        , groupSubmissionAllowed = a.groupSubmissionAllowed
        , taskRefs = refs
        }

lookupTask :: Document -> TaskId -> Maybe Task
lookupTask doc tid =
  Ix.getOne (doc.tasks Ix.@= tid)

lookupAssignment :: Document -> AssignmentId -> Maybe Assignment
lookupAssignment doc aid = Ix.getOne (doc.assignments Ix.@= aid)

lookupResource :: Document -> ResourceId -> Maybe Resource
lookupResource doc rid = Ix.getOne (doc.resources Ix.@= rid)

taskToExchange :: Document -> Task -> ExchangeTask
taskToExchange doc t =
  let TaskIdentifier ident = t.identifier
      solutions = Ix.toList (doc.solutions Ix.@= t.id)
   in ExchangeTask
        { identifier = ident
        , replaces = Nothing
        , title = t.title
        , content = fmap toRawText t.content
        , purpose = t.purpose
        , primary = mapMaybe (competenceRef doc) t.primary
        , secondary = mapMaybe (competenceRef doc) t.secondary
        , solutions = map solutionToExchange solutions
        , attachments = map attachmentToExchange t.attachments
        }

solutionToExchange :: Solution -> ExchangeSolution
solutionToExchange s =
  ExchangeSolution
    { solutionType = s.solutionType
    , content = toRawText s.content
    }

attachmentToExchange :: FileRef -> ExchangeAttachment
attachmentToExchange fref =
  ExchangeAttachment
    { fileName = fref.fileName
    , mimeType = fref.mimeType
    , sha256 = fref.hash.unSHA256Hash
    , bytes = fref.fileSize
    }

competenceRef :: Document -> CompetenceLevelId -> Maybe ExchangeCompetenceRef
competenceRef doc (cid, level) = do
  comp <- Ix.getOne (doc.competences Ix.@= cid)
  grid <- Ix.getOne (doc.competenceGrids Ix.@= comp.competenceGridId)
  pure
    ExchangeCompetenceRef
      { grid = grid.title
      , description = comp.description
      , level = level
      }

resourceToExchange :: Document -> Resource -> ExchangeResource
resourceToExchange doc r =
  let ResourceIdentifier ident = r.identifier
   in ExchangeResource
        { identifier = ident
        , replaces = Nothing
        , content = resourceContentToExchange r.content
        , competenceLevels = mapMaybe (competenceRef doc) r.competenceLevels
        , attachments = map attachmentToExchange r.attachments
        }

resourceContentToExchange :: ResourceContent -> ExchangeResourceContent
resourceContentToExchange = \case
  InlineContent rc -> ExInlineContent (toRawText rc)
  WebLink url desc -> ExWebLink url desc
  VideoLink url desc -> ExVideoLink url desc
  FileContent fref -> ExFileContent (attachmentToExchange fref)

lessonToExchange :: Document -> Lesson -> ExchangeLesson
lessonToExchange doc l =
  ExchangeLesson
    { title = l.title
    , replaces = Nothing
    , description = toRawText l.description
    , date = l.date
    , competences = mapMaybe (competenceRef doc) l.competenceLevels
    , phases = map (lessonPhaseToExchange doc) l.phases
    , notes = toRawText l.notes
    , supplementalItems = map (lessonItemToExchange doc) l.supplementalItems
    , notesTitleOverride = l.notesTitleOverride
    , assignmentRefs = mapMaybe (fmap assignmentNameText . lookupAssignment doc) l.assignments
    , resourceRefs = mapMaybe (fmap resourceIdentText . lookupResource doc) l.resources
    }

referencedTasks :: Lesson -> [TaskId]
referencedTasks l =
  [tid | LessonItem (PhaseTask tid) _ <- allItems l]

referencedResources :: Lesson -> [ResourceId]
referencedResources l =
  [rid | LessonItem (PhaseResource rid) _ <- allItems l]

allItems :: Lesson -> [LessonItem]
allItems l = concatMap (.items) l.phases <> l.supplementalItems

lessonPhaseToExchange :: Document -> LessonPhase -> ExchangeLessonPhase
lessonPhaseToExchange doc p =
  ExchangeLessonPhase
    { title = p.title
    , socialForm = p.socialForm
    , duration = p.duration
    , actionForm = p.actionForm
    , notes = toRawText p.notes
    , items = map (lessonItemToExchange doc) p.items
    }

lessonItemToExchange :: Document -> LessonItem -> ExchangeLessonItem
lessonItemToExchange doc item =
  let (kind', ref') = case item.content of
        PhaseAssignment aid ->
          ( ItemAssignment
          , maybe "" assignmentNameText (lookupAssignment doc aid)
          )
        PhaseTask tid ->
          ( ItemTask
          , maybe "" taskIdentText (lookupTask doc tid)
          )
        PhaseResource rid ->
          ( ItemResource
          , maybe "" resourceIdentText (lookupResource doc rid)
          )
   in ExchangeLessonItem
        { kind = kind'
        , ref = ref'
        , publish = item.publish
        }

assignmentNameText :: Assignment -> Text
assignmentNameText a = let AssignmentName n = a.name in n

taskIdentText :: Task -> Text
taskIdentText t = let TaskIdentifier n = t.identifier in n

resourceIdentText :: Resource -> Text
resourceIdentText r = let ResourceIdentifier n = r.identifier in n

competenceGridToExchange :: Document -> CompetenceGrid -> ExchangeCompetenceGrid
competenceGridToExchange doc grid =
  ExchangeCompetenceGrid
    { title = grid.title
    , replaces = Nothing
    , description = grid.description
    , competences = map competenceToExchange (Ix.toList (doc.competences Ix.@= grid.id))
    }

competenceToExchange :: Competence -> ExchangeCompetence
competenceToExchange c =
  ExchangeCompetence
    { description = c.description
    , replaces = Nothing
    , levels = fmap (\li -> li.description) c.levels
    }
