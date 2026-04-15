-- | Unified resource lookup with lesson-note grouping.
--
-- Given a list of competence levels, discovers matching resources and tasks
-- with complete solutions, groups them by lesson notes, and annotates each
-- item as 'Relevant' or 'ContextOnly'.
--
-- Used by both the Assignment TaskResources component and the Resource Modal.
module Competences.Frontend.Component.ResourceLookup
  ( -- * Types
    GroupedResources (..)
  , AnnotatedLessonNoteGroup (..)
  , ResolvedItem (..)
  , ItemRelevance (..)
    -- * Lookup
  , findGroupedResources
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , LessonNoteItem (..)
  , LessonNotes (..)
  , Resource (..)
  , SolutionType (..)
  , Task (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Task
  ( TaskId
  , isResourceTask
  )
import Competences.Frontend.Component.Task.Projection (TaskWithSolutions (..))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

-- ============================================================================
-- Types
-- ============================================================================

-- | Whether an item matches the requested competence levels.
data ItemRelevance
  = Relevant
  | ContextOnly
  deriving (Eq, Show)

-- | An item that can appear in a lesson note group or ungrouped.
data ResolvedItem
  = ResolvedResource !Resource
  | ResolvedTask !TaskWithSolutions
  deriving (Eq, Show)

-- | A lesson note group with all its items annotated by relevance.
-- When a lesson note contains at least one 'Relevant' item, ALL items
-- in that lesson note are included, with non-matching ones marked 'ContextOnly'.
data AnnotatedLessonNoteGroup = AnnotatedLessonNoteGroup
  { lessonNotes :: !LessonNotes
  , items :: ![(ResolvedItem, ItemRelevance)]
  }
  deriving (Eq, Generic, Show)

-- | The complete result of a resource lookup.
data GroupedResources = GroupedResources
  { lessonNoteGroups :: ![AnnotatedLessonNoteGroup]
  , ungroupedResources :: ![Resource]
  , ungroupedTasks :: ![TaskWithSolutions]
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Lookup
-- ============================================================================

-- | Find matching resources and tasks with complete solutions for the given
-- competence levels, grouped by lesson notes.
--
-- Algorithm:
-- 1. Find all resources matching the competence levels
-- 2. Find all tasks with competence overlap AND Complete solutions AND displayInResources
-- 3. For each lesson note (sorted by date desc), if it contains at least one
--    matching item: include ALL items, annotating each as Relevant or ContextOnly
-- 4. Resources/tasks not claimed by any lesson note go into ungrouped lists
findGroupedResources :: Document -> [CompetenceLevelId] -> GroupedResources
findGroupedResources doc compLevels =
  let -- Step 1: Find matching resources
      matchingResources = Ix.toList $ doc.resources Ix.@+ compLevels
      matchingResourceIds = Set.fromList (map (.id) matchingResources)

      -- Step 2: Find matching tasks (with Complete solutions, displayInResources)
      matchingTasks = findMatchingTasks compLevels doc
      matchingTaskIds = Set.fromList (map (\tws -> tws.task.id) matchingTasks)
      matchingTaskMap = Map.fromList [(tws.task.id, tws) | tws <- matchingTasks]

      -- Step 3: Group by lesson notes
      allLessonNotes = sortOn (Down . (.date)) $ Ix.toList doc.lessonNotes
      (groups, claimedResourceIds, claimedTaskIds) =
        buildGroups doc matchingResourceIds matchingTaskIds matchingTaskMap allLessonNotes

      -- Step 4: Ungrouped items (relevant but not in any lesson note)
      ungroupedRes =
        [ r | r <- matchingResources, not (Set.member r.id claimedResourceIds) ]
      ungroupedTsks =
        [ tws | tws <- matchingTasks, not (Set.member tws.task.id claimedTaskIds) ]
   in GroupedResources groups ungroupedRes ungroupedTsks

-- | Find tasks with competence overlap, Complete solutions, and displayInResources.
findMatchingTasks :: [CompetenceLevelId] -> Document -> [TaskWithSolutions]
findMatchingTasks compLevels doc =
  let compLevelSet = Set.fromList compLevels
      allTasks = filter isResourceTask $ Ix.toList doc.tasks
   in [ TaskWithSolutions
          { task = t
          , taskContent = t.content
          , taskPurpose = t.purpose
          , solutions = completeSols
          }
      | t <- allTasks
      , let tCompLevels = Set.fromList (t.primary <> t.secondary)
      , not (Set.disjoint compLevelSet tCompLevels)
      , let completeSols = Ix.toList $ doc.solutions Ix.@= t.id Ix.@= Complete
      , not (null completeSols)
      ]

-- | Build annotated lesson note groups.
-- Returns (groups, claimed resource IDs, claimed task IDs).
--
-- A lesson note is included if it contains at least one relevant (matching) item.
-- When included, ALL items in the lesson note are resolved — matching ones as
-- Relevant, others as ContextOnly.
buildGroups
  :: Document
  -> Set ResourceId
  -> Set TaskId
  -> Map.Map TaskId TaskWithSolutions
  -> [LessonNotes]
  -> ([AnnotatedLessonNoteGroup], Set ResourceId, Set TaskId)
buildGroups doc matchingResourceIds matchingTaskIds matchingTaskMap =
  foldr addGroup ([], Set.empty, Set.empty)
  where
    addGroup ln (groups, claimedRes, claimedTasks) =
      let -- Check if this lesson note has at least one relevant item
          hasRelevantItem = any (isRelevantItem matchingResourceIds matchingTaskIds) ln.items
       in if not hasRelevantItem
            then (groups, claimedRes, claimedTasks)
            else
              let -- Resolve ALL items with relevance annotation
                  annotatedItems = resolveAndAnnotate doc matchingResourceIds matchingTaskMap ln.items
                  -- Track claimed IDs (only for relevant items)
                  newClaimedRes = foldr claimResource claimedRes ln.items
                  newClaimedTasks = foldr claimTask claimedTasks ln.items
                  group = AnnotatedLessonNoteGroup ln annotatedItems
               in (group : groups, newClaimedRes, newClaimedTasks)

    claimResource (LessonResource rid) acc
      | Set.member rid matchingResourceIds = Set.insert rid acc
    claimResource _ acc = acc

    claimTask (LessonTask tid) acc
      | Set.member tid matchingTaskIds = Set.insert tid acc
    claimTask _ acc = acc

-- | Check whether a lesson note item is relevant (matches the requested competences).
isRelevantItem :: Set ResourceId -> Set TaskId -> LessonNoteItem -> Bool
isRelevantItem resourceIds taskIds = \case
  LessonResource rid -> Set.member rid resourceIds
  LessonTask tid -> Set.member tid taskIds

-- | Resolve all items in a lesson note, annotating each with relevance.
resolveAndAnnotate
  :: Document
  -> Set ResourceId
  -> Map.Map TaskId TaskWithSolutions
  -> [LessonNoteItem]
  -> [(ResolvedItem, ItemRelevance)]
resolveAndAnnotate doc matchingResourceIds matchingTaskMap =
  concatMap resolveOne
  where
    resolveOne (LessonResource rid) =
      case Ix.getOne (doc.resources Ix.@= rid) of
        Nothing -> []
        Just r ->
          let relevance =
                if Set.member rid matchingResourceIds
                  then Relevant
                  else ContextOnly
           in [(ResolvedResource r, relevance)]
    resolveOne (LessonTask tid) =
      case Map.lookup tid matchingTaskMap of
        Just tws ->
          -- Task is in matching set → Relevant
          [(ResolvedTask tws, Relevant)]
        Nothing ->
          -- Task not in matching set → resolve from document as ContextOnly
          case Ix.getOne (doc.tasks Ix.@= tid) of
            Nothing -> []
            Just t ->
              let tws =
                    TaskWithSolutions
                      { task = t
                      , taskContent = t.content
                      , taskPurpose = t.purpose
                      , solutions = Ix.toList (doc.solutions Ix.@= tid)
                      }
               in [(ResolvedTask tws, ContextOnly)]
