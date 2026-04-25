-- | Unified resource lookup with per-lesson grouping.
--
-- Given a list of competence levels, discovers matching resources and tasks
-- with complete solutions, groups them by the lessons whose phase items or
-- supplemental items reference them, and annotates each item as 'Relevant'
-- or 'ContextOnly'.
--
-- Used by both the Assignment TaskResources component and the Resource Modal.
module Competences.Frontend.Component.ResourceLookup
  ( -- * Types
    GroupedResources (..)
  , AnnotatedLessonGroup (..)
  , ResolvedItem (..)
  , ItemRelevance (..)
    -- * Lookup
  , findGroupedResources
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lesson (..)
  , Resource (..)
  , SolutionType (..)
  , Task (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson
  ( LessonItem (..)
  , LessonItemContent (..)
  , LessonPhase (..)
  )
import Competences.Document.Resource (ResourceId)
import Competences.Document.Task
  ( TaskId
  , isResourceTask
  )
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
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

-- | An item that can appear in a lesson group or ungrouped.
data ResolvedItem
  = ResolvedResource !Resource
  | ResolvedTask !TaskWithSolutions
  deriving (Eq, Show)

-- | A lesson group with all its items annotated by relevance.
-- When a lesson contains at least one 'Relevant' item, ALL items in
-- that lesson are included, with non-matching ones marked
-- 'ContextOnly'.
data AnnotatedLessonGroup = AnnotatedLessonGroup
  { lesson :: !Lesson
  , items :: ![(ResolvedItem, ItemRelevance)]
  }
  deriving (Eq, Generic, Show)

-- | The complete result of a resource lookup.
data GroupedResources = GroupedResources
  { lessonGroups :: ![AnnotatedLessonGroup]
  , ungroupedResources :: ![Resource]
  , ungroupedTasks :: ![TaskWithSolutions]
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Lookup
-- ============================================================================

-- | Find matching resources and tasks with complete solutions for the given
-- competence levels, grouped by the lessons that reference them.
--
-- Algorithm:
-- 1. Find all resources matching the competence levels
-- 2. Find all tasks with competence overlap AND Complete solutions AND displayInResources
-- 3. For each lesson (sorted by date desc), if its phase items or
--    supplemental items contain at least one matching resource/task:
--    include ALL such items, annotating each as Relevant or ContextOnly.
-- 4. Resources/tasks not claimed by any lesson go into ungrouped lists.
findGroupedResources :: Document -> [CompetenceLevelId] -> GroupedResources
findGroupedResources doc compLevels =
  let -- Step 1: Find matching resources
      matchingResources = Ix.toList $ doc.resources Ix.@+ compLevels
      matchingResourceIds = Set.fromList (map (.id) matchingResources)

      -- Step 2: Find matching tasks (with Complete solutions, displayInResources)
      matchingTasks = findMatchingTasks compLevels doc
      matchingTaskIds = Set.fromList (map (\tws -> tws.task.id) matchingTasks)
      matchingTaskMap = Map.fromList [(tws.task.id, tws) | tws <- matchingTasks]

      -- Step 3: Group by lessons (most recent first; lessons without a
      -- date sort to the end via 'Maybe' ordering on Down).
      allLessons = sortOn (Down . (.date)) $ Ix.toList doc.lessons
      (groups, claimedResourceIds, claimedTaskIds) =
        buildGroups doc matchingResourceIds matchingTaskIds matchingTaskMap allLessons

      -- Step 4: Ungrouped items (relevant but not in any lesson)
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

-- | Build annotated lesson groups.
-- Returns (groups, claimed resource IDs, claimed task IDs).
--
-- A lesson is included if its phase items or supplemental items
-- contain at least one relevant (matching) resource or task. When
-- included, ALL such items are resolved — matching ones as Relevant,
-- others as ContextOnly. Assignment items in phases/supplemental are
-- ignored for this lookup; this view is about resources and tasks.
buildGroups
  :: Document
  -> Set ResourceId
  -> Set TaskId
  -> Map.Map TaskId TaskWithSolutions
  -> [Lesson]
  -> ([AnnotatedLessonGroup], Set ResourceId, Set TaskId)
buildGroups doc matchingResourceIds matchingTaskIds matchingTaskMap =
  foldr addGroup ([], Set.empty, Set.empty)
  where
    addGroup l (groups, claimedRes, claimedTasks) =
      let contents = lessonItemContents l
          hasRelevantItem = any (isRelevantItem matchingResourceIds matchingTaskIds) contents
       in if not hasRelevantItem
            then (groups, claimedRes, claimedTasks)
            else
              let annotatedItems = resolveAndAnnotate doc matchingResourceIds matchingTaskMap contents
                  newClaimedRes = foldr claimResource claimedRes contents
                  newClaimedTasks = foldr claimTask claimedTasks contents
                  group = AnnotatedLessonGroup l annotatedItems
               in (group : groups, newClaimedRes, newClaimedTasks)

    claimResource (PhaseResource rid) acc
      | Set.member rid matchingResourceIds = Set.insert rid acc
    claimResource _ acc = acc

    claimTask (PhaseTask tid) acc
      | Set.member tid matchingTaskIds = Set.insert tid acc
    claimTask _ acc = acc

-- | Flatten a lesson into its list of item-content references, drawing
-- from every phase and the supplemental list.
lessonItemContents :: Lesson -> [LessonItemContent]
lessonItemContents l =
  map (.content) (concatMap (.items) l.phases <> l.supplementalItems)

-- | Check whether a lesson item content is relevant (matches the requested
-- competences). Assignment items are never relevant for this lookup.
isRelevantItem :: Set ResourceId -> Set TaskId -> LessonItemContent -> Bool
isRelevantItem resourceIds taskIds = \case
  PhaseResource rid -> Set.member rid resourceIds
  PhaseTask tid -> Set.member tid taskIds
  PhaseAssignment _ -> False

-- | Resolve all items in a lesson, annotating each with relevance.
-- Assignment items are skipped entirely.
resolveAndAnnotate
  :: Document
  -> Set ResourceId
  -> Map.Map TaskId TaskWithSolutions
  -> [LessonItemContent]
  -> [(ResolvedItem, ItemRelevance)]
resolveAndAnnotate doc matchingResourceIds matchingTaskMap =
  concatMap resolveOne
  where
    resolveOne (PhaseResource rid) =
      case Ix.getOne (doc.resources Ix.@= rid) of
        Nothing -> []
        Just r ->
          let relevance =
                if Set.member rid matchingResourceIds
                  then Relevant
                  else ContextOnly
           in [(ResolvedResource r, relevance)]
    resolveOne (PhaseTask tid) =
      case Map.lookup tid matchingTaskMap of
        Just tws ->
          [(ResolvedTask tws, Relevant)]
        Nothing ->
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
    resolveOne (PhaseAssignment _) = []
