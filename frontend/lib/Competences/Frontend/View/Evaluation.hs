-- |
-- Module      : Competences.Frontend.View.Evaluation
-- Description : Shared view helpers for evaluation components
--
-- Contains reusable view functions and pure helpers shared between
-- the assignment evaluator (EvaluatorDetail) and the lesson evaluator
-- (StudentEvaluatorModal).
module Competences.Frontend.View.Evaluation
  ( -- * Projection types
    TaskViewData (..)
  , CompetenceLevelInfo (..)
    -- * Projection builders
  , projectTasks
  , projectCompetenceLevels
    -- * Pure helpers (re-exported from Color.Ability)
  , abilityPalette
  , computeAggregation
    -- * View primitives
  , viewCompetenceName
  , viewAbilityBtn
  , viewCompetenceRow
    -- * Task section views
  , viewTaskHeader
  , viewTaskContent
  , viewTaskCompetences
    -- * Aggregation views
  , viewAggregationSection
  , viewAggregatedResults
  , viewAggregatedCompetenceRow
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridIxs
  , LevelInfo (..)
  , Order
  )
import Competences.Document.Competence (CompetenceIxs, CompetenceLevelId)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Evidence (Ability (..), abilities)
import Competences.Document.Task
  ( Task (..)
  , TaskAttributes (..)
  , TaskGroup
  , TaskGroupIxs
  , TaskId
  , TaskIdentifier (..)
  , TaskIxs
  , getTaskAttributes
  , getTaskContent
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Color.Ability (abilityPalette)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (RichContent)
import Data.List (groupBy, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- ============================================================================
-- PROJECTION TYPES
-- ============================================================================

-- | Pre-computed display data for a single task.
data TaskViewData = TaskViewData
  { identifier :: !T.Text
  , content :: !(Maybe RichContent)
  , competenceLevels :: ![CompetenceLevelId]
  }
  deriving (Eq, Generic, Show)

-- | Pre-computed display info for a single competence level.
data CompetenceLevelInfo = CompetenceLevelInfo
  { displayName :: !T.Text
  , gridId :: !CompetenceGridId
  , gridTitle :: !T.Text
  , gridOrder :: !Order
  , competenceOrder :: !Order
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- PROJECTION BUILDERS
-- ============================================================================

-- | Pre-compute display data for all tasks in the document.
projectTasks :: Ix.IxSet TaskGroupIxs TaskGroup -> Ix.IxSet TaskIxs Task -> Map.Map TaskId TaskViewData
projectTasks taskGroups tasks =
  Map.fromList
    [ (task.id, mkTaskViewData task)
    | task <- Ix.toList tasks
    ]
  where
    mkTaskViewData task =
      let TaskIdentifier ident = task.identifier
          attrs = getTaskAttributes taskGroups task
       in TaskViewData
            { identifier = ident
            , content = getTaskContent taskGroups task
            , competenceLevels = attrs.primary <> attrs.secondary
            }

-- | Pre-compute display info for all competence levels in the document.
projectCompetenceLevels
  :: Ix.IxSet CompetenceIxs Competence
  -> Ix.IxSet CompetenceGridIxs CompetenceGrid
  -> Map.Map CompetenceLevelId CompetenceLevelInfo
projectCompetenceLevels competences competenceGrids =
  Map.fromList
    [ ((comp.id, level), mkInfo comp grid level levelInfo)
    | comp <- Ix.toList competences
    , let mGrid = Ix.getOne (competenceGrids Ix.@= comp.competenceGridId)
    , (level, levelInfo) <- Map.toList comp.levels
    , Just grid <- [mGrid]
    ]
  where
    mkInfo comp grid _level levelInfo =
      CompetenceLevelInfo
        { displayName = levelInfo.description
        , gridId = grid.id
        , gridTitle = grid.title
        , gridOrder = grid.order
        , competenceOrder = comp.order
        }

-- ============================================================================
-- PURE HELPERS
-- ============================================================================

-- | Compute aggregated results: worst (max) ability per competence across all tasks
computeAggregation :: Map.Map (TaskId, CompetenceLevelId) Ability -> Map.Map CompetenceLevelId Ability
computeAggregation =
  Map.foldrWithKey (\(_, compId) ability acc -> Map.insertWith max compId ability acc) Map.empty

-- ============================================================================
-- VIEW PRIMITIVES
-- ============================================================================

-- | Resolve CompetenceLevelId to display name from pre-computed map.
viewCompetenceName :: Map.Map CompetenceLevelId CompetenceLevelInfo -> CompetenceLevelId -> M.View m a
viewCompetenceName compInfos compId =
  let name = case Map.lookup compId compInfos of
        Nothing -> C.translate' C.LblCompetence <> " " <> ms (T.pack (show compId))
        Just info -> ms info.displayName
   in MH.span_ [class_ "flex-1 text-sm"] [M.text name]

-- | Single ability toggle button.
-- @currentAbility@: Nothing = unset, Just x = currently selected.
-- @mkAction@: partially applied action constructor, receives the clicked Ability.
viewAbilityBtn :: Maybe Ability -> (Ability -> a) -> Ability -> M.View m a
viewAbilityBtn currentAbility mkAction ability =
  let isSelected = currentAbility == Just ability
   in Button.toggleSm isSelected (Button.button (C.LblAbility ability) (Just (mkAction ability)))

-- | Competence name + row of ability buttons
viewCompetenceRow :: Map.Map CompetenceLevelId CompetenceLevelInfo -> CompetenceLevelId -> Maybe Ability -> (Ability -> a) -> M.View m a
viewCompetenceRow compInfos compId currentAbility mkAction =
  Layout.hFlow
    (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
    [ viewCompetenceName compInfos compId
    , MH.div_
        [class_ "shrink-0"]
        [ Layout.hFlow
            Layout.gapT
            (map (viewAbilityBtn currentAbility mkAction) abilities)
        ]
    ]

-- ============================================================================
-- TASK SECTION VIEWS
-- ============================================================================

-- | Task header: identifier + include\/exclude toggle + optional extra content (e.g. status dots).
viewTaskHeader :: Map.Map TaskId TaskViewData -> TaskId -> Bool -> a -> [M.View m a] -> M.View m a
viewTaskHeader taskData taskId isExcluded toggleAction extraContent =
  case Map.lookup taskId taskData of
    Nothing -> MH.div_ [] [M.text $ C.translate' C.LblTaskNotFound <> ": " <> ms (show taskId)]
    Just tvd ->
      MH.div_
        [class_ "mt-3 mb-1"]
        [ Layout.hFlow
            (Layout.hFull <> Layout.crossCenter)
            [ Layout.hFlow
                (Layout.gapM <> Layout.hFull <> Layout.crossCenter)
                (Typography.h4 (C.translate' C.LblTaskPrefix <> ms tvd.identifier) : extraContent)
            , Layout.flowSpring
            , Button.toggleSm (not isExcluded)
                (Button.button (if isExcluded then C.LblIncludeTask else C.LblExcludeTask) (Just toggleAction))
            ]
        ]

-- | Collapsible task content (disclosure chevron + rich text).
viewTaskContent
  :: FormulaCache
  -> Map.Map TaskId TaskViewData
  -> Set.Set TaskId
  -> TaskId
  -> (TaskId -> a)
  -> M.View m a
viewTaskContent fc taskData expandedSet taskId toggleAction =
  case Map.lookup taskId taskData of
    Nothing -> M.text ""
    Just tvd ->
      let isExpanded = Set.member taskId expandedSet
       in case tvd.content of
            Nothing -> M.text ""
            Just c
              | c == mempty -> M.text ""
              | otherwise ->
                  let titleView = Disclosure.titleText (C.translate' C.LblTaskStatement)
                      bodyView = MH.div_ [class_ "prose prose-sm prose-stone max-w-none"] [renderRichText fc c]
                   in Disclosure.innerDisclosure (toggleAction taskId) $
                        Disclosure.contents titleView isExpanded bodyView []

-- | Per-task competence evaluations (lists competence rows with ability buttons).
viewTaskCompetences
  :: Map.Map TaskId TaskViewData
  -> Map.Map CompetenceLevelId CompetenceLevelInfo
  -> Map.Map (TaskId, CompetenceLevelId) Ability
  -> TaskId
  -> (TaskId -> CompetenceLevelId -> Ability -> a)
  -> M.View m a
viewTaskCompetences taskData compInfos taskObs taskId mkAction =
  case Map.lookup taskId taskData of
    Nothing -> M.text ""
    Just tvd ->
      let compIds = tvd.competenceLevels
       in if null compIds
            then MH.div_ [class_ "mt-2"] [Typography.muted (C.translate' C.LblNoCompetences)]
            else
              MH.div_
                [class_ "mt-2"]
                [ Layout.vFlow
                    Layout.gapT
                    ( map
                        ( \compId ->
                            viewCompetenceRow
                              compInfos
                              compId
                              (Map.lookup (taskId, compId) taskObs)
                              (mkAction taskId compId)
                        )
                        compIds
                    )
                ]

-- ============================================================================
-- AGGREGATION VIEWS
-- ============================================================================

-- | Aggregation section: title + stale warning + compute button + content.
viewAggregationSection :: Bool -> Bool -> a -> M.View m a -> M.View m a
viewAggregationSection isStale hasResults computeAction resultsContent =
  MH.div_
    [class_ "border-t pt-4"]
    [ MH.div_
        [class_ "mb-3"]
        [ Layout.hFlow
            (Layout.hFull <> Layout.crossCenter)
            [ Typography.h4 (C.translate' C.LblAggregatedResults)
            , Layout.flowSpring
            , Layout.hFlow
                (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                [ if isStale
                    then Badge.outline (Badge.badgeText (C.translate' C.LblAggregationStale))
                    else M.text ""
                , Button.primarySm (Button.button C.LblComputeAggregation (Just computeAction))
                ]
            ]
        ]
    , if hasResults
        then resultsContent
        else Typography.muted (C.translate' C.LblComputeAggregationHint)
    ]

-- | A single aggregated result enriched with grid/competence ordering info.
data AggregatedEntry = AggregatedEntry
  { gridOrder :: !Order
  , competenceOrder :: !Order
  , gridTitle :: !T.Text
  , competenceLevelId :: !CompetenceLevelId
  , ability :: !Ability
  }
  deriving (Eq, Ord)

-- | Group aggregated results by competence grid using pre-computed info.
viewAggregatedResults
  :: Map.Map CompetenceLevelId CompetenceLevelInfo
  -> Map.Map CompetenceLevelId Ability
  -> ((CompetenceLevelId, Ability) -> M.View m a)
  -> M.View m a
viewAggregatedResults compInfos aggResults rowRenderer =
  let entries = sort $ mapMaybe enrichEntry (Map.toList aggResults)
      grouped = groupBy (\a b -> a.gridOrder == b.gridOrder) entries
   in Layout.vFlow Layout.gapS (map viewGrid grouped)
  where
    enrichEntry (compId, ab) = do
      info <- Map.lookup compId compInfos
      pure
        AggregatedEntry
          { gridOrder = info.gridOrder
          , competenceOrder = info.competenceOrder
          , gridTitle = info.gridTitle
          , competenceLevelId = compId
          , ability = ab
          }
    viewGrid [] = M.text ""
    viewGrid es@(first : _) =
      Card.cardWithHeader (ms first.gridTitle) Nothing
        (map (\e -> rowRenderer (e.competenceLevelId, e.ability)) es)

-- | Default row renderer: competence name + ability buttons.
viewAggregatedCompetenceRow
  :: Map.Map CompetenceLevelId CompetenceLevelInfo
  -> (CompetenceLevelId -> Ability -> a)
  -> (CompetenceLevelId, Ability)
  -> M.View m a
viewAggregatedCompetenceRow compInfos mkAction (compId, ability) =
  viewCompetenceRow compInfos compId (Just ability) (mkAction compId)
