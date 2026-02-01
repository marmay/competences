-- |
-- Module      : Competences.Frontend.View.Evaluation
-- Description : Shared view helpers for evaluation components
--
-- Contains reusable view functions and pure helpers shared between
-- the assignment evaluator (EvaluatorDetail) and the lesson evaluator
-- (StudentEvaluatorModal).
module Competences.Frontend.View.Evaluation
  ( -- * Pure helpers
    abilityColorClass
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
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- ============================================================================
-- PURE HELPERS
-- ============================================================================

-- | Map Ability to CSS color classes for badges
abilityColorClass :: Ability -> T.Text
abilityColorClass SelfReliant = "bg-green-100 text-green-800"
abilityColorClass SelfReliantWithSillyMistakes = "bg-lime-100 text-lime-800"
abilityColorClass WithSupport = "bg-yellow-100 text-yellow-800"
abilityColorClass NotYet = "bg-red-100 text-red-800"

-- | Compute aggregated results: worst (max) ability per competence across all tasks
computeAggregation :: Map.Map (TaskId, CompetenceLevelId) Ability -> Map.Map CompetenceLevelId Ability
computeAggregation =
  Map.foldrWithKey (\(_, compId) ability acc -> Map.insertWith max compId ability acc) Map.empty

-- ============================================================================
-- VIEW PRIMITIVES
-- ============================================================================

-- | Resolve CompetenceLevelId to display name from competences IxSet
viewCompetenceName :: Ix.IxSet CompetenceIxs Competence -> CompetenceLevelId -> M.View m a
viewCompetenceName competences compId =
  let (competenceId, level) = compId
      competenceM = Ix.getOne (competences Ix.@= competenceId)
      name = case competenceM of
        Nothing -> C.translate' C.LblCompetence <> " " <> ms (T.pack (show compId))
        Just comp -> ms $ maybe (comp.description <> " - " <> T.pack (show level)) (.description) (comp.levels Map.!? level)
   in MH.span_ [class_ "flex-1 text-sm"] [M.text name]

-- | Single ability toggle button.
-- @currentAbility@: Nothing = unset, Just x = currently selected.
-- @mkAction@: partially applied action constructor, receives the clicked Ability.
viewAbilityBtn :: Maybe Ability -> (Ability -> a) -> Ability -> M.View m a
viewAbilityBtn currentAbility mkAction ability =
  let isSelected = currentAbility == Just ability
      buttonClass =
        if isSelected
          then "bg-primary text-primary-foreground px-2 py-0.5 text-xs rounded"
          else "bg-secondary text-secondary-foreground px-2 py-0.5 text-xs rounded hover:bg-secondary/80"
   in MH.button_
        [class_ buttonClass, MH.onClick (mkAction ability)]
        [M.text $ C.translate' $ C.LblAbility ability]

-- | Competence name + row of ability buttons
viewCompetenceRow :: Ix.IxSet CompetenceIxs Competence -> CompetenceLevelId -> Maybe Ability -> (Ability -> a) -> M.View m a
viewCompetenceRow competences compId currentAbility mkAction =
  MH.div_
    [class_ "flex items-center gap-2"]
    [ viewCompetenceName competences compId
    , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn currentAbility mkAction) abilities)
    ]

-- ============================================================================
-- TASK SECTION VIEWS
-- ============================================================================

-- | Task header: identifier + include\/exclude toggle + optional extra content (e.g. status dots).
viewTaskHeader :: Ix.IxSet TaskIxs Task -> TaskId -> Bool -> a -> [M.View m a] -> M.View m a
viewTaskHeader tasks taskId isExcluded toggleAction extraContent =
  case Ix.getOne (tasks Ix.@= taskId) of
    Nothing -> MH.div_ [] [M.text $ C.translate' C.LblTaskNotFound <> ": " <> ms (show taskId)]
    Just task ->
      let TaskIdentifier identifier = task.identifier
          toggleClass =
            if isExcluded
              then "px-2 py-1 rounded text-sm cursor-pointer border border-muted-foreground text-muted-foreground hover:bg-muted/50"
              else "px-2 py-1 rounded text-sm cursor-pointer bg-primary text-primary-foreground hover:bg-primary/90"
       in MH.div_
            [class_ "mt-3 mb-1 flex items-center justify-between"]
            [ MH.div_
                [class_ "flex items-center gap-3"]
                (Typography.h4 (C.translate' C.LblTaskPrefix <> ms identifier) : extraContent)
            , MH.button_
                [class_ toggleClass, MH.onClick toggleAction]
                [M.text $ C.translate' $ if isExcluded then C.LblIncludeTask else C.LblExcludeTask]
            ]

-- | Collapsible task content (disclosure chevron + rich text).
viewTaskContent
  :: Ix.IxSet TaskIxs Task
  -> Ix.IxSet TaskGroupIxs TaskGroup
  -> Set.Set TaskId
  -> TaskId
  -> (TaskId -> a)
  -> M.View m a
viewTaskContent tasks taskGroups expandedSet taskId toggleAction =
  case Ix.getOne (tasks Ix.@= taskId) of
    Nothing -> M.text ""
    Just task ->
      let content = getTaskContent taskGroups task
          isExpanded = Set.member taskId expandedSet
       in case content of
            Nothing -> M.text ""
            Just c
              | c == mempty -> M.text ""
              | otherwise ->
                  MH.div_
                    [class_ "mb-2"]
                    [ MH.div_
                        [ class_ "flex items-center gap-2 cursor-pointer hover:bg-muted/50 px-2 py-1 rounded"
                        , MH.onClick (toggleAction taskId)
                        ]
                        [ Disclosure.disclosureChevron isExpanded
                        , MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.translate' C.LblTaskStatement]
                        ]
                    , if isExpanded
                        then MH.div_ [class_ "ml-6 mb-2 prose prose-sm prose-stone max-w-none"] [renderRichText c]
                        else M.text ""
                    ]

-- | Per-task competence evaluations (lists competence rows with ability buttons).
viewTaskCompetences
  :: Ix.IxSet TaskIxs Task
  -> Ix.IxSet TaskGroupIxs TaskGroup
  -> Ix.IxSet CompetenceIxs Competence
  -> Map.Map (TaskId, CompetenceLevelId) Ability
  -> TaskId
  -> (TaskId -> CompetenceLevelId -> Ability -> a)
  -> M.View m a
viewTaskCompetences tasks taskGroups competences taskObs taskId mkAction =
  case Ix.getOne (tasks Ix.@= taskId) of
    Nothing -> M.text ""
    Just task ->
      let attrs = getTaskAttributes taskGroups task
          compIds = attrs.primary <> attrs.secondary
       in if null compIds
            then MH.div_ [class_ "mt-2"] [Typography.muted (C.translate' C.LblNoCompetences)]
            else
              MH.div_
                [class_ "mt-2 space-y-1"]
                ( map
                    ( \compId ->
                        viewCompetenceRow
                          competences
                          compId
                          (Map.lookup (taskId, compId) taskObs)
                          (mkAction taskId compId)
                    )
                    compIds
                )

-- ============================================================================
-- AGGREGATION VIEWS
-- ============================================================================

-- | Aggregation section: title + stale warning + compute button + content.
viewAggregationSection :: Bool -> Bool -> a -> M.View m a -> M.View m a
viewAggregationSection isStale hasResults computeAction resultsContent =
  MH.div_
    [class_ "border-t pt-4"]
    [ MH.div_
        [class_ "flex items-center justify-between mb-3"]
        [ Typography.h4 (C.translate' C.LblAggregatedResults)
        , MH.div_
            [class_ "flex items-center gap-2"]
            [ if isStale
                then MH.span_ [class_ "text-xs text-yellow-700"] [M.text $ C.translate' C.LblAggregationStale]
                else M.text ""
            , MH.button_
                [ MH.onClick computeAction
                , class_ "bg-primary text-primary-foreground px-3 py-1 text-sm rounded hover:bg-primary/90"
                ]
                [M.text $ C.translate' C.LblComputeAggregation]
            ]
        ]
    , if hasResults
        then resultsContent
        else Typography.muted (C.translate' C.LblComputeAggregationHint)
    ]

-- | Group aggregated results by competence grid. Delegates to a row renderer for
-- per-component customization (e.g. assignment adds contributing tasks info).
viewAggregatedResults
  :: Ix.IxSet CompetenceIxs Competence
  -> Ix.IxSet CompetenceGridIxs CompetenceGrid
  -> Map.Map CompetenceLevelId Ability
  -> ((CompetenceLevelId, Ability) -> M.View m a)
  -> M.View m a
viewAggregatedResults competences competenceGrids aggResults rowRenderer =
  let compIds = Set.fromList [compId | (compId, _) <- Map.keys aggResults]
      competencesWithResults = Ix.toAscList (Proxy @Order) $ competences Ix.@+ Set.toList compIds
      gridIds = Set.fromList $ map (.competenceGridId) competencesWithResults
      sortedGrids = Ix.toAscList (Proxy @Order) $ competenceGrids Ix.@+ Set.toList gridIds
   in MH.div_ [class_ "space-y-3"] (map viewGrid sortedGrids)
  where
    viewGrid grid =
      let gridCompetences = Ix.toAscList (Proxy @Order) $ competences Ix.@= grid.id
          resultsForGrid =
            [ (compLevelId, ability)
            | comp <- gridCompetences
            , (compLevelId@(compId, _), ability) <- Map.toList aggResults
            , compId == comp.id
            ]
       in if null resultsForGrid
            then M.text ""
            else
              MH.div_
                [class_ "border border-border rounded bg-muted/50"]
                [ MH.div_ [class_ "px-3 py-2 border-b bg-muted font-medium text-sm"] [M.text $ ms grid.title]
                , MH.div_ [class_ "p-2 space-y-1"] (map rowRenderer resultsForGrid)
                ]

-- | Default row renderer: competence name + ability buttons.
viewAggregatedCompetenceRow
  :: Ix.IxSet CompetenceIxs Competence
  -> (CompetenceLevelId -> Ability -> a)
  -> (CompetenceLevelId, Ability)
  -> M.View m a
viewAggregatedCompetenceRow competences mkAction (compId, ability) =
  MH.div_
    [class_ "flex items-center gap-2"]
    [ viewCompetenceName competences compId
    , MH.div_ [class_ "flex gap-1 shrink-0"] (map (viewAbilityBtn (Just ability) (mkAction compId)) abilities)
    ]
