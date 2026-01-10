module Competences.Frontend.Component.EvidenceEditor.BulkEvidenceEditor
  ( bulkEvidenceEditorComponent
  , BulkEditorModel (..)
  , BulkEditorAction (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , User (..)
  , emptyDocument
  , levelDescription
  )
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  , activityTypes
  )
import Competences.Document.Order (formatOrderNumber)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId, isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.MultiStageSelector
  ( HList (..)
  , IncrementalParserSpec (..)
  , MultiStageSelectorConfig (..)
  , MultiStageSelectorStyle (..)
  , Pipeline
  , ResultView (..)
  , StageKind (..)
  , done
  , initialize
  , matchingInput
  , multiStageSelectorComponent
  , stage'
  )
import Competences.Frontend.Component.Selector.ObservationSelector
  ( competenceGridP
  , competenceP
  , levelP
  )
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , selectorLens
  )
import Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , searchableMultiUserSelectorComponent
  )
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncContext
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~), (^.))

-- | Model for the bulk evidence editor
data BulkEditorModel = BulkEditorModel
  { document :: !Document
  , date :: !Day
  , activityType :: !ActivityType
  , selectedTask :: !(Maybe TaskId)
  , selectedStudents :: ![User]
  , pendingObservations :: ![Observation]
  , studentObservations :: !(Map.Map UserId [Observation])
  }
  deriving (Eq, Generic, Show)

-- | Actions for the bulk evidence editor
data BulkEditorAction
  = UpdateDocument !DocumentChange
  | SetDate !Day
  | SetActivityType !ActivityType
  | AddObservationsToSelectedStudents
  | RemoveObservation !UserId !Observation
  | SaveAll
  | Cancel
  deriving (Eq, Show)

-- | Create the bulk evidence editor component
bulkEvidenceEditorComponent
  :: SyncContext
  -> M.Component p BulkEditorModel BulkEditorAction
bulkEvidenceEditorComponent r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      BulkEditorModel
        { document = emptyDocument
        , date = syncDocumentEnv r ^. #currentDay
        , activityType = Conversation  -- Default to Conversation (Gespräch)
        , selectedTask = Nothing
        , selectedStudents = []
        , pendingObservations = []
        , studentObservations = Map.empty
        }

    update (UpdateDocument (DocumentChange d _)) =
      M.modify $ #document .~ d

    update (SetDate d) =
      M.modify $ #date .~ d

    update (SetActivityType t) =
      M.modify $ #activityType .~ t

    update AddObservationsToSelectedStudents = do
      m <- M.get
      let studentIds = map (.id) m.selectedStudents
          -- Add pending observations to all selected students
          updatedObs = foldl
            (\acc uid ->
              let existing = Map.findWithDefault [] uid acc
                  -- Create new observations with unique IDs for each student
               in Map.insert uid (existing ++ m.pendingObservations) acc
            )
            m.studentObservations
            studentIds
      M.modify $ \s ->
        s & #studentObservations .~ updatedObs
          & #pendingObservations .~ []

    update (RemoveObservation uid obs) =
      M.modify $ \m ->
        let existingObs = Map.findWithDefault [] uid m.studentObservations
            filteredObs = filter (\o -> o.id /= obs.id) existingObs
         in m & #studentObservations .~ Map.insert uid filteredObs m.studentObservations

    update SaveAll = do
      m <- M.get
      let studentsWithObs = Map.toList $ Map.filter (not . null) m.studentObservations
      -- Generate fresh IDs for all observations when saving
      M.io_ $ mapM_ (saveStudentEvidence r m) studentsWithObs
      -- Clear observations after saving
      M.modify $ #studentObservations .~ Map.empty

    update Cancel =
      M.modify $ \s ->
        s & #studentObservations .~ Map.empty
          & #pendingObservations .~ []
          & #selectedStudents .~ []

    view m =
      MH.div_
        [class_ "flex flex-col h-full gap-4 p-4"]
        [ header m
        , settingsRow m
        , observationEntry r m
        , studentList m
        ]

    -- Header with title and save/cancel buttons
    header m =
      MH.div_
        [class_ "flex items-center justify-between"]
        [ Typography.h2 (C.translate' C.LblBulkEvidenceEntry)
        , MH.div_
            [class_ "flex gap-2"]
            [ Button.buttonOutline (C.translate' C.LblCancel)
                & Button.withClick Cancel
                & Button.renderButton
            , Button.buttonPrimary (C.translate' C.LblSaveAll)
                & Button.withClick SaveAll
                & Button.withDisabled (Map.null $ Map.filter (not . null) m.studentObservations)
                & Button.renderButton
            ]
        ]

    -- Settings row with date and activity type
    settingsRow m =
      MH.div_
        [class_ "flex gap-4 items-end"]
        [ MH.div_
            [class_ "w-40"]
            [ Input.fieldWrapper (C.translate' C.LblEvidenceDate) $
                Input.dateInput (M.ms $ show m.date) (\s -> SetDate (read $ M.fromMisoString s))
            ]
        , MH.div_
            [class_ "w-48"]
            [ MH.label_ [class_ "text-sm font-medium text-muted-foreground mb-1 block"] [M.text $ C.translate' C.LblActivityType]
            , MH.select_
                [ class_ "flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                , MH.onChange (\s -> SetActivityType (read (M.fromMisoString s) :: ActivityType))
                ]
                [ MH.option_
                    [ MP.value_ (M.ms $ show t)
                    , MP.selected_ (t == m.activityType)
                    ]
                    [M.text $ C.translate' $ C.LblActivityTypeDescription t]
                | t <- activityTypes
                ]
            ]
        ]

    -- Observation entry: students selector + observation selector + add button
    observationEntry r' m' =
      Card.card
        [ Typography.h4 (C.translate' C.LblAddObservation)
        , MH.div_
            [class_ "flex flex-col gap-4 mt-2"]
            [ -- Student selector
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text $ C.translate' C.LblStudents]
                , V.componentA
                    "bulk-student-selector"
                    [class_ "w-full"]
                    (searchableMultiUserSelectorComponent
                      r'
                      (studentSelectorConfig m')
                      studentSelectorLens
                    )
                ]
            , -- Observation selector
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text $ C.translate' C.LblActivityObservations]
                , V.componentA
                    "bulk-observation-selector"
                    [class_ "w-full"]
                    (multiStageSelectorComponent
                      r'
                      (bulkObservationConfig r')
                      observationSelectorLens
                    )
                ]
            , -- Add button
              MH.div_
                [class_ "flex justify-end"]
                [ Button.buttonPrimary (C.translate' C.LblAddToSelectedStudents)
                    & Button.withClick AddObservationsToSelectedStudents
                    & Button.withDisabled (null m'.selectedStudents || null m'.pendingObservations)
                    & Button.renderButton
                ]
            ]
        ]

    -- Student selector configuration
    studentSelectorConfig m' =
      UserSelectorConfig
        { isPossibleUser = isStudent
        , isInitialUser = \u -> u `elem` m'.selectedStudents
        }

    -- Student selector lens (syncs selectedStudents with component)
    studentSelectorLens :: SelectorTransformedLens BulkEditorModel [] User [] User
    studentSelectorLens = selectorLens #selectedStudents

    -- Observation selector lens (syncs pendingObservations with component)
    observationSelectorLens :: SelectorTransformedLens BulkEditorModel [] Observation [] Observation
    observationSelectorLens = selectorLens #pendingObservations

    -- List of students with observations
    studentList m' =
      let studentsWithObs = Map.toList $ Map.filter (not . null) m'.studentObservations
       in if null studentsWithObs
            then V.empty
            else
              MH.div_
                [class_ "flex-1 overflow-y-auto"]
                [ Typography.h4 (C.translate' C.LblStudentOverview)
                , MH.div_
                    [class_ "flex flex-col gap-3 mt-2"]
                    (map (studentCard m') studentsWithObs)
                ]

    -- Card for a single student showing their observations
    studentCard m' (uid, observations) =
      let userName = case Ix.getOne (m'.document.users Ix.@= uid) of
            Just u -> M.ms u.name
            Nothing -> M.ms $ show uid
       in Card.card
            [ MH.div_
                [class_ "flex items-center justify-between mb-2"]
                [Typography.h4 userName]
            , MH.div_
                [class_ "flex flex-wrap gap-1"]
                (map (observationBadge m' uid) observations)
            ]

    -- Badge showing an observation with delete button
    observationBadge m' uid obs =
      let label = formatObservation m' obs
       in MH.span_
            [class_ "inline-flex items-center gap-1 px-2 py-1 bg-muted rounded text-sm"]
            [ M.text label
            , MH.button_
                [ class_ "text-muted-foreground hover:text-destructive"
                , MH.onClick (RemoveObservation uid obs)
                ]
                [M.text "×"]
            ]

    -- Format observation for display
    formatObservation m' obs =
      let (cid, lvl) = obs.competenceLevelId
       in case Ix.getOne (m'.document.competences Ix.@= cid) of
            Just c ->
              case Ix.getOne (m'.document.competenceGrids Ix.@= c.competenceGridId) of
                Just cg ->
                  let cgLabel = formatOrderNumber cg.order
                      cLabel = formatOrderNumber c.order
                      lvlLabel = levelLabel lvl
                      sfLabel = socialFormLabel obs.socialForm
                      aLabel = abilityLabel obs.ability
                   in M.ms $ intercalate "." [cgLabel, cLabel, lvlLabel, sfLabel, aLabel]
                Nothing -> M.ms $ show obs.competenceLevelId
            Nothing -> M.ms $ show obs.competenceLevelId

    levelLabel BasicLevel = "1"
    levelLabel IntermediateLevel = "2"
    levelLabel AdvancedLevel = "3"

    socialFormLabel Group = "1"
    socialFormLabel Individual = "2"

    abilityLabel SelfReliant = "1"
    abilityLabel SelfReliantWithSillyMistakes = "2"
    abilityLabel WithSupport = "3"
    abilityLabel NotYet = "4"

-- | Configuration for bulk observation selector
-- Same as regular observation selector but with empty initial results
bulkObservationConfig :: SyncContext -> MultiStageSelectorConfig Observation
bulkObservationConfig r =
  MultiStageSelectorConfig
    { initialState = initialize (bulkObservationPipeline r)
    , errorMessage = C.translate' C.LblPleaseCompleteObservation
    , initResults = const []  -- No initial observations in bulk mode
    , validateResults = \_ results -> results
    , viewResult = viewBulkObservationResult
    , style = MultiStageSelectorEnabled
    }

-- | Pipeline for bulk observation creation (same as regular)
bulkObservationPipeline :: SyncContext -> Pipeline 'IsStage '[] CompetenceGrid Observation
bulkObservationPipeline r =
  stage' competenceGridP $ \cg ->
    stage' (competenceP cg) $ \c ->
      stage' (levelP c) $ \_ ->
        stage' socialFormP $ \_ ->
          stage' abilityP $ \_ ->
            done $ \(HCons a' (HCons sf' (HCons l' (HCons c' (HCons _cg HNil))))) -> do
              obsId <- nextId r
              pure $
                Observation
                  { id = obsId
                  , competenceLevelId = (c'.id, l')
                  , socialForm = sf'
                  , ability = a'
                  }

-- | View result for bulk observation
viewBulkObservationResult :: Document -> Observation -> ResultView
viewBulkObservationResult doc observation =
  case lookupObservationData doc observation of
    Nothing ->
      ResultView
        { badgeText = "???"
        , tooltipContent = Just "Die Beobachtung bezieht sich auf Daten, die nicht länger existieren."
        }
    Just (competence, competenceGrid) ->
      let competenceGridLabel = competenceGridP.reconstructInput competenceGrid
          competenceLabel = (competenceP competenceGrid).reconstructInput competence
          lvlLabel = (levelP competence).reconstructInput (snd observation.competenceLevelId)
          sfLabel = socialFormP.reconstructInput observation.socialForm
          aLabel = abilityP.reconstructInput observation.ability
          label = M.ms $ intercalate "." [competenceGridLabel, competenceLabel, lvlLabel, sfLabel, aLabel]
          tooltipText =
            M.ms competenceLabel
              <> ": "
              <> M.ms competence.description
              <> "\n"
              <> M.ms lvlLabel
              <> ": "
              <> M.ms (levelDescription (snd observation.competenceLevelId) competence)
              <> "\n"
              <> M.ms sfLabel
              <> ": "
              <> C.translate' (C.LblSocialForm observation.socialForm)
              <> "\n"
              <> M.ms aLabel
              <> ": "
              <> C.translate' (C.LblAbility observation.ability)
       in ResultView
            { badgeText = label
            , tooltipContent = Just tooltipText
            }

-- | Helper to lookup observation-related data
lookupObservationData :: Document -> Observation -> Maybe (Competence, CompetenceGrid)
lookupObservationData doc observation = do
  competence <- Ix.getOne $ doc.competences Ix.@= fst observation.competenceLevelId
  competenceGrid <- Ix.getOne $ doc.competenceGrids Ix.@= competence.competenceGridId
  pure (competence, competenceGrid)

-- | Social form parser
socialFormP :: IncrementalParserSpec SocialForm
socialFormP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions _ s =
      matchingInput s $
        map
          ( \sf ->
              let label = reconstructInput sf
                  description = C.translate' (C.LblSocialForm sf)
               in (label, M.ms label <> ": " <> description, sf)
          )
          [Group, Individual]
    reconstructInput Group = "1"
    reconstructInput Individual = "2"

-- | Ability parser
abilityP :: IncrementalParserSpec Ability
abilityP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions _ s =
      matchingInput s $
        map
          ( \ability ->
              let label = reconstructInput ability
                  description = C.translate' (C.LblAbility ability)
               in (label, M.ms label <> ": " <> description, ability)
          )
          [SelfReliant, SelfReliantWithSillyMistakes, WithSupport, NotYet]
    reconstructInput SelfReliant = "1"
    reconstructInput SelfReliantWithSillyMistakes = "2"
    reconstructInput WithSupport = "3"
    reconstructInput NotYet = "4"

-- | Save evidence for a single student
saveStudentEvidence
  :: SyncContext
  -> BulkEditorModel
  -> (UserId, [Observation])
  -> IO ()
saveStudentEvidence r m (uid, observations) = do
  evidenceId <- nextId r
  -- Generate fresh IDs for all observations
  freshObservations <- mapM (refreshObservationId r) observations
  let evidence =
        Evidence
          { id = evidenceId
          , userId = Just uid
          , activityType = m.activityType
          , date = m.date
          , tasks = maybeToList m.selectedTask
          , oldTasks = ""
          , observations = Ix.fromList freshObservations
          , assignmentId = Nothing
          }
  modifySyncDocument r $ Cmd.Evidences $ Cmd.OnEvidences $ Cmd.Create evidence

-- | Create observation with fresh ID
refreshObservationId :: SyncContext -> Observation -> IO Observation
refreshObservationId r obs = do
  newId <- nextId r
  pure $ obs & #id .~ newId
