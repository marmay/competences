module Competences.Frontend.Component.CompetenceGrid
  ( competenceGridComponent
  , CompetenceGridMode (..)
  )
where

import Competences.Command (Command (..), CompetenceAssessmentsCommand (..), CompetenceGridPatch (..), CompetencePatch (..), CompetencesCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceGrid (..)
  , CompetenceId
  , Document (..)
  , Level (..)
  , Lock (..)
  , Order
  , User
  , UserId
  , emptyDocument
  , getActiveAssessment
  , levels
  , orderMax
  , ordered
  )
import Competences.Document.Evidence
  ( ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.Order (orderPosition)
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (translateReorder')
import Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridSelectorComponent
  )
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncContext
  , getFocusedUserRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , subscribeFocusedUser
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (withTooltip)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day, getCurrentTime, utctDay)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Svg.Property qualified as MSP
import Optics.Core ((%), (&), (.~), (?~), (^.))
import Optics.Core qualified as O

-- ============================================================================
-- MODE TYPE
-- ============================================================================

-- | Mode for the competence grid component
data CompetenceGridMode
  = GridView
  | GridEdit
  | GridAssessment
  deriving (Eq, Ord, Enum, Bounded, Show)

-- ============================================================================
-- MAIN COMPONENT
-- ============================================================================

-- | Competence grid component with view/edit mode switching
--
-- Uses SelectorDetail to provide:
-- - Selector on left (with create button for teachers)
-- - Mode switcher when multiple modes available
-- - View mode: displays competence grid with student evidence
-- - Edit mode: allows editing grid and competences
competenceGridComponent
  :: SyncContext
  -> CompetenceGridMode
  -- ^ Initial mode (GridView or GridEdit)
  -> NonEmpty CompetenceGridMode
  -- ^ Available modes (for role-based filtering)
  -> M.Component p (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
competenceGridComponent r initialMode availableModes =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "competence-grid"
      , SD.selectorComponent = \sel ->
          -- Use create style if edit mode is available, otherwise view-only
          let style =
                if GridEdit `elem` availableModes
                  then CompetenceGridSelectorViewAndCreateStyle
                  else CompetenceGridSelectorViewOnlyStyle
           in competenceGridSelectorComponent r style sel
      , SD.detailView = \mode grid ->
          case mode of
            GridView -> viewerDetailView r grid
            GridEdit -> editorDetailView r grid
            GridAssessment -> assessmentDetailView r grid
      , SD.modeLabel = \case
          GridView -> C.translate' C.LblView
          GridEdit -> C.translate' C.LblEdit
          GridAssessment -> C.translate' C.LblAssess
      , SD.modeIcon = \case
          GridView -> Just IcnView
          GridEdit -> Just IcnEdit
          GridAssessment -> Just IcnApply
      , SD.availableModes = availableModes
      , SD.defaultMode = initialMode
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }

-- ============================================================================
-- VIEW MODE DETAIL
-- ============================================================================

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { document :: !Document
  , focusedUser :: !(Maybe User)  -- From global focused user subscription
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
data ViewerAction
  = ViewerUpdateDocument !DocumentChange
  | ViewerFocusedUserChanged !FocusedUserChange
  deriving (Eq, Show)

-- | View for the viewer detail - shows competence grid with student evidence
viewerDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
viewerDetailView r grid =
  V.component
    ("competence-grid-viewer-" <> M.ms (show grid.id))
    (viewerComponent r grid)

viewerComponent :: SyncContext -> CompetenceGrid -> M.Component p ViewerModel ViewerAction
viewerComponent r grid =
  (M.component model update view)
    { M.subs =
        [ subscribeDocument r ViewerUpdateDocument
        , subscribeFocusedUser (getFocusedUserRef r) ViewerFocusedUserChanged
        ]
    }
  where
    model = ViewerModel emptyDocument Nothing

    update (ViewerUpdateDocument (DocumentChange doc _)) =
      M.modify $ #document .~ doc

    update (ViewerFocusedUserChanged change) =
      M.modify $ #focusedUser .~ change.user

    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ header
        , description
        , competencesTable m
        ]
      where
        -- User selector removed - now uses global focused user from nav bar
        header = Typography.h2 (M.ms grid.title)

        description = Typography.paragraph (M.ms grid.description)

        competencesTable vm =
          let evidences = case vm.focusedUser of
                Just user -> vm.document.evidences Ix.@= user.id
                Nothing -> Ix.empty
           in V.viewTable $
                V.defTable
                  { V.columns =
                      [ViewerDescriptionColumn]
                        <> map ViewerLevelColumn levels
                  , V.rows = ordered (vm.document.competences Ix.@= grid.id)
                  , V.columnSpec = \case
                      ViewerDescriptionColumn ->
                        Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                      ViewerLevelColumn l ->
                        Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
                  , V.rowContents = V.cellContents $ \competence -> \case
                      ViewerDescriptionColumn -> Typography.small (M.ms competence.description)
                      ViewerLevelColumn level ->
                        let competenceLevelId = (competence.id, level)
                            levelDescription = M.ms $ fromMaybe "" (competence.levelDescriptions Map.!? level)
                            evidences' = evidences Ix.@= competenceLevelId
                            showEvidence evidence =
                              case Ix.getOne (evidence.observations Ix.@= competenceLevelId) of
                                Just observation ->
                                  showSummary evidence.activityType observation.socialForm observation.ability
                                Nothing -> V.empty
                            showSummary activityType socialForm ability =
                              let abilityClass = Colors.abilityTextClass ability
                                  activityTypeIcn = case activityType of
                                    Conversation -> IcnActivityTypeConversation
                                    Exam -> IcnActivityTypeExam
                                    SchoolExercise -> IcnActivityTypeSchoolExercise
                                    HomeExercise -> IcnActivityTypeHomeExercise
                                  socialFormIcn = case socialForm of
                                    Group -> IcnSocialFormGroup
                                    Individual -> IcnSocialFormIndividual
                                  coloredIcon icn = MH.span_ [class_ abilityClass] [V.icon [MSP.stroke_ "currentColor"] icn]
                               in V.viewFlow V.hFlow [coloredIcon i | i <- [activityTypeIcn, socialFormIcn]]
                         in V.viewFlow
                              (V.vFlow & (#expandOrthogonal .~ V.Expand V.Start))
                              [ Typography.small levelDescription
                              , V.viewFlow
                                  (V.hFlow & (#gap .~ V.SmallSpace) & (#expandDirection .~ V.Expand V.Start))
                                  (V.flowSpring : map showEvidence (Ix.toAscList (Proxy @Day) evidences'))
                              ]
                  }

data ViewerColumn
  = ViewerDescriptionColumn
  | ViewerLevelColumn !Level
  deriving (Eq, Show)

-- ============================================================================
-- EDIT MODE DETAIL
-- ============================================================================

-- | Action for the editor detail component
data EditorAction = CreateNewCompetence
  deriving (Eq, Generic, Show)

-- | View for the editor detail - allows editing grid and competences
editorDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
editorDetailView r grid =
  V.component
    ("competence-grid-editor-" <> M.ms (show grid.id))
    (editorComponent r grid)

editorComponent :: SyncContext -> CompetenceGrid -> M.Component p () EditorAction
editorComponent r grid =
  M.component () update view
  where
    update CreateNewCompetence = M.io_ $ do
      competenceId <- nextId r
      let competence =
            Competence
              { id = competenceId
              , competenceGridId = grid.id
              , order = orderMax
              , description = ""
              , levelDescriptions = Map.empty
              }
      modifySyncDocument r (Competences $ OnCompetences $ CreateAndLock competence)

    view _ =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ V.component
            ("competence-grid-editor-grid-" <> M.ms (show grid.id))
            (TE.editorComponent competenceGridEditor r)
        , V.component
            ("competence-grid-editor-competences-" <> M.ms (show grid.id))
            (TE.editorComponent competencesEditor r)
        , Button.buttonPrimary (C.translate' C.LblAddNewCompetence)
            & Button.withIcon IcnAdd
            & Button.withClick CreateNewCompetence
            & Button.renderButton
        ]

    competenceGridEditable =
      TE.editable
        ( \d -> do
            grid' <- Ix.getOne $ (d ^. #competenceGrids) Ix.@= grid.id
            pure (grid', (d ^. #locks) Map.!? CompetenceGridLock grid'.id)
        )
        & (#modify ?~ (\c m -> Competences $ OnCompetenceGrids (Modify c.id m)))
        & (#delete ?~ (\c -> Competences $ OnCompetenceGrids (Delete c.id)))

    competenceGridEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblCompetenceGrid)
            id
        )
        competenceGridEditable
        `TE.addNamedField` ( C.translate' C.LblCompetenceGridTitle
                           , TE.textEditorField #title #title
                           )
        `TE.addNamedField` ( C.translate' C.LblCompetenceGridDescription
                           , TE.textEditorField #description #description
                           )

    competenceEditable =
      TE.editable
        ( \d ->
            map
              (\c -> (c, (d ^. #locks) Map.!? CompetenceLock c.id))
              (Ix.toAscList (Proxy @Order) ((d ^. #competences) Ix.@= grid.id))
        )
        & (#modify ?~ (\c m -> Competences $ OnCompetences (Modify c.id m)))
        & (#delete ?~ (\c -> Competences $ OnCompetences (Delete c.id)))
        & ( #reorder
              ?~ ( \d c a -> do
                     p <- orderPosition d.competences c.id
                     pure $ Competences $ ReorderCompetence p (translateReorder' (.id) a)
                 )
          )

    competencesEditor =
      TE.editor
        TE.editorTableRowView'
        competenceEditable
        `TE.addNamedField` ( C.translate' C.LblCompetenceDescription
                           , TE.textEditorField #description #description
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription BasicLevel)
                           , TE.textEditorField (#levelDescriptions % O.at BasicLevel % O.non T.empty) (#levelDescriptions % O.at BasicLevel % O.non Nothing)
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription IntermediateLevel)
                           , TE.textEditorField (#levelDescriptions % O.at IntermediateLevel % O.non T.empty) (#levelDescriptions % O.at IntermediateLevel % O.non Nothing)
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription AdvancedLevel)
                           , TE.textEditorField (#levelDescriptions % O.at AdvancedLevel % O.non T.empty) (#levelDescriptions % O.at AdvancedLevel % O.non Nothing)
                           )

-- ============================================================================
-- ASSESSMENT MODE DETAIL
-- ============================================================================

-- | Model for the assessment detail component
data AssessmentModel = AssessmentModel
  { document :: !Document
  , focusedUser :: !(Maybe User)
  , today :: !(Maybe Day) -- Current date for creating/deleting assessments
  }
  deriving (Eq, Generic, Show)

-- | Action for the assessment detail component
data AssessmentAction
  = AssessmentUpdateDocument !DocumentChange
  | AssessmentFocusedUserChanged !FocusedUserChange
  | SetAssessmentLevel !Competence !Level -- Set level for competence (creates/updates today's assessment)
  | ClearAssessment !Competence -- Remove today's assessment
  | InitToday !Day -- Initialize today's date
  deriving (Eq, Show)

-- | View for the assessment detail - allows assessing student competences
assessmentDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
assessmentDetailView r grid =
  V.component
    ("competence-grid-assessment-" <> M.ms (show grid.id))
    (assessmentComponent r grid)

assessmentComponent :: SyncContext -> CompetenceGrid -> M.Component p AssessmentModel AssessmentAction
assessmentComponent r grid =
  (M.component model update view)
    { M.subs =
        [ subscribeDocument r AssessmentUpdateDocument
        , subscribeFocusedUser (getFocusedUserRef r) AssessmentFocusedUserChanged
        ]
    , M.initialAction = Just initTodayAction
    }
  where
    model = AssessmentModel emptyDocument Nothing Nothing

    -- Initialize today's date on mount
    initTodayAction :: AssessmentAction
    initTodayAction = InitToday $ unsafePerformIO $ utctDay <$> getCurrentTime

    update (AssessmentUpdateDocument (DocumentChange doc _)) =
      M.modify $ #document .~ doc

    update (AssessmentFocusedUserChanged change) =
      M.modify $ #focusedUser .~ change.user

    update (InitToday day) =
      M.modify $ #today .~ Just day

    update (SetAssessmentLevel competence level) = do
      m <- M.get
      case (m.focusedUser, m.today) of
        (Just user, Just day) -> M.io_ $ do
          assessmentId <- nextId r
          let assessment =
                CompetenceAssessment
                  { id = assessmentId
                  , userId = user.id
                  , competenceId = competence.id
                  , level = level
                  , date = day
                  , comment = Nothing
                  }
          -- Always Create - command handler handles one-per-day constraint
          modifySyncDocument r $ CompetenceAssessments $ OnCompetenceAssessments $ Create assessment
        _ -> pure ()

    update (ClearAssessment competence) = do
      m <- M.get
      case (m.focusedUser, m.today) of
        (Just user, Just day) -> do
          -- Find today's assessment to delete
          let existingToday = findAssessmentForDay m.document user.id competence.id day
          case existingToday of
            Just assessment ->
              M.io_ $ modifySyncDocument r $ CompetenceAssessments $ OnCompetenceAssessments $ Delete assessment.id
            Nothing -> pure () -- No-op
        _ -> pure ()

    view m = case m.focusedUser of
      Nothing -> Typography.muted (C.translate' C.LblNoStudentSelected)
      Just user ->
        V.viewFlow
          ( V.vFlow
              & (#expandDirection .~ V.Expand V.Start)
              & (#expandOrthogonal .~ V.Expand V.Center)
              & (#gap .~ V.SmallSpace)
          )
          [ header
          , description
          , competenceAssessmentList m user
          ]
      where
        header = Typography.h2 (M.ms grid.title)
        description = Typography.paragraph (M.ms grid.description)

        competenceAssessmentList am u =
          V.viewFlow
            (V.vFlow & (#gap .~ V.MediumSpace))
            [ competenceAssessmentCard am u c
            | c <- ordered (am.document.competences Ix.@= grid.id)
            ]

        competenceAssessmentCard am u competence =
          let evidences = am.document.evidences Ix.@= u.id
              currentAssessment = getActiveAssessment am.document u.id competence.id
              currentLevel = fmap (.level) currentAssessment
              todayAssessment = case am.today of
                Just day -> findAssessmentForDay am.document u.id competence.id day
                Nothing -> Nothing
           in Card.card
                [ competenceHeaderWithButtons competence currentLevel todayAssessment
                , timelineForCompetence competence evidences
                ]

        competenceHeaderWithButtons competence currentLevel todayAssessment =
          V.viewFlow
            (V.hFlow & (#gap .~ V.SmallSpace) & (#expandOrthogonal .~ V.Expand V.Center))
            [ MH.span_ [class_ "flex-1"] [Typography.paragraph (M.ms competence.description)]
            , assessmentButtons competence currentLevel todayAssessment
            ]

        assessmentButtons competence currentLevel todayAssessment =
          let levelBtn lvl =
                (if currentLevel == Just lvl then Button.buttonPrimary else Button.buttonOutline)
                  (C.translate' $ C.LblCompetenceLevelDescription lvl)
                  & Button.withClick (SetAssessmentLevel competence lvl)
                  & Button.renderButton
              clearBtn =
                (if isJust todayAssessment then Button.buttonOutline else Button.buttonGhost)
                  "—"
                  & Button.withClick (ClearAssessment competence)
                  & Button.renderButton
           in V.viewFlow
                (V.hFlow & (#gap .~ V.TinySpace))
                [ levelBtn BasicLevel
                , levelBtn IntermediateLevel
                , levelBtn AdvancedLevel
                , clearBtn
                ]

        timelineForCompetence competence evidences =
          V.viewFlow
            (V.vFlow & (#gap .~ V.TinySpace))
            [ levelRow level competence evidences
            | level <- [AdvancedLevel, IntermediateLevel, BasicLevel]
            ]

        levelRow level competence evidences =
          let competenceLevelId = (competence.id, level)
              levelEvidences = Ix.toAscList (Proxy @Day) (evidences Ix.@= competenceLevelId)
              levelDesc = M.ms $ fromMaybe "" (competence.levelDescriptions Map.!? level)
              levelName = C.translate' (C.LblCompetenceLevelDescription level)
              levelLabel =
                withTooltip levelDesc $
                  MH.span_ [class_ "w-24 text-sm font-medium"] [M.text levelName]
              showEvidenceForLevel evidence =
                case Ix.getOne (evidence.observations Ix.@= competenceLevelId) of
                  Just observation ->
                    showSummary evidence.activityType observation.socialForm observation.ability
                  Nothing -> V.empty
           in V.viewFlow
                (V.hFlow & (#gap .~ V.SmallSpace) & (#expandOrthogonal .~ V.Expand V.Center))
                [ levelLabel
                , V.viewFlow
                    (V.hFlow & (#gap .~ V.SmallSpace) & (#expandDirection .~ V.Expand V.Start))
                    (map showEvidenceForLevel levelEvidences)
                ]

        showSummary activityType socialForm ability =
          let abilityClass = Colors.abilityTextClass ability
              activityTypeIcn = case activityType of
                Conversation -> IcnActivityTypeConversation
                Exam -> IcnActivityTypeExam
                SchoolExercise -> IcnActivityTypeSchoolExercise
                HomeExercise -> IcnActivityTypeHomeExercise
              socialFormIcn = case socialForm of
                Group -> IcnSocialFormGroup
                Individual -> IcnSocialFormIndividual
              coloredIcon icn = MH.span_ [class_ abilityClass] [V.icon [MSP.stroke_ "currentColor"] icn]
           in V.viewFlow V.hFlow [coloredIcon i | i <- [activityTypeIcn, socialFormIcn]]

-- | Find assessment for specific day
findAssessmentForDay :: Document -> UserId -> CompetenceId -> Day -> Maybe CompetenceAssessment
findAssessmentForDay doc userId competenceId day =
  listToMaybe $
    filter (\a -> a.competenceId == competenceId && a.date == day) $
      Ix.toList (doc.competenceAssessments Ix.@= userId)
