module Competences.Frontend.Component.CompetenceGrid
  ( competenceGridComponent
  , CompetenceGridMode (..)
  )
where

import Competences.Command (Command (..), CompetenceGridPatch (..), CompetencePatch (..), CompetencesCommand (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , Lock (..)
  , Order
  , User
  , emptyDocument
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
import Competences.Document.User (User (..), isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (translateReorder')
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridSelectorComponent
  )
import Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , defaultUserSelectorConfig
  , searchableSingleUserSelectorComponent
  )
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day)
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
  :: SyncDocumentRef
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
      , SD.modeLabel = \case
          GridView -> C.translate' C.LblView
          GridEdit -> C.translate' C.LblEdit
      , SD.modeIcon = \case
          GridView -> Just IcnView
          GridEdit -> Just IcnEdit
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
  , selectedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
newtype ViewerAction = ViewerUpdateDocument DocumentChange
  deriving (Eq, Show)

-- | View for the viewer detail - shows competence grid with student evidence
viewerDetailView
  :: SyncDocumentRef
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
viewerDetailView r grid =
  V.component
    ("competence-grid-viewer-" <> M.ms (show grid.id))
    (viewerComponent r grid)

viewerComponent :: SyncDocumentRef -> CompetenceGrid -> M.Component p ViewerModel ViewerAction
viewerComponent r grid =
  (M.component model update view)
    { M.subs = [subscribeDocument r ViewerUpdateDocument]
    }
  where
    model = ViewerModel emptyDocument Nothing

    update (ViewerUpdateDocument (DocumentChange doc _)) =
      M.modify $ #document .~ doc

    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ header m
        , description
        , competencesTable m
        ]
      where
        header _ =
          V.viewFlow
            ( V.hFlow
                & (#gap .~ V.MediumSpace)
                & (#expandDirection .~ V.Expand V.Start)
                & (#expandOrthogonal .~ V.Expand V.Center)
            )
            [ Typography.h2 (M.ms grid.title)
            , V.flowSpring
            , userSelector
            ]

        description = Typography.paragraph (M.ms grid.description)

        userSelector =
          V.component
            "competence-grid-viewer-user-selector"
            ( searchableSingleUserSelectorComponent
                r
                defaultUserSelectorConfig {isPossibleUser = isStudent}
                (selectorLens #selectedUser)
            )

        competencesTable vm =
          let evidences = case vm.selectedUser of
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
  :: SyncDocumentRef
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
editorDetailView r grid =
  V.component
    ("competence-grid-editor-" <> M.ms (show grid.id))
    (editorComponent r grid)

editorComponent :: SyncDocumentRef -> CompetenceGrid -> M.Component p () EditorAction
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
      modifySyncDocument r (Competences $ OnCompetences $ Create competence)
      modifySyncDocument r (Competences $ OnCompetences $ Modify competenceId Lock)

    view _ =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ description
        , V.component
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

    -- Description
    description = Typography.paragraph (M.ms grid.description)

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
