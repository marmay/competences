module Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( competenceGridSelectorComponent
  , CompetenceGridSelectorStyle (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceGridIxs
  , Document (..)
  , Order
  , orderMax
  , User (..)
  )
import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeIxs)
import Competences.Document.Id (Id (..))
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge (BadgeVariant (..), badge)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Modal (modalHost)
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Import.CompetenceGridParser (parseGridImport)
import Competences.Import.Matching (matchGridImport)
import Competences.Import.Types
  ( CompetenceImportAction (..)
  , GridImportPreview (..)
  , ImportAction (..)
  , ParsedCompetence (..)
  , levelToGerman
  )
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Event.Types (stopPropagation)
import Miso.Html qualified as M
import Miso.Html.Event (onClickWithOptions)
import Miso.Html.Property qualified as MP
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

-- | Projection type: extracts only the data needed for this component.
-- Grid grades are filtered to only the focused user's grades.
data GridSelectorProjection = GridSelectorProjection
  { allGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , userGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Compute the projection from document and focused user.
-- Filters grid grades to only those for the focused user.
gridSelectorProjection :: Document -> Maybe User -> GridSelectorProjection
gridSelectorProjection doc mUser = GridSelectorProjection
  { allGrids = doc.competenceGrids
  , userGridGrades = case mUser of
      Nothing -> Ix.empty
      Just u -> doc.competenceGridGrades Ix.@= u.id
  , focusedUser = mUser
  }

data Model = Model
  { projection :: !GridSelectorProjection
  , selectedCompetenceGrid :: !(Maybe CompetenceGrid)
  , newCompetenceGrid :: !(Maybe CompetenceGrid)
  , isDropdownOpen :: !Bool
  -- Import modal state
  , showImportModal :: !Bool
  , importInputText :: !Text
  , importParseResult :: !(Either String [GridImportPreview])
  }
  deriving (Eq, Generic, Show)

data Action
  = NoOp
  | SelectCompetenceGrid !CompetenceGrid
  | CreateNewCompetenceGrid
  | ProjectionChanged !(ProjectedChange GridSelectorProjection)
  | ToggleDropdown
  | OpenImportModal
  | CloseImportModal
  | SetImportInputText !Text
  | ParseImportInput
  | ApplyImport
  deriving (Eq, Show)

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

competenceGridSelectorComponent
  :: SyncContext
  -> CompetenceGridSelectorStyle
  -> Lens' p (Maybe CompetenceGrid)
  -> M.Component p Model Action
competenceGridSelectorComponent r style parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedCompetenceGrid]
    , M.subs = [subscribeWithProjection r gridSelectorProjection ProjectionChanged]
    }
  where
    model = Model
      { projection = GridSelectorProjection Ix.empty Ix.empty Nothing
      , selectedCompetenceGrid = Nothing
      , newCompetenceGrid = Nothing
      , isDropdownOpen = False
      , showImportModal = False
      , importInputText = ""
      , importParseResult = Right []
      }

    update NoOp = pure ()

    update (SelectCompetenceGrid c) =
      M.modify $ \m -> case Ix.getOne (m.projection.allGrids Ix.@= c.id) of
        Just c' -> m & (#selectedCompetenceGrid ?~ c') & (#newCompetenceGrid .~ Nothing)
        Nothing -> m & (#newCompetenceGrid ?~ c)

    update CreateNewCompetenceGrid = M.withSink $ \s -> do
      competenceGridId <- nextId r
      let competenceGrid = CompetenceGrid competenceGridId orderMax "" ""
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create competenceGrid)
      s (SelectCompetenceGrid competenceGrid)

    update (ProjectionChanged change) = M.modify $ updateFromProjection change.projection

    update ToggleDropdown = M.modify $ \m -> m & #isDropdownOpen .~ not m.isDropdownOpen

    update OpenImportModal = M.modify $ \m ->
      m & #isDropdownOpen .~ False
        & #showImportModal .~ True
        & #importInputText .~ ""
        & #importParseResult .~ Right []

    update CloseImportModal = M.modify $ #showImportModal .~ False

    update (SetImportInputText t) = M.modify $ #importInputText .~ t

    update ParseImportInput = M.modify $ \m ->
      let doc = getDocument m.projection
          result = case parseGridImport m.importInputText of
            Left err -> Left err
            Right parsed -> Right $ matchGridImport doc parsed
       in m & #importParseResult .~ result

    update ApplyImport = do
      m <- M.get
      let doc = getDocument m.projection
      case m.importParseResult of
        Right previews -> M.io_ $ applyGridPreviews r doc previews
        Left _ -> pure ()
      M.modify $ #showImportModal .~ False

    getDocument :: GridSelectorProjection -> Document
    getDocument proj = Document
      { competenceGrids = proj.allGrids
      , competences = Ix.empty  -- Not needed for grid import
      , users = Ix.empty
      , evidences = Ix.empty
      , locks = mempty
      , tasks = Ix.empty
      , taskGroups = Ix.empty
      , solutions = Ix.empty
      , resources = Ix.empty
      , assignments = Ix.empty
      , competenceAssessments = Ix.empty
      , competenceGridGrades = Ix.empty
      }

    updateFromProjection :: GridSelectorProjection -> Model -> Model
    updateFromProjection proj m =
      let grids = proj.allGrids
          validateCompetenceGrid c = do
            c' <- c
            Ix.getOne $ grids Ix.@= c'.id
          (selected', new') = case (validateCompetenceGrid m.selectedCompetenceGrid, validateCompetenceGrid m.newCompetenceGrid) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { projection = proj
            , selectedCompetenceGrid = selected'
            , newCompetenceGrid = new'
            }

    view (m :: Model) =
      M.div_
        []
        [ V.viewFlow
            ( V.vFlow
                & (#gap .~ V.SmallSpace)
                & (#expandDirection .~ V.Expand V.Start)
                & (#extraAttrs .~ [V.fullHeight])
            )
            [ case style of
                CompetenceGridSelectorViewOnlyStyle ->
                  SL.selectorHeader (C.translate' C.LblSelectCompetenceGrids) Nothing
                CompetenceGridSelectorViewAndCreateStyle ->
                  SL.selectorHeaderWithDropdown
                    (C.translate' C.LblSelectCompetenceGrids)
                    m.isDropdownOpen
                    ToggleDropdown
                    [ SL.dropdownItem IcnAdd (C.translate' C.LblCreate) CreateNewCompetenceGrid
                    , SL.dropdownItem IcnImport (C.translate' C.LblImportCompetenceGrids) OpenImportModal
                    ]
            , SL.selectorList (map (viewCompetenceGrid m) (Ix.toAscList (Proxy @Order) m.projection.allGrids))
            ]
        , if m.showImportModal then importModalView m else M.text ""
        ]

    importModalView m' =
      modalHost
        [M.onClick CloseImportModal]
        [ M.div_
            [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg w-[80vw] h-[80vh] max-w-[80vw] flex flex-col"
            , onClickWithOptions stopPropagation NoOp
            ]
            [ -- Header
              M.div_
                [class_ "flex items-center justify-between p-4 border-b border-border"]
                [ Typography.h2 (C.translate' C.LblImportCompetenceGrids)
                , Button.buttonGhost ""
                    & Button.withIcon IcnCancel
                    & Button.withClick CloseImportModal
                    & Button.renderButton
                ]
            , -- Content
              M.div_
                [class_ "flex-1 min-h-0 flex gap-4 p-4 overflow-hidden"]
                [ -- Left: Input area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Eingabe"
                    , M.textarea_
                        [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                        , MP.placeholder_ "# Rastername\n\n## Kompetenzbeschreibung\n- Wesentlich: ...\n- Mittelstufe: ...\n- Fortgeschritten: ..."
                        , MP.value_ (M.ms m'.importInputText)
                        , M.onInput (SetImportInputText . M.fromMisoString)
                        ]
                        []
                    ]
                , -- Right: Preview area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Vorschau"
                    , M.div_
                        [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                        [importPreviewView m'.importParseResult]
                    ]
                ]
            , -- Footer
              M.div_
                [class_ "flex justify-end gap-2 p-4 border-t border-border"]
                [ Button.buttonSecondary (C.translate' C.LblCancel)
                    & Button.withClick CloseImportModal
                    & Button.renderButton
                , Button.buttonPrimary "Vorschau"
                    & Button.withClick ParseImportInput
                    & Button.renderButton
                , case m'.importParseResult of
                    Right previews
                      | not (null previews) && any hasChanges previews ->
                          Button.buttonPrimary (C.translate' C.LblApply)
                            & Button.withIcon IcnApply
                            & Button.withClick ApplyImport
                            & Button.renderButton
                    _ -> M.text ""
                ]
            ]
        ]

    viewCompetenceGrid m c =
      let isSelected = m.selectedCompetenceGrid == Just c || m.newCompetenceGrid == Just c
          label = M.ms $ if c.title == "" then "Ohne Titel" else c.title
          -- Get active grade for this grid and focused user
          -- userGridGrades is already filtered to the focused user
          mGrade = do
            user <- m.projection.focusedUser
            gridGrade <- getActiveGridGrade' m.projection.userGridGrades user.id c.id
            pure gridGrade.grade
          gradeBadge = gradeBadgeView <$> mGrade
       in SL.selectorItemWithBadge isSelected IcnCompetenceGrid label gradeBadge (SelectCompetenceGrid c)

-- | Get the most recent (active) grid grade for a user and competence grid.
-- Uses IxSet indexing for efficient lookup.
getActiveGridGrade'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> UserId
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
getActiveGridGrade' grades userId gridId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $
    grades Ix.@= userId Ix.@= gridId

-- ============================================================================
-- Import Preview View
-- ============================================================================

importPreviewView :: Either String [GridImportPreview] -> M.View Model Action
importPreviewView = \case
  Left err ->
    M.div_
      [class_ "text-destructive"]
      [M.text $ M.ms $ "Fehler: " <> err]
  Right [] ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "Keine Eingabe. Geben Sie Text ein und klicken Sie auf 'Vorschau'."]
  Right previews ->
    M.div_
      [class_ "flex flex-col gap-4"]
      (map previewGridView previews)

previewGridView :: GridImportPreview -> M.View Model Action
previewGridView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ M.div_
        [class_ "flex items-center gap-2 mb-2"]
        [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ gridTitle preview.gridAction]
        , actionBadge preview.gridAction
        ]
    , M.div_
        [class_ "flex flex-col gap-2"]
        (map previewCompetenceView preview.competenceActions)
    ]

gridTitle :: ImportAction CompetenceGrid -> Text
gridTitle (Create g) = g.title
gridTitle (Update _ g) = g.title
gridTitle (NoChange g) = g.title

previewCompetenceView :: CompetenceImportAction -> M.View Model Action
previewCompetenceView ca =
  M.div_
    [class_ "pl-4 border-l-2 border-border"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ M.span_
            [class_ "font-medium text-sm"]
            [M.text $ M.ms ca.parsedCompetence.description]
        , actionBadge ca.action
        ]
    , M.div_
        [class_ "text-xs text-muted-foreground mt-1"]
        (levelPreview ca.parsedCompetence.levels)
    ]

levelPreview :: Map.Map Level Text -> [M.View Model Action]
levelPreview levels =
  map levelItem [BasicLevel, IntermediateLevel, AdvancedLevel]
  where
    levelItem lvl = case Map.lookup lvl levels of
      Nothing -> M.text ""
      Just desc ->
        M.div_
          []
          [ M.span_
              [class_ "font-medium"]
              [M.text $ M.ms $ levelToGerman lvl <> ": "]
          , M.text $ M.ms $ T.take 40 desc <> if T.length desc > 40 then "..." else ""
          ]

actionBadge :: ImportAction a -> M.View Model Action
actionBadge (Create _) = badge BadgePrimary "Neu"
actionBadge (Update _ _) = badge BadgeSecondary "Aktualisiert"
actionBadge (NoChange _) = badge BadgeOutline "Unverändert"

-- ============================================================================
-- Apply Import
-- ============================================================================

hasChanges :: GridImportPreview -> Bool
hasChanges preview =
  isChange preview.gridAction
    || any (\ca -> isChange ca.action) preview.competenceActions
  where
    isChange (Create _) = True
    isChange (Update _ _) = True
    isChange (NoChange _) = False

-- | Apply all grid import previews
applyGridPreviews :: SyncContext -> Document -> [GridImportPreview] -> IO ()
applyGridPreviews r doc previews = mapM_ (applyGridPreview r doc) previews

-- | Apply a single grid import preview
applyGridPreview :: SyncContext -> Document -> GridImportPreview -> IO ()
applyGridPreview r _doc preview = do
  -- Handle grid action
  gridId <- case preview.gridAction of
    Create g -> do
      newId <- nextId r
      let newGrid =
            CompetenceGrid
              { id = newId
              , order = orderMax
              , title = g.title
              , description = g.description
              }
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create newGrid)
      pure newId
    Update _old new -> pure new.id -- Updates not yet implemented
    NoChange g -> pure g.id

  -- Handle competence actions
  mapM_ (applyCompetenceAction r gridId) preview.competenceActions

-- | Apply a single competence import action
applyCompetenceAction :: SyncContext -> Id CompetenceGrid -> CompetenceImportAction -> IO ()
applyCompetenceAction r gridId ca = case ca.action of
  Create c -> do
    newId <- nextId r
    let newComp =
          Competence
            { id = newId
            , competenceGridId = gridId
            , order = orderMax
            , description = c.description
            , levels = c.levels
            }
    modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Create newComp)
  Update _ _ -> pure () -- Updates not yet implemented
  NoChange _ -> pure ()
