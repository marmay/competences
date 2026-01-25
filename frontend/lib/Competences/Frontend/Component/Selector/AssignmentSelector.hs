module Competences.Frontend.Component.Selector.AssignmentSelector
  ( assignmentSelectorComponent
  , searchableSingleAssignmentEditorField
  )
where

import Competences.Command qualified as Cmd
import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..), User (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..), mkAssignment)
import Competences.Document.Id (Id (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge (BadgeVariant (..), badge)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Modal (modalHost)
import Competences.Frontend.View.SelectorList qualified as SelectorList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Import.AssignmentParser (parseAssignmentImport)
import Competences.Import.Matching (matchAssignmentImport)
import Competences.Import.Types
  ( AssignmentImportPreview (..)
  , CompetenceMatch (..)
  , ImportAction (..)
  , TaskImportPreview (..)
  , activityTypeToGerman
  , levelToGerman
  )
import Competences.Import.Types qualified as Import
import Competences.Query.Assignment (AssignmentStatus (..), assignmentStatus)
import Data.Default (Default)
import Data.List (find, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Event.Types (stopPropagation)
import Miso.Html qualified as M
import Miso.Html.Event (onClickWithOptions)
import Miso.Html.Property qualified as MP
import Miso.String (fromMisoString, ms)
import Optics.Core (Lens', castOptic, toLensVL, (&), (.~), (%~), (?~), (^.))

-- | Projection from document + focused user
data SelectorProjection = SelectorProjection
  { assignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , focusedUser :: !(Maybe User)
    -- | Pre-computed status for each assignment (only when focusedUser is set)
  , statusMap :: !(Map.Map AssignmentId AssignmentStatus)
  }
  deriving (Eq, Generic, Show)

emptyProjection :: SelectorProjection
emptyProjection = SelectorProjection Ix.empty Nothing Map.empty

-- | Projection function - pre-computes all assignment statuses
-- Filters assignments by focused user if set (shows only assignments assigned to that user)
selectorProjection :: Document -> Maybe User -> SelectorProjection
selectorProjection doc mUser =
  let -- Filter assignments by focused user if set
      assignments = case mUser of
        Nothing -> doc.assignments  -- No focused user, show all
        Just user -> doc.assignments Ix.@= user.id  -- Filter by focused user's studentIds
      statusMap = case mUser of
        Nothing -> Map.empty
        Just user -> Map.fromList
          [ (a.id, assignmentStatus doc user.id a.id)
          | a <- Ix.toList assignments
          ]
   in SelectorProjection
        { assignments
        , focusedUser = mUser
        , statusMap
        }

data Model = Model
  { projection :: !SelectorProjection
  , selectedAssignment :: !(Maybe Assignment)  -- bound to parent
  , newAssignment :: !(Maybe Assignment)       -- temporary for new assignments
  , searchQuery :: !Text
  , showIncompleteOnly :: !Bool
  , isDropdownOpen :: !Bool
  -- Import modal state
  , showImportModal :: !Bool
  , importInputText :: !Text
  , importParseResult :: !(Either String [AssignmentImportPreview])
  }
  deriving (Eq, Generic, Show)

data Action
  = NoOp
  | SelectAssignment !Assignment
  | CreateNewAssignment
  | SetSearchQuery !Text
  | SetShowIncompleteOnly !Bool
  | ProjectionChanged !(ProjectedChange SelectorProjection)
  | ToggleDropdown
  | OpenImportModal
  | CloseImportModal
  | SetImportInputText !Text
  | ParseImportInput
  | ApplyImport
  deriving (Eq, Show)

assignmentSelectorComponent
  :: SyncContext -> Lens' p (Maybe Assignment) -> M.Component p Model Action
assignmentSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedAssignment]
    , M.subs = [subscribeWithProjection r selectorProjection ProjectionChanged]
    }
  where
    model = Model
      { projection = emptyProjection
      , selectedAssignment = Nothing
      , newAssignment = Nothing
      , searchQuery = ""
      , showIncompleteOnly = True  -- Default to showing only not-graded assignments
      , isDropdownOpen = False
      , showImportModal = False
      , importInputText = ""
      , importParseResult = Right []
      }

    update NoOp = pure ()

    update (SelectAssignment a) =
      M.modify $ \m -> case Ix.getOne (m.projection.assignments Ix.@= a.id) of
        Just a' -> m & (#selectedAssignment ?~ a') & (#newAssignment .~ Nothing)
        Nothing -> m & (#newAssignment ?~ a)

    update CreateNewAssignment = M.withSink $ \s -> do
      assignmentId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let newAssignment = mkAssignment assignmentId (AssignmentName "") today
      modifySyncDocument r $ Assignments (OnAssignments (CreateAndLock newAssignment))
      s (SelectAssignment newAssignment)

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (SetShowIncompleteOnly b) = M.modify $ \m ->
      m & #showIncompleteOnly .~ b

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection

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
          result = case parseAssignmentImport m.importInputText of
            Left err -> Left err
            Right parsed -> Right $ matchAssignmentImport doc parsed
       in m & #importParseResult .~ result

    update ApplyImport = do
      m <- M.get
      let doc = getDocument m.projection
      case m.importParseResult of
        Right previews -> M.io_ $ applyAssignmentPreviews r doc previews
        Left _ -> pure ()
      M.modify $ #showImportModal .~ False

    getDocument :: SelectorProjection -> Document
    getDocument proj = Document
      { competenceGrids = Ix.empty
      , competences = Ix.empty
      , users = Ix.empty
      , evidences = Ix.empty
      , locks = mempty
      , tasks = Ix.empty
      , taskGroups = Ix.empty
      , solutions = Ix.empty
      , resources = Ix.empty
      , assignments = proj.assignments
      , competenceAssessments = Ix.empty
      , competenceGridGrades = Ix.empty
      }

    view' m =
      M.div_
        []
        [ V.viewFlow
            ( V.vFlow
                & (#gap .~ V.SmallSpace)
                & (#expandDirection .~ V.Expand V.Start)
                & (#extraAttrs .~ [V.fullHeight])
            )
            [ SelectorList.selectorHeaderWithDropdown
                (C.translate' C.LblAssignments)
                m.isDropdownOpen
                ToggleDropdown
                [ SelectorList.dropdownItem IcnAdd (C.translate' C.LblCreate) CreateNewAssignment
                , SelectorList.dropdownItem IcnImport (C.translate' C.LblImportAssignments) OpenImportModal
                ]
            , SelectorList.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterAssignments) (SetSearchQuery . M.fromMisoString)
            , viewStatusFilters m
            , SelectorList.selectorList (map (viewAssignment m) (filteredAssignments m))
            ]
        , if m.showImportModal then importModalView m else M.text ""
        ]

    importModalView m' =
      modalHost
        [M.onClick CloseImportModal]
        [ M.div_
            [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg max-w-4xl w-full mx-4 max-h-[90vh] flex flex-col"
            , onClickWithOptions stopPropagation NoOp
            ]
            [ -- Header
              M.div_
                [class_ "flex items-center justify-between p-4 border-b border-border"]
                [ Typography.h2 (C.translate' C.LblImportAssignments)
                , Button.buttonGhost ""
                    & Button.withIcon IcnCancel
                    & Button.withClick CloseImportModal
                    & Button.renderButton
                ]
            , -- Content
              M.div_
                [class_ "flex-1 min-h-0 grid grid-cols-2 gap-4 p-4 overflow-hidden"]
                [ -- Left: Input area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0"]
                    [ Typography.h3 "Eingabe"
                    , M.textarea_
                        [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                        , MP.placeholder_ assignmentPlaceholderText
                        , MP.value_ (M.ms m'.importInputText)
                        , M.onInput (SetImportInputText . M.fromMisoString)
                        ]
                        []
                    ]
                , -- Right: Preview area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0"]
                    [ Typography.h3 "Vorschau"
                    , M.div_
                        [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                        [assignmentPreviewView m'.importParseResult]
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
                      | not (null previews) && any hasAssignmentChanges previews ->
                          Button.buttonPrimary (C.translate' C.LblApply)
                            & Button.withIcon IcnApply
                            & Button.withClick ApplyImport
                            & Button.renderButton
                    _ -> M.text ""
                ]
            ]
        ]

    assignmentPlaceholderText :: M.MisoString
    assignmentPlaceholderText =
      "# Aufgabenname\n\n\
      \## Beschreibung\n\
      \Beschreibungstext...\n\n\
      \## Angaben\n\
      \Date: 2026-01-25\n\
      \Type: Hausübung\n\n\
      \### Buch-1.2.3\n\n\
      \#### Angabe\n\
      \Aufgabentext...\n\n\
      \#### Kompetenzen\n\
      \- Rastername / Kompetenz / Wesentlich\n\n\
      \#### Hinweis\n\
      \Optionaler Hinweis..."

    viewStatusFilters m =
      case m.projection.focusedUser of
        Nothing -> M.text "" -- No filters when no user is focused
        Just _ ->
          M.div_
            [class_ "flex gap-1"]
            [ filterButton m False "Alle"
            , filterButton m True "Nicht korrigiert"
            ]

    filterButton m filterValue label =
      let isActive = m.showIncompleteOnly == filterValue
          baseClass = "px-2 py-1 text-xs rounded-full cursor-pointer transition-colors "
          activeClass = if isActive then "bg-primary text-primary-foreground" else "bg-muted hover:bg-muted/80"
       in M.button_
            [ class_ (baseClass <> activeClass)
            , M.onClick (SetShowIncompleteOnly filterValue)
            ]
            [M.text label]

    filteredAssignments m =
      let proj = m.projection
          query = T.toLower m.searchQuery
          sorted = sortOn (.assignmentDate) $ Ix.toList proj.assignments
          textFiltered =
            if T.null query
              then sorted
              else filter (\a -> query `T.isInfixOf` T.toLower (unAssignmentName a.name)) sorted
          -- Check if assignment is not graded (no evidence linked)
          isNotGraded a = case Map.lookup a.id proj.statusMap of
            Just NotGraded -> True
            _ -> False  -- NeedsWork and Completed are both "graded"
       in case (proj.focusedUser, m.showIncompleteOnly) of
            (Just _, True) -> filter isNotGraded textFiltered
            _ -> textFiltered

    unAssignmentName (AssignmentName t) = t

    viewAssignment m a =
      let proj = m.projection
          isSelected = m.selectedAssignment == Just a || m.newAssignment == Just a
          mStatus = do
            _ <- proj.focusedUser  -- Only show status if user is focused
            Map.lookup a.id proj.statusMap
       in SelectorList.selectorItemMultiLine isSelected
            [ -- Line 1: Icon + Name
              M.div_
                [class_ "flex items-center gap-2"]
                [ V.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] IcnAssignment
                , M.span_ [class_ "text-sm truncate font-medium"] [M.text $ ms $ unAssignmentName a.name]
                ]
            , -- Line 2: Date + Status
              M.div_
                [class_ "flex items-center gap-2 text-xs text-muted-foreground"]
                [ M.span_ [] [M.text $ C.formatDay a.assignmentDate]
                , case mStatus of
                    Just status -> statusIcon status
                    Nothing -> M.text ""
                ]
            ]
            (SelectAssignment a)

    -- | Status icon display: growing icon (yellow) for NeedsWork, checkmark (green) for Completed
    statusIcon :: AssignmentStatus -> M.View Model Action
    statusIcon NotGraded = M.text ""  -- No icon for not graded
    statusIcon NeedsWork = V.icon [class_ "w-4 h-4 text-yellow-500"] IcnProgress
    statusIcon Completed = V.icon [class_ "w-4 h-4 text-green-600"] IcnApply

-- ============================================================================
-- ASSIGNMENT EDITOR FIELD (for use in Evidence editor etc.)
-- ============================================================================

-- | Searchable single-assignment editor field for use in editors
-- Uses a read-only viewer (assignment name or placeholder) and searchable combobox for editing
searchableSingleAssignmentEditorField
  :: (Eq t, Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens p patch Maybe Assignment Maybe t
  -> EditorField p patch f
searchableSingleAssignmentEditorField r k eptl =
  let config e =
        AssignmentEditorConfig
          { isInitialAssignment = \a -> e ^. eptl.viewLens == Just (eptl.transform a)
          }
   in selectorEditorFieldWithViewer
        k
        eptl
        (selectedAssignmentViewerComponent r . config)
        (searchableSingleAssignmentSelectorComponent r . config)

-- | Configuration for assignment editor components
data AssignmentEditorConfig = AssignmentEditorConfig
  { isInitialAssignment :: Assignment -> Bool
  }

-- ============================================================================
-- EDITOR FIELD PROJECTION (for viewer and selector)
-- ============================================================================

-- | Projection for assignment editor field components
-- Filters assignments by focused user if set
data EditorFieldProjection = EditorFieldProjection
  { assignments :: ![Assignment]
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Projection function for editor field - filters by focused user
editorFieldProjection :: Document -> Maybe User -> EditorFieldProjection
editorFieldProjection doc mUser =
  let -- Filter assignments by focused user if set
      filteredAssignments = case mUser of
        Nothing -> Ix.toList doc.assignments  -- No focused user, show all
        Just user -> Ix.toList $ doc.assignments Ix.@= user.id  -- Filter by focused user
      sorted = sortOn (.assignmentDate) filteredAssignments
   in EditorFieldProjection
        { assignments = sorted
        , focusedUser = mUser
        }

-- ============================================================================
-- VIEWER COMPONENT (Read-only display)
-- ============================================================================

-- | Model for the selected assignment viewer
data SelectedAssignmentViewerModel = SelectedAssignmentViewerModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

-- | Action for the selected assignment viewer
newtype SelectedAssignmentViewerAction = AssignmentViewerProjectionChanged (ProjectedChange EditorFieldProjection)
  deriving (Eq, Show)

-- | Component that displays selected assignment name or placeholder
selectedAssignmentViewerComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p SelectedAssignmentViewerModel SelectedAssignmentViewerAction
selectedAssignmentViewerComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (castOptic #selectedValue)]
    , M.subs = [subscribeWithProjection r editorFieldProjection AssignmentViewerProjectionChanged]
    }
  where
    model =
      SelectedAssignmentViewerModel
        { possibleValues = []
        , selectedValue = Nothing
        }

    update (AssignmentViewerProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue =
              if change.changeInfo == InitialSnapshot
                then case filter config.isInitialAssignment newPossibleValues of
                       (a : _) -> Just a
                       [] -> Nothing
                else m.selectedValue >>= \sel -> find (\a -> a.id == sel.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)

    view m = viewSelectedAssignment m.selectedValue

-- | Render assignment name or placeholder
viewSelectedAssignment :: Maybe Assignment -> M.View m a
viewSelectedAssignment = \case
  Nothing -> Typography.muted (C.translate' C.LblNoAssignmentSelected)
  Just a -> M.span_ [] [M.text $ ms $ unAssignmentName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"]
  where
    unAssignmentName (AssignmentName t) = t

-- ============================================================================
-- SELECTOR COMPONENT (Searchable dropdown with focused user filtering)
-- ============================================================================

-- | Model for searchable single-select assignment selector
data AssignmentSelectorModel = AssignmentSelectorModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Actions for searchable single-select assignment selector
data AssignmentSelectorAction
  = SelectorProjectionChanged !(ProjectedChange EditorFieldProjection)
  | SelectorToggle !Assignment
  | SelectorSetSearchQuery !Text
  | SelectorSetOpen !Bool
  deriving (Eq, Show)

-- | Searchable single-select assignment component with focused user filtering
searchableSingleAssignmentSelectorComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p AssignmentSelectorModel AssignmentSelectorAction
searchableSingleAssignmentSelectorComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedValue]
    , M.subs = [subscribeWithProjection r editorFieldProjection SelectorProjectionChanged]
    }
  where
    model =
      AssignmentSelectorModel
        { possibleValues = []
        , selectedValue = Nothing
        , searchQuery = ""
        , isOpen = False
        }

    update (SelectorProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue
              | change.changeInfo == InitialSnapshot = find config.isInitialAssignment newPossibleValues
              | otherwise = m.selectedValue >>= \v -> find (\a -> a.id == v.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)
    update (SelectorToggle a) =
      M.modify $ \m ->
        m
          & (#selectedValue %~ \s -> if (fmap (.id) s) == Just a.id then Nothing else Just a)
          & (#isOpen .~ False) -- Close dropdown after selection
    update (SelectorSetSearchQuery q) =
      M.modify (#searchQuery .~ q)
    update (SelectorSetOpen open) =
      M.modify (#isOpen .~ open)

    view m =
      let options =
            map
              (\v -> Combobox.ComboboxOption v (fromMisoString $ showAssignment v))
              m.possibleValues
          selectedSet = maybe Set.empty Set.singleton m.selectedValue
          displayTxt = fmap (fromMisoString . showAssignment) m.selectedValue
       in Combobox.singleSelectCombobox
            SelectorSetSearchQuery
            SelectorToggle
            SelectorSetOpen
            & Combobox.withPlaceholder (fromMisoString $ C.translate' C.LblSelectAssignment)
            & Combobox.withOptions options
            & Combobox.withSelected selectedSet
            & Combobox.withDisplayText displayTxt
            & Combobox.withSearchQuery m.searchQuery
            & Combobox.withIsOpen m.isOpen
            & Combobox.renderCombobox

    showAssignment a = ms $ unAssignmentName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"
    unAssignmentName (AssignmentName t) = t

-- ============================================================================
-- Assignment Import Preview View
-- ============================================================================

assignmentPreviewView :: Either String [AssignmentImportPreview] -> M.View Model Action
assignmentPreviewView = \case
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
      (map previewAssignmentView previews)

previewAssignmentView :: AssignmentImportPreview -> M.View Model Action
previewAssignmentView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ -- Assignment header
      M.div_
        [class_ "flex items-center gap-2 mb-2"]
        [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ assignmentName preview.assignmentAction]
        , assignmentActionBadge preview.assignmentAction
        ]
    , -- Assignment metadata
      M.div_
        [class_ "text-sm text-muted-foreground mb-2"]
        [ M.text $ M.ms $ formatMetadata preview.assignmentAction
        ]
    , -- Tasks
      if null preview.taskPreviews
        then M.text ""
        else
          M.div_
            [class_ "pl-4 border-l-2 border-border space-y-2"]
            (map previewTaskView preview.taskPreviews)
    ]

assignmentName :: ImportAction Assignment -> Text
assignmentName (Import.Create a) = let AssignmentName n = a.name in n
assignmentName (Import.Update _ a) = let AssignmentName n = a.name in n
assignmentName (Import.NoChange a) = let AssignmentName n = a.name in n

formatMetadata :: ImportAction Assignment -> Text
formatMetadata action =
  let a = case action of
        Import.Create x -> x
        Import.Update _ x -> x
        Import.NoChange x -> x
   in formatDay a.assignmentDate <> " | " <> activityTypeToGerman a.activityType

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

previewTaskView :: TaskImportPreview -> M.View Model Action
previewTaskView preview =
  M.div_
    [class_ "py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ M.span_ [class_ "font-medium text-sm"] [M.text $ M.ms $ taskTitle preview.taskAction]
        , assignmentActionBadge preview.taskAction
        ]
    , -- Solutions count
      if null preview.solutionActions
        then M.text ""
        else
          M.div_
            [class_ "text-xs text-muted-foreground mt-1"]
            [M.text $ M.ms $ "Lösungen: " <> T.pack (show (length preview.solutionActions))]
    , -- Competence matches
      if null preview.competenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            (map competenceMatchView preview.competenceMatches)
    ]

taskTitle :: ImportAction Task -> Text
taskTitle (Import.Create t) = let TaskIdentifier ident = t.identifier in ident
taskTitle (Import.Update _ t) = let TaskIdentifier ident = t.identifier in ident
taskTitle (Import.NoChange t) = let TaskIdentifier ident = t.identifier in ident

competenceMatchView :: CompetenceMatch -> M.View Model Action
competenceMatchView cm =
  M.div_
    [class_ "flex items-center gap-1 text-xs"]
    [ M.span_ [class_ "text-muted-foreground"] [M.text $ M.ms cm.gridName]
    , M.span_ [] [M.text "/"]
    , M.span_ [] [M.text $ M.ms $ T.take 20 cm.description <> if T.length cm.description > 20 then "..." else ""]
    , badge BadgeOutline (M.ms $ levelToGerman cm.level)
    , case cm.matched of
        Just _ -> badge BadgePrimary "OK"
        Nothing -> badge BadgeDestructive "?"
    ]

assignmentActionBadge :: ImportAction a -> M.View Model Action
assignmentActionBadge (Import.Create _) = badge BadgePrimary "Neu"
assignmentActionBadge (Import.Update _ _) = badge BadgeSecondary "Aktualisiert"
assignmentActionBadge (Import.NoChange _) = badge BadgeOutline "Unverändert"

-- ============================================================================
-- Apply Assignment Import
-- ============================================================================

hasAssignmentChanges :: AssignmentImportPreview -> Bool
hasAssignmentChanges preview =
  isChange preview.assignmentAction
    || any taskHasChanges preview.taskPreviews
  where
    isChange (Import.Create _) = True
    isChange (Import.Update _ _) = True
    isChange (Import.NoChange _) = False

    taskHasChanges tp =
      isChange tp.taskAction
        || any isChange tp.solutionActions

-- | Apply all assignment import previews
applyAssignmentPreviews :: SyncContext -> Document -> [AssignmentImportPreview] -> IO ()
applyAssignmentPreviews r doc previews = mapM_ (applyAssignmentPreview r doc) previews

-- | Apply a single assignment import preview
applyAssignmentPreview :: SyncContext -> Document -> AssignmentImportPreview -> IO ()
applyAssignmentPreview r doc preview = do
  -- First, apply all tasks and collect their IDs
  taskIds <- mapM (applyTaskAndGetId r doc) preview.taskPreviews

  -- Then create/update the assignment with the task IDs
  case preview.assignmentAction of
    Import.Create a -> do
      newId <- nextId r
      let newAssignment =
            Assignment
              { id = newId
              , name = a.name
              , description = a.description
              , assignmentDate = a.assignmentDate
              , activityType = a.activityType
              , studentIds = Set.empty -- Start with no students
              , tasks = taskIds
              }
      modifySyncDocument r (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Create newAssignment)
    Import.Update _ _ -> do
      -- For updates, we'd need to modify the assignment
      -- Currently not implemented
      pure ()
    Import.NoChange _ -> pure ()

-- | Apply a task preview and return its ID
applyTaskAndGetId :: SyncContext -> Document -> TaskImportPreview -> IO (Id Task)
applyTaskAndGetId r doc preview = do
  -- Find a teacher to use as solution author
  let teachers = filter isTeacher $ Ix.toList doc.users
      mTeacherId = (.id) <$> listToMaybe teachers

  taskId <- case preview.taskAction of
    Import.Create t -> do
      newId <- nextId r
      let newTask =
            Task
              { id = newId
              , identifier = t.identifier
              , content = t.content
              , taskType = t.taskType
              }
      modifySyncDocument r (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Create newTask)
      pure newId
    Import.Update _ new -> pure new.id
    Import.NoChange t -> pure t.id

  -- Apply solutions
  mapM_ (applySolutionAction r taskId mTeacherId) preview.solutionActions

  pure taskId

-- | Apply a single solution import action
applySolutionAction :: SyncContext -> Id Task -> Maybe (Id User) -> ImportAction Solution -> IO ()
applySolutionAction r taskId mTeacherId action = case action of
  Import.Create s -> case mTeacherId of
    Just teacherId -> do
      newId <- nextId r
      let newSolution =
            Solution
              { id = newId
              , taskId = taskId
              , userId = teacherId
              , solutionType = s.solutionType
              , content = s.content
              }
      modifySyncDocument r (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Create newSolution)
    Nothing -> pure ()
  Import.Update _ _ -> pure ()
  Import.NoChange _ -> pure ()
