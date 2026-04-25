module Competences.Frontend.Component.Selector.AssignmentSelector
  ( assignmentSelectorComponent
  , searchableSingleAssignmentEditorField
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), DraftAssignmentsCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..), User (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..), mkAssignment)
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, currentValue, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.Component.Selector.EnumSelector qualified as ES
import Competences.Frontend.Component.ImportModal qualified as ImportModal
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.Fragment.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Color.Completion (CompletionStatus (..))
import Competences.Frontend.View.StatusIcon (completionIcon)
import Competences.Frontend.View.SelectorList qualified as SelectorList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (AssignmentStatus (..), assignmentStatus, hasOpenSubmissions, isAssignmentOpen)
import Data.Default (Default)
import Data.List (find, sortOn)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (fromMisoString, ms)
import Optics.Core (Lens', castOptic, toLensVL, (&), (.~), (%~), (?~), (^.))

-- | Projection from document + focused user
data SelectorProjection = SelectorProjection
  { assignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , focusedUser :: !(Maybe User)
    -- | Pre-computed status for each assignment (only when focusedUser is set)
  , statusMap :: !(Map.Map AssignmentId AssignmentStatus)
    -- | IDs of draft assignments (for showing Draft badge)
  , draftIds :: !(Set.Set AssignmentId)
    -- | IDs of "open" assignments (student still needs to act)
  , openSet :: !(Set.Set AssignmentId)
    -- | IDs of assignments with any unreviewed submissions (teacher-level)
  , openSubmissionsSet :: !(Set.Set AssignmentId)
  }
  deriving (Eq, Generic, Show)

emptyProjection :: SelectorProjection
emptyProjection = SelectorProjection Ix.empty Nothing Map.empty Set.empty Set.empty Set.empty

-- | Projection function - pre-computes all assignment statuses
-- Filters assignments by focused user if set (shows only assignments assigned to that user)
-- Merges real + draft assignments (drafts are empty for students via projection)
selectorProjection :: Document -> Maybe User -> SelectorProjection
selectorProjection doc mUser =
  let -- Merge real + draft assignments
      draftList = Ix.toList doc.draftAssignments
      draftIds' = Set.fromList $ map (.id) draftList
      allAssignments = Ix.fromList $ Ix.toList doc.assignments <> draftList
      -- Filter assignments by focused user if set
      assignments = case mUser of
        Nothing -> allAssignments
        Just user -> allAssignments Ix.@= user.id
      statusMap = case mUser of
        Nothing -> Map.empty
        Just user -> Map.fromList
          [ (a.id, assignmentStatus doc user.id a.id)
          | a <- Ix.toList assignments
          , not (Set.member a.id draftIds')  -- Only real assignments have status
          ]
      openSet' = case mUser of
        Nothing -> Set.empty
        Just user -> Set.fromList
          [ a.id
          | a <- Ix.toList assignments
          , not (Set.member a.id draftIds')
          , isAssignmentOpen doc user.id a.id
          ]
      openSubmissionsSet' = Set.fromList
          [ a.id
          | a <- Ix.toList allAssignments
          , not (Set.member a.id draftIds')
          , hasOpenSubmissions doc a.id
          ]
   in SelectorProjection
        { assignments
        , focusedUser = mUser
        , statusMap
        , draftIds = draftIds'
        , openSet = openSet'
        , openSubmissionsSet = openSubmissionsSet'
        }

data AssignmentFilter = AllAssignments | NotGradedOnly | OpenOnly | HasOpenSubmissions
  deriving (Eq, Show)

data Model = Model
  { projection :: !SelectorProjection
  , selectedAssignment :: !(Maybe Assignment)  -- bound to parent
  , newAssignment :: !(Maybe Assignment)       -- temporary for new assignments
  , searchQuery :: !Text
  , assignmentFilter :: !AssignmentFilter
  , isDropdownOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectAssignment !Assignment
  | CreateNewAssignment
  | CreateNewDraftAssignment
  | SetSearchQuery !Text
  | ProjectionChanged !(ProjectedChange SelectorProjection)
  | ToggleDropdown
  | OpenImportModal
  deriving (Eq, Show)

assignmentSelectorComponent
  :: SyncContext
  -> Maybe (Ix.IxSet AssignmentIxs Assignment -> Maybe Assignment)
  -> Lens' p (Maybe Assignment)
  -> M.Component p Model Action
assignmentSelectorComponent r initialSelection parentLens =
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
      , assignmentFilter = defaultFilter
      , isDropdownOpen = False
      }

    update (SelectAssignment a) =
      M.modify $ \m -> case Ix.getOne (m.projection.assignments Ix.@= a.id) of
        Just a' -> m & (#selectedAssignment ?~ a') & (#newAssignment .~ Nothing)
        Nothing -> m & (#newAssignment ?~ a)

    update CreateNewAssignment = M.withSink $ \s -> do
      assignmentId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let newAssignment = mkAssignment assignmentId (AssignmentName "") today
      modifySyncDocument r $ Assignments (OnAssignments (CreateAndLock newAssignment))
      s ToggleDropdown
      s (SelectAssignment newAssignment)

    update CreateNewDraftAssignment = M.withSink $ \s -> do
      assignmentId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let newAssignment = mkAssignment assignmentId (AssignmentName "") today
      modifySyncDocument r $ DraftAssignments (OnDraftAssignments (CreateAndLock newAssignment))
      s ToggleDropdown
      s (SelectAssignment newAssignment)

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (ProjectionChanged change) =
      M.modify $ \m ->
        let m' = updateFromProjection change.projection m
         in case (change.changeInfo, m'.selectedAssignment, initialSelection) of
              (InitialSnapshot, Nothing, Just f) ->
                m' & #selectedAssignment .~ f change.projection.assignments
              _ -> m'

    update ToggleDropdown = M.modify $ \m -> m & #isDropdownOpen .~ not m.isDropdownOpen

    update OpenImportModal = do
      M.modify $ #isDropdownOpen .~ False
      M.io_ $ ImportModal.openImportModal r

    updateFromProjection :: SelectorProjection -> Model -> Model
    updateFromProjection proj m =
      let assignments = proj.assignments
          validateAssignment a = do
            a' <- a
            Ix.getOne $ assignments Ix.@= a'.id
          (selected', new') = case (validateAssignment m.selectedAssignment, validateAssignment m.newAssignment) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            & #projection .~ proj
            & #selectedAssignment .~ selected'
            & #newAssignment .~ new'

    connectedUser = (syncDocumentEnv r).connectedUser
    defaultFilter = if isTeacher connectedUser then AllAssignments else OpenOnly

    view' m =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull) $
            [ if isTeacher connectedUser
                then
                  SelectorList.selectorHeaderWithDropdown
                    (C.translate' C.LblAssignments)
                    m.isDropdownOpen
                    ToggleDropdown
                    [ SelectorList.dropdownItem Icon.IcnAdd (C.translate' C.LblCreate) CreateNewAssignment
                    , SelectorList.dropdownItem Icon.IcnAdd (C.translate' C.LblNewDraftAssignment) CreateNewDraftAssignment
                    , SelectorList.dropdownItem Icon.IcnImport (C.translate' C.LblImportAssignments) OpenImportModal
                    ]
                else
                  SelectorList.selectorHeader (C.translate' C.LblAssignments) Nothing
            , SelectorList.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterAssignments) (SetSearchQuery . M.fromMisoString)
            ]
            <> [ inlineComponent
                   "assignment-status-filter"
                   ( ES.enumSelectorComponent'
                       defaultFilter
                       ( if isTeacher connectedUser
                           then
                             [AllAssignments, HasOpenSubmissions]
                               <> [OpenOnly | isJust m.projection.focusedUser]
                               <> [NotGradedOnly | isJust m.projection.focusedUser]
                           else
                             [OpenOnly, AllAssignments, NotGradedOnly]
                       )
                       ES.SelectDropdown
                       translateAssignmentFilter
                       #assignmentFilter
                   )
               ]
            <> [ SelectorList.selectorList (map (viewAssignment m) (filteredAssignments m))
               ]
        ]

    translateAssignmentFilter AllAssignments = C.translate' C.LblFilterAllAssignments
    translateAssignmentFilter NotGradedOnly = C.translate' C.LblFilterNotGraded
    translateAssignmentFilter OpenOnly = C.translate' C.LblFilterOpenAssignments
    translateAssignmentFilter HasOpenSubmissions = C.translate' C.LblFilterHasOpenSubmissions

    filteredAssignments m =
      let proj = m.projection
          query = T.toLower m.searchQuery
          sorted = sortOn (.assignmentDate) $ Ix.toList proj.assignments
          textFiltered =
            if T.null query
              then sorted
              else filter (\a -> query `T.isInfixOf` T.toLower (unAssignmentName a.name)) sorted
          -- Check if assignment is not completed (includes NotGraded, NeedsWork, and drafts)
          isNotCompleted a = case Map.lookup a.id proj.statusMap of
            Just Completed -> False
            _ -> True  -- NotGraded, NeedsWork, and drafts (not in statusMap)
       in case (proj.focusedUser, m.assignmentFilter) of
            (_, HasOpenSubmissions) -> filter (\a -> Set.member a.id proj.openSubmissionsSet) textFiltered
            (Just _, NotGradedOnly) -> filter isNotCompleted textFiltered
            (Just _, OpenOnly) -> filter (\a -> Set.member a.id proj.openSet) textFiltered
            _ -> textFiltered

    unAssignmentName (AssignmentName t) = t

    viewAssignment m a =
      let proj = m.projection
          isSelected = m.selectedAssignment == Just a || m.newAssignment == Just a
          isDraft = Set.member a.id proj.draftIds
          mStatus = do
            _ <- proj.focusedUser  -- Only show status if user is focused
            Map.lookup a.id proj.statusMap
       in SelectorList.selectorItemMultiLine isSelected
            [ -- Line 1: Icon + Name + Draft badge
              M.div_
                [class_ "flex items-center gap-2"]
                ( [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] (EvidenceIcon.activityTypeIcon a.activityType)
                  , M.span_ [class_ "text-sm truncate font-medium"] [M.text $ ms $ unAssignmentName a.name]
                  ]
                  <> [ Badge.secondary (Badge.badgeText (C.translate' C.LblDraft)) | isDraft ]
                )
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
    statusIcon NeedsWork = completionIcon InProgress
    statusIcon Completed = completionIcon Done

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
  let config currentAssignmentId =
        AssignmentEditorConfig
          { isInitialAssignment = \a -> currentAssignmentId == Just (eptl.transform a)
          }
   in selectorEditorFieldWithViewer
        k
        eptl
        (\e -> selectedAssignmentViewerComponent r (config (e ^. eptl.viewLens)))
        (\e p -> searchableSingleAssignmentSelectorComponent r
                   (config (currentValue e p eptl.viewLens eptl.patchLens)))

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
