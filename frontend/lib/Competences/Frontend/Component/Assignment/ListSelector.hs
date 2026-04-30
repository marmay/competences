-- | Assignment list selector — config builder over
-- 'listSelectorComponent', plus the assignment-specific filter
-- fragment and the standalone read-only viewer / searchable single-
-- select used by the Editor framework.
--
-- Selected type is 'WithOrigin Assignment' so drafts and published
-- assignments share one indexed collection. The projection is the
-- richer 'SelectorProjection' that pre-computes per-assignment
-- status maps; the role-aware filter dropdown reads those when
-- deciding which modes to render and apply.
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Competences.Frontend.Component.Assignment.ListSelector
  ( -- * Selector
    AssignmentWithOriginIxs
  , Selected
  , Projection
  , assignmentListSelectorComponent
    -- * Editor-framework integration (read-only viewer + searchable selector)
  , searchableSingleAssignmentEditorField
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), DraftAssignmentsCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), User (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..), mkAssignment)
import Competences.Document.User (UserId, isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.WithOrigin (WithOrigin (..))
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Editor.EditorField (EditorField, currentValue, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.Component.Selector.List
  ( Action (..)
  , CreateAction (..)
  , ListSelectorConfig (..)
  , ItemRenderer (..)
  , Model
  , listSelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.SelectorFilter (FilterFragment (..))
import Competences.Frontend.Fragment.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.Page (Page (..))
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
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (AssignmentStatus (..), assignmentStatus, hasOpenSubmissions, isAssignmentOpen)
import Data.Default (Default)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, fromMisoString, ms)
import Optics.Core (Lens', castOptic, (&), (.~), (^.), (%~))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | The selected entity in the assignment selector.
type Selected = WithOrigin Assignment

-- | Indices for the merged published+draft 'IxSet'.
type AssignmentWithOriginIxs = '[AssignmentId, UserId, Day, EntityOrigin]

instance Ix.Indexable AssignmentWithOriginIxs Selected where
  indices =
    Ix.ixList
      (Ix.ixFun $ \w -> [w.value.id])
      (Ix.ixFun $ \w -> Set.toList w.value.studentIds)
      (Ix.ixFun $ \w -> [w.value.assignmentDate])
      (Ix.ixFun $ \w -> [w.origin])

-- | Projection consumed by the selector. Pre-computes status maps so
-- the filter can render a role-aware enum without re-walking the
-- document each time.
data SelectorProjection = SelectorProjection
  { assignments :: !(Ix.IxSet AssignmentWithOriginIxs Selected)
  , focusedUser :: !(Maybe User)
  , statusMap :: !(Map AssignmentId AssignmentStatus)
  , openSet :: !(Set AssignmentId)
  , openSubmissionsSet :: !(Set AssignmentId)
  }
  deriving (Eq, Generic, Show)

type Projection = SelectorProjection

emptyProjection :: SelectorProjection
emptyProjection =
  SelectorProjection
    { assignments = Ix.empty
    , focusedUser = Nothing
    , statusMap = Map.empty
    , openSet = Set.empty
    , openSubmissionsSet = Set.empty
    }

-- | Pre-compute everything the selector / its filter could ever
-- need from one document + focused-user pair.
selectorProjection :: Document -> Maybe User -> SelectorProjection
selectorProjection doc mUser =
  let draftList = Ix.toList doc.draftAssignments
      realList = Ix.toList doc.assignments
      allAssignments :: Ix.IxSet AssignmentWithOriginIxs Selected
      allAssignments =
        Ix.fromList $
          map (WithOrigin Published) realList <> map (WithOrigin Draft) draftList
      filtered = case mUser of
        Nothing -> allAssignments
        Just u -> allAssignments Ix.@= u.id
      statusMap_ = case mUser of
        Nothing -> Map.empty
        Just u ->
          Map.fromList
            [ (w.value.id, assignmentStatus doc u.id w.value.id)
            | w <- Ix.toList filtered
            , w.origin == Published
            ]
      openSet_ = case mUser of
        Nothing -> Set.empty
        Just u ->
          Set.fromList
            [ w.value.id
            | w <- Ix.toList filtered
            , w.origin == Published
            , isAssignmentOpen doc u.id w.value.id
            ]
      openSubmissionsSet_ =
        Set.fromList
          [ w.value.id
          | w <- Ix.toList allAssignments
          , w.origin == Published
          , hasOpenSubmissions doc w.value.id
          ]
   in SelectorProjection
        { assignments = filtered
        , focusedUser = mUser
        , statusMap = statusMap_
        , openSet = openSet_
        , openSubmissionsSet = openSubmissionsSet_
        }

-- ---------------------------------------------------------------------------
-- Filter fragment
-- ---------------------------------------------------------------------------

data AssignmentFilterMode
  = AllAssignments
  | NotGradedOnly
  | OpenOnly
  | HasOpenSubmissions
  deriving (Eq, Show)

data AssignmentFilterState = AssignmentFilterState
  { search :: !Text
  , mode :: !AssignmentFilterMode
  }
  deriving (Eq, Show, Generic)

data AssignmentFilterAction
  = SetSearch !Text
  | SetMode !AssignmentFilterMode
  deriving (Eq, Show)

assignmentFilter
  :: SyncContext
  -> FilterFragment SelectorProjection AssignmentFilterState AssignmentFilterAction Selected
assignmentFilter r =
  FilterFragment
    { initialState =
        AssignmentFilterState
          { search = ""
          , mode = if teacher then AllAssignments else OpenOnly
          }
    , update = \act s -> case act of
        SetSearch q -> s & #search .~ q
        SetMode m -> s & #mode .~ m
    , view = \st proj ->
        M.div_
          [class_ "flex flex-col gap-2"]
          [ SL.selectorSearchField
              (ms st.search)
              (C.translate' C.LblFilterAssignments)
              (SetSearch . (fromMisoString :: MisoString -> Text))
          , modeDropdown st proj
          ]
    , apply = \st proj items ->
        let q = T.toLower st.search
            textFiltered =
              if T.null q
                then items
                else filter (\w -> q `T.isInfixOf` T.toLower (unName w.value.name)) items
            isNotCompleted w = case Map.lookup w.value.id proj.statusMap of
              Just Completed -> False
              _ -> True
         in case (proj.focusedUser, st.mode) of
              (_, HasOpenSubmissions) ->
                filter (\w -> Set.member w.value.id proj.openSubmissionsSet) textFiltered
              (Just _, NotGradedOnly) ->
                filter isNotCompleted textFiltered
              (Just _, OpenOnly) ->
                filter (\w -> Set.member w.value.id proj.openSet) textFiltered
              _ -> textFiltered
    }
  where
    teacher = isTeacher (syncDocumentEnv r).connectedUser

    modeDropdown :: forall p. AssignmentFilterState -> SelectorProjection -> M.View p AssignmentFilterAction
    modeDropdown st proj =
      let modes =
            if teacher
              then
                [AllAssignments, HasOpenSubmissions]
                  <> [OpenOnly | isJust proj.focusedUser]
                  <> [NotGradedOnly | isJust proj.focusedUser]
              else [OpenOnly, AllAssignments, NotGradedOnly]
          labelMap :: Map MisoString AssignmentFilterMode
          labelMap = Map.fromList [(modeLabel m, m) | m <- modes]
          lookupByLabel v = Map.findWithDefault st.mode v labelMap
       in M.select_
            [ class_ "w-full h-8 rounded-md border border-input bg-background px-2 text-sm"
            , M.onChange (SetMode . lookupByLabel . fromMisoString)
            ]
            (map (renderOption st.mode) modes)

    renderOption :: forall p. AssignmentFilterMode -> AssignmentFilterMode -> M.View p AssignmentFilterAction
    renderOption sel m =
      M.option_
        ( [M.textProp "value" (modeLabel m)]
            <> [M.boolProp "selected" True | m == sel]
        )
        [M.text (modeLabel m)]

    modeLabel = \case
      AllAssignments -> C.translate' C.LblFilterAllAssignments
      NotGradedOnly -> C.translate' C.LblFilterNotGraded
      OpenOnly -> C.translate' C.LblFilterOpenAssignments
      HasOpenSubmissions -> C.translate' C.LblFilterHasOpenSubmissions

-- ---------------------------------------------------------------------------
-- Selector
-- ---------------------------------------------------------------------------

assignmentListSelectorComponent
  :: SyncContext
  -> Maybe (Ix.IxSet AssignmentWithOriginIxs Selected -> Maybe Selected)
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection AssignmentFilterState) (Action Selected Projection AssignmentFilterAction)
assignmentListSelectorComponent r initialPickFn parentLens =
  listSelectorComponent r (config r initialPickFn parentLens)

config
  :: SyncContext
  -> Maybe (Ix.IxSet AssignmentWithOriginIxs Selected -> Maybe Selected)
  -> Lens' p (Maybe Selected)
  -> ListSelectorConfig p Selected Projection AssignmentWithOriginIxs AssignmentId AssignmentFilterState AssignmentFilterAction
config r initialPickFn parentLens =
  ListSelectorConfig
    { title = C.translate' C.LblAssignments
    , project = selectorProjection
    , emptyProjection = emptyProjection
    , entitiesOf = (.assignments)
    , itemsInOrder = sortOn (.value.assignmentDate) . Ix.toList
    , idOf = (.value.id)
    , lookupBy = \xs aid -> Ix.getOne (xs Ix.@= aid)
    , itemView = ItemRenderer renderItem
    , createActions =
        if teacher
          then
            [ CreateAction
                { icon = Icon.IcnAdd
                , label = C.translate' C.LblCreate
                , run = \r' -> do
                    aid <- nextId r'
                    let today = (syncDocumentEnv r').currentDay
                        a = mkAssignment aid (AssignmentName "") today
                    modifySyncDocument r' $ Assignments (OnAssignments (CreateAndLock a))
                    pure (Just (WithOrigin Published a))
                }
            , CreateAction
                { icon = Icon.IcnAdd
                , label = C.translate' C.LblNewDraftAssignment
                , run = \r' -> do
                    aid <- nextId r'
                    let today = (syncDocumentEnv r').currentDay
                        a = mkAssignment aid (AssignmentName "") today
                    modifySyncDocument r' $ DraftAssignments (OnDraftAssignments (CreateAndLock a))
                    pure (Just (WithOrigin Draft a))
                }
            ]
          else []
    , uriBinding =
        Just $ pageBinding (ManageAssignments . Just) $ \case
          ManageAssignments (Just aid) -> Just aid
          _ -> Nothing
    , initialPick = initialPickFn
    , filter = assignmentFilter r
    , parentLens = parentLens
    }
  where
    teacher = isTeacher (syncDocumentEnv r).connectedUser

renderItem
  :: Selected
  -> Bool
  -> M.View m (Action Selected Projection AssignmentFilterAction)
renderItem w isSel =
  let isDraft = w.origin == Draft
      iconView =
        Icon.icon
          [class_ "w-4 h-4 text-muted-foreground shrink-0"]
          (EvidenceIcon.activityTypeIcon w.value.activityType)
   in SL.selectorItemMultiLine
        isSel
        [ M.div_
            [class_ "flex items-center gap-2"]
            ( [ iconView
              , M.span_ [class_ "text-sm truncate font-medium"] [M.text $ ms $ unName w.value.name]
              ]
                <> [Badge.secondary (Badge.badgeText (C.translate' C.LblDraft)) | isDraft]
            )
        , M.div_
            [class_ "flex items-center gap-2 text-xs text-muted-foreground"]
            [M.span_ [] [M.text (C.formatDay w.value.assignmentDate)]]
        ]
        (Pick w)

unName :: AssignmentName -> Text
unName (AssignmentName t) = t

-- ===========================================================================
-- Editor-framework integration: read-only viewer + searchable selector
-- (used by Evidence editor etc.)
-- ===========================================================================
--
-- Separate machinery from the list selector above: a read-only
-- viewer plus a searchable combobox-style single-select used in
-- editor fields. Different projection (just the list of
-- assignments) and different binding shape ('SelectorTransformedLens').

-- | Searchable single-assignment editor field for use in editors.
searchableSingleAssignmentEditorField
  :: (Eq t, Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens p patch Maybe Assignment Maybe t
  -> EditorField p patch f
searchableSingleAssignmentEditorField r k eptl =
  let conf currentAssignmentId =
        AssignmentEditorConfig
          { isInitialAssignment = \a -> currentAssignmentId == Just (eptl.transform a)
          }
   in selectorEditorFieldWithViewer
        k
        eptl
        (\e -> selectedAssignmentViewerComponent r (conf (e ^. eptl.viewLens)))
        ( \e p ->
            searchableSingleAssignmentSelectorComponent
              r
              (conf (currentValue e p eptl.viewLens eptl.patchLens))
        )

newtype AssignmentEditorConfig = AssignmentEditorConfig
  { isInitialAssignment :: Assignment -> Bool
  }

data EditorFieldProjection = EditorFieldProjection
  { assignments :: ![Assignment]
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

editorFieldProjection :: Document -> Maybe User -> EditorFieldProjection
editorFieldProjection doc mUser =
  let filtered = case mUser of
        Nothing -> Ix.toList doc.assignments
        Just u -> Ix.toList (doc.assignments Ix.@= u.id)
   in EditorFieldProjection
        { assignments = sortOn (.assignmentDate) filtered
        , focusedUser = mUser
        }

data SelectedAssignmentViewerModel = SelectedAssignmentViewerModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

newtype SelectedAssignmentViewerAction
  = AssignmentViewerProjectionChanged (ProjectedChange EditorFieldProjection)
  deriving (Eq, Show)

selectedAssignmentViewerComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p SelectedAssignmentViewerModel SelectedAssignmentViewerAction
selectedAssignmentViewerComponent r conf lensBinding =
  (M.component model0 update0 view0)
    { M.bindings = [mkSelectorBinding lensBinding (castOptic #selectedValue)]
    , M.subs = [subscribeWithProjection r editorFieldProjection AssignmentViewerProjectionChanged]
    }
  where
    model0 = SelectedAssignmentViewerModel{possibleValues = [], selectedValue = Nothing}

    update0 (AssignmentViewerProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue
              | change.changeInfo == InitialSnapshot =
                  case filter conf.isInitialAssignment newPossibleValues of
                    (a : _) -> Just a
                    [] -> Nothing
              | otherwise =
                  m.selectedValue >>= \sel -> find (\a -> a.id == sel.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)

    view0 m = viewSelectedAssignment m.selectedValue

viewSelectedAssignment :: Maybe Assignment -> M.View m a
viewSelectedAssignment = \case
  Nothing -> Typography.muted (C.translate' C.LblNoAssignmentSelected)
  Just a ->
    M.span_
      []
      [M.text $ ms $ unName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"]

data AssignmentSelectorModel = AssignmentSelectorModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data AssignmentSelectorAction
  = SelectorProjectionChanged !(ProjectedChange EditorFieldProjection)
  | SelectorToggle !Assignment
  | SelectorSetSearchQuery !Text
  | SelectorSetOpen !Bool
  deriving (Eq, Show)

searchableSingleAssignmentSelectorComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p AssignmentSelectorModel AssignmentSelectorAction
searchableSingleAssignmentSelectorComponent r conf lensBinding =
  (M.component model0 update0 view0)
    { M.bindings = [mkSelectorBinding lensBinding #selectedValue]
    , M.subs = [subscribeWithProjection r editorFieldProjection SelectorProjectionChanged]
    }
  where
    model0 =
      AssignmentSelectorModel
        { possibleValues = []
        , selectedValue = Nothing
        , searchQuery = ""
        , isOpen = False
        }

    update0 (SelectorProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue
              | change.changeInfo == InitialSnapshot = find conf.isInitialAssignment newPossibleValues
              | otherwise = m.selectedValue >>= \v -> find (\a -> a.id == v.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)
    update0 (SelectorToggle a) =
      M.modify $ \m ->
        m
          & (#selectedValue %~ \s -> if (fmap (.id) s) == Just a.id then Nothing else Just a)
          & (#isOpen .~ False)
    update0 (SelectorSetSearchQuery q) =
      M.modify (#searchQuery .~ q)
    update0 (SelectorSetOpen open) =
      M.modify (#isOpen .~ open)

    view0 m =
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

    showAssignment a = ms $ unName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"
