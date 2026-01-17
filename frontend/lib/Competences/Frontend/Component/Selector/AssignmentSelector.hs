module Competences.Frontend.Component.Selector.AssignmentSelector
  ( assignmentSelectorComponent
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..), User (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..), mkAssignment)
import Competences.Frontend.Common qualified as C
import Competences.Query.Assignment (AssignmentStatus (..), assignmentStatus)
import Competences.Query.Assignment qualified as Q
import Data.Map.Strict qualified as Map
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncDocumentEnv (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Miso.Html qualified as M
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))

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
selectorProjection :: Document -> Maybe User -> SelectorProjection
selectorProjection doc mUser =
  let assignments = doc.assignments
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
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectAssignment !Assignment
  | CreateNewAssignment
  | SetSearchQuery !Text
  | SetShowIncompleteOnly !Bool
  | ProjectionChanged !(ProjectedChange SelectorProjection)
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
      , showIncompleteOnly = False
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
      s (SelectAssignment newAssignment)

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (SetShowIncompleteOnly b) = M.modify $ \m ->
      m & #showIncompleteOnly .~ b

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    view' m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader (C.translate' C.LblAssignments) (Just CreateNewAssignment)
        , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterAssignments) (SetSearchQuery . M.fromMisoString)
        , viewStatusFilters m
        , SL.selectorList (map (viewAssignment m) (filteredAssignments m))
        ]

    viewStatusFilters m =
      case m.projection.focusedUser of
        Nothing -> M.text "" -- No filters when no user is focused
        Just _ ->
          M.div_
            [class_ "flex gap-1"]
            [ filterButton m False "Alle"
            , filterButton m True "Nicht erledigt"
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
          -- Check if assignment is incomplete using pre-computed status
          isIncomplete a = case Map.lookup a.id proj.statusMap of
            Just Completed -> False
            _ -> True  -- NotGraded or NeedsWork count as incomplete
       in case (proj.focusedUser, m.showIncompleteOnly) of
            (Just _, True) -> filter isIncomplete textFiltered
            _ -> textFiltered

    unAssignmentName (AssignmentName t) = t

    viewAssignment m a =
      let proj = m.projection
          isSelected = m.selectedAssignment == Just a || m.newAssignment == Just a
          mStatus = do
            _ <- proj.focusedUser  -- Only show status if user is focused
            Map.lookup a.id proj.statusMap
       in SL.selectorItemMultiLine isSelected
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
                    Just status -> statusBadge status
                    Nothing -> M.text ""
                ]
            ]
            (SelectAssignment a)

    statusBadge :: AssignmentStatus -> M.View Model Action
    statusBadge status =
      Badge.badge (statusBadgeVariant status) (ms $ Q.statusLabel status)

    statusBadgeVariant NotGraded = Badge.BadgeSecondary
    statusBadgeVariant NeedsWork = Badge.BadgeOutline
    statusBadgeVariant Completed = Badge.BadgePrimary
