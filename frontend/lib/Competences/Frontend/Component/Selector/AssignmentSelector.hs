module Competences.Frontend.Component.Selector.AssignmentSelector
  ( assignmentSelectorComponent
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..), User (..), emptyDocument)
import Competences.Document.Assignment (AssignmentName (..), mkAssignment)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.ViewerDetail
  ( AssignmentStatus (..)
  , assignmentStatus
  , statusLabel
  )
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncDocumentEnv (..)
  , SyncContext
  , getFocusedUserRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , subscribeFocusedUser
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

data Model = Model
  { allAssignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , selectedAssignment :: !(Maybe Assignment)
  , newAssignment :: !(Maybe Assignment)
  , searchQuery :: !Text
  , document :: !Document
  , focusedUser :: !(Maybe User)
  , statusFilter :: !(Maybe AssignmentStatus)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectAssignment !Assignment
  | CreateNewAssignment
  | SetSearchQuery !Text
  | SetStatusFilter !(Maybe AssignmentStatus)
  | UpdateDocument !DocumentChange
  | FocusedUserChanged !FocusedUserChange
  deriving (Eq, Show)

assignmentSelectorComponent
  :: SyncContext -> Lens' p (Maybe Assignment) -> M.Component p Model Action
assignmentSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedAssignment]
    , M.subs =
        [ subscribeDocument r UpdateDocument
        , subscribeFocusedUser (getFocusedUserRef r) FocusedUserChanged
        ]
    }
  where
    model = Model Ix.empty Nothing Nothing "" emptyDocument Nothing Nothing

    update (SelectAssignment a) =
      M.modify $ \m -> case Ix.getOne (m.allAssignments Ix.@= a.id) of
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

    update (SetStatusFilter sf) = M.modify $ \m ->
      m & #statusFilter .~ sf

    update (UpdateDocument dc) = M.modify $ \m ->
      m & #allAssignments .~ dc.document.assignments
        & #document .~ dc.document

    update (FocusedUserChanged fc) = M.modify $ \m ->
      m & #focusedUser .~ fc.user

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
      case m.focusedUser of
        Nothing -> M.text "" -- No filters when no user is focused
        Just _ ->
          M.div_
            [class_ "flex flex-wrap gap-1"]
            [ filterButton m Nothing "Alle"
            , filterButton m (Just NotGraded) (statusLabel NotGraded)
            , filterButton m (Just NeedsWork) (statusLabel NeedsWork)
            , filterButton m (Just Completed) (statusLabel Completed)
            ]

    filterButton m filterValue label =
      let isActive = m.statusFilter == filterValue
          baseClass = "px-2 py-1 text-xs rounded-full cursor-pointer transition-colors "
          activeClass = if isActive then "bg-primary text-primary-foreground" else "bg-muted hover:bg-muted/80"
       in M.button_
            [ class_ (baseClass <> activeClass)
            , M.onClick (SetStatusFilter filterValue)
            ]
            [M.text label]

    filteredAssignments m =
      let query = T.toLower m.searchQuery
          sorted = sortOn (.assignmentDate) $ Ix.toList m.allAssignments
          textFiltered =
            if T.null query
              then sorted
              else filter (\a -> query `T.isInfixOf` T.toLower (unAssignmentName a.name)) sorted
       in case (m.focusedUser, m.statusFilter) of
            (Just user, Just sf) ->
              filter (\a -> assignmentStatus m.document user.id a.id == sf) textFiltered
            _ -> textFiltered

    unAssignmentName (AssignmentName t) = t

    viewAssignment m a =
      let isSelected = m.selectedAssignment == Just a || m.newAssignment == Just a
          mBadge = do
            user <- m.focusedUser
            let status = assignmentStatus m.document user.id a.id
            pure $ statusBadge status
       in SL.selectorItemWithBadge isSelected IcnAssignment (ms $ unAssignmentName a.name) mBadge (SelectAssignment a)

    statusBadge :: AssignmentStatus -> M.View Model Action
    statusBadge status =
      Badge.badge (statusBadgeVariant status) (statusLabel status)

    statusBadgeVariant NotGraded = Badge.BadgeSecondary
    statusBadgeVariant NeedsWork = Badge.BadgeOutline
    statusBadgeVariant Completed = Badge.BadgePrimary
