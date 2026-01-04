module Competences.Frontend.Component.Selector.AssignmentSelector
  ( assignmentSelectorComponent
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..))
import Competences.Document.Assignment (AssignmentName (..), mkAssignment)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
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
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectAssignment !Assignment
  | CreateNewAssignment
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

assignmentSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe Assignment) -> M.Component p Model Action
assignmentSelectorComponent r parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedAssignment]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing ""

    update (SelectAssignment a) =
      M.modify $ \m -> case Ix.getOne (m.allAssignments Ix.@= a.id) of
        Just a' -> m & (#selectedAssignment ?~ a') & (#newAssignment .~ Nothing)
        Nothing -> m & (#newAssignment ?~ a)

    update CreateNewAssignment = M.withSink $ \s -> do
      assignmentId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let newAssignment = mkAssignment assignmentId (AssignmentName "") today
      modifySyncDocument r $ Assignments (OnAssignments (Create newAssignment))
      s (SelectAssignment newAssignment)

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      m & #allAssignments .~ dc.document.assignments

    view' m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader (C.translate' C.LblAssignments) (Just CreateNewAssignment)
        , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterAssignments) (SetSearchQuery . M.fromMisoString)
        , SL.selectorList (map (viewAssignment m) (filteredAssignments m))
        ]

    filteredAssignments m =
      let query = T.toLower m.searchQuery
          sorted = sortOn (.assignmentDate) $ Ix.toList m.allAssignments
       in if T.null query
            then sorted
            else filter (\a -> query `T.isInfixOf` T.toLower (unAssignmentName a.name)) sorted

    unAssignmentName (AssignmentName t) = t

    viewAssignment m a =
      let isSelected = m.selectedAssignment == Just a || m.newAssignment == Just a
       in SL.selectorItem isSelected IcnAssignment (ms $ unAssignmentName a.name) (SelectAssignment a)
