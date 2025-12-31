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
import Data.List (sortOn)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))

data Model = Model
  { allAssignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , selectedAssignment :: !(Maybe Assignment)
  , newAssignment :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectAssignment !Assignment
  | CreateNewAssignment
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

assignmentSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe Assignment) -> M.Component p Model Action
assignmentSelectorComponent r parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedAssignment]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing

    update (SelectAssignment a) =
      M.modify $ \m -> case Ix.getOne (m.allAssignments Ix.@= a.id) of
        Just a' -> m & (#selectedAssignment ?~ a') & (#newAssignment .~ Nothing)
        Nothing -> m & (#newAssignment ?~ a)

    update CreateNewAssignment = M.withSink $ \s -> do
      assignmentId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let newAssignment = mkAssignment assignmentId (AssignmentName "") today
      -- Emit command to create the assignment
      modifySyncDocument r $ Assignments (OnAssignments (Create newAssignment))
      -- Select the newly created assignment
      s (SelectAssignment newAssignment)

    update (UpdateDocument dc) = M.modify $ \m ->
      m & #allAssignments .~ dc.document.assignments

    view m =
      M.div_
        []
        [ M.h2_ [] [M.text $ C.translate' C.LblAssignments]
        , M.button_
            [M.onClick CreateNewAssignment]
            [M.text $ C.translate' C.LblNewAssignment]
        , M.div_
            []
            (map viewAssignmentItem (sortedAssignments m))
        ]

    sortedAssignments m = sortOn (\a -> a.assignmentDate) $ Ix.toList m.allAssignments

    viewAssignmentItem assignment =
      M.div_
        [M.onClick (SelectAssignment assignment)]
        [M.text $ assignmentNameToMiso assignment.name]

    assignmentNameToMiso :: AssignmentName -> MisoString
    assignmentNameToMiso (AssignmentName t) = ms t
