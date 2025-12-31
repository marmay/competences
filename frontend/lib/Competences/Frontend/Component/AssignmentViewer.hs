module Competences.Frontend.Component.AssignmentViewer
  ( assignmentViewerComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , AssignmentIxs
  , Document (..)
  , User (..)
  , emptyDocument
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , subscribeDocument
  )
import Data.List (sortOn)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Miso.String (ms)

data Model = Model
  { allAssignments :: !(Ix.IxSet AssignmentIxs Assignment)
  , currentUser :: !User
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

assignmentViewerComponent :: SyncDocumentRef -> User -> M.Component p Model Action
assignmentViewerComponent r user =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty user emptyDocument

    update (UpdateDocument dc) = M.modify $ \m ->
      m
        { allAssignments = dc.document.assignments
        , document = dc.document
        }

    view m =
      M.div_
        []
        [ M.h2_ [] [M.text $ C.translate' C.LblAssignments]
        , if null myAssignments
            then M.p_ [] [M.text "Keine Aufträge"]
            else M.div_ [] (map (viewAssignment m.document) myAssignments)
        ]
      where
        myAssignments = sortOn (.assignmentDate) $ filter isMyAssignment $ Ix.toList m.allAssignments
        isMyAssignment a = m.currentUser.id `elem` a.studentIds

    viewAssignment doc assignment =
      M.div_
        [M.class_ "border rounded p-4 mb-4"]
        [ M.h3_ [M.class_ "font-bold text-lg mb-2"] [M.text $ assignmentNameToText assignment.name]
        , M.p_ [M.class_ "text-sm text-gray-600 mb-2"]
            [ M.text "Datum: "
            , M.text $ C.formatDay assignment.assignmentDate
            ]
        , M.p_ [M.class_ "text-sm text-gray-600 mb-2"]
            [ M.text "Art: "
            , M.text $ C.translate' $ C.LblActivityTypeDescription assignment.activityType
            ]
        , M.div_ [M.class_ "mt-3"]
            [ M.h4_ [M.class_ "font-semibold mb-1"] [M.text "Aufgaben:"]
            , if null assignment.tasks
                then M.p_ [M.class_ "text-sm text-gray-500"] [M.text "Keine Aufgaben"]
                else M.ul_ [M.class_ "list-disc list-inside"] (map (viewTask doc) assignment.tasks)
            ]
        ]

    viewTask doc taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId doc.tasks)
       in case taskM of
            Nothing -> M.li_ [] [M.text $ "Aufgabe " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
               in M.li_ [M.class_ "text-sm"] [M.text $ ms identifier]

    assignmentNameToText (AssignmentName t) = ms t
