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
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , subscribeDocument
  )
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
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

assignmentViewerComponent :: SyncContext -> User -> M.Component p Model Action
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
        [ Typography.h2 (C.translate' C.LblAssignments)
        , if null myAssignments
            then Typography.paragraph "Keine Aufträge"
            else M.div_ [class_ "space-y-4 mt-4"] (map (viewAssignment m.document) myAssignments)
        ]
      where
        myAssignments = sortOn (.assignmentDate) $ filter isMyAssignment $ Ix.toList m.allAssignments
        isMyAssignment a = m.currentUser.id `elem` a.studentIds

    viewAssignment doc assignment =
      Card.card
        [ M.div_ []
            [ Typography.h3 (assignmentNameToText assignment.name)
            , M.p_ [class_ "text-sm text-stone-600 mt-2"]
                [ Typography.small "Datum: "
                , M.text $ C.formatDay assignment.assignmentDate
                ]
            , M.p_ [class_ "text-sm text-stone-600"]
                [ Typography.small "Art: "
                , M.text $ C.translate' $ C.LblActivityTypeDescription assignment.activityType
                ]
            , M.div_ [class_ "mt-4"]
                [ Typography.h4 "Aufgaben:"
                , if null assignment.tasks
                    then Typography.muted "Keine Aufgaben"
                    else M.ul_ [class_ "list-disc list-inside mt-2"] (map (viewTask doc) assignment.tasks)
                ]
            ]
        ]

    viewTask doc taskId =
      let taskM = Ix.getOne (Ix.getEQ taskId doc.tasks)
       in case taskM of
            Nothing -> M.li_ [class_ "text-sm"] [M.text $ "Aufgabe " <> ms (show taskId)]
            Just task ->
              let TaskIdentifier identifier = task.identifier
               in M.li_ [class_ "text-sm"] [M.text $ ms identifier]

    assignmentNameToText (AssignmentName t) = ms t
