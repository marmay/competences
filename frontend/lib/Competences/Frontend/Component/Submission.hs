-- | Student submission component for assignments.
--
-- Renders inline inside the assignment viewer (students only).
-- Allows uploading files with an optional description to create submissions.
module Competences.Frontend.Component.Submission
  ( submissionComponent
  )
where

import Competences.Command (Command (..))
import Competences.Command.Submissions (SubmissionsCommand (..))
import Competences.Command.Common (EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Submission (..)
  , User (..)
  )
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.FileRef (FileRef (..))
import Competences.Document.Submission (SubmissionId)
import Competences.Document.User (UserId)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.FileUpload (fileUploadComponent, showFileSize)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.SyncContext.WindowManager (WindowMode, inlineComponent)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, fromMisoString, ms)
import Optics.Core ((&), (.~))

-- | Projection: existing submissions for this assignment + user
data SubmissionProjection = SubmissionProjection
  { submissions :: ![Submission]
  }
  deriving (Eq, Generic, Show)

-- | Component model
data SubmissionModel = SubmissionModel
  { projection :: !SubmissionProjection
  , files :: ![FileRef]
  , description :: !MisoString
  , confirmDelete :: !(Maybe SubmissionId)
  }
  deriving (Eq, Generic, Show)

data SubmissionAction
  = ProjectionChanged !(ProjectedChange SubmissionProjection)
  | SetDescription !MisoString
  | SubmitWork
  | DoSubmit !UTCTime
  | RequestDelete !SubmissionId
  | ConfirmDelete !SubmissionId
  | CancelDelete
  deriving (Eq, Show)

-- | Create a submission component for a specific assignment and user.
submissionComponent
  :: SyncContext
  -> AssignmentId
  -> UserId
  -> WindowMode
  -> M.Component p SubmissionModel SubmissionAction
submissionComponent r assignmentId userId _wm =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (submissionProjection assignmentId userId) ProjectionChanged]
    }
  where
    model = SubmissionModel
      { projection = SubmissionProjection []
      , files = []
      , description = ""
      , confirmDelete = Nothing
      }

    submissionProjection :: AssignmentId -> UserId -> Document -> Maybe User -> SubmissionProjection
    submissionProjection aid uid doc _mUser =
      SubmissionProjection
        { submissions = sortOn (Down . (.submittedAt)) $ Ix.toList $ doc.submissions Ix.@= aid Ix.@= uid
        }

    update (ProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    update (SetDescription t) =
      M.modify $ \m -> m & #description .~ t

    update SubmitWork =
      M.io $ DoSubmit <$> getCurrentTime

    update (DoSubmit now) = do
      m <- M.get
      if null m.files
        then pure ()
        else do
          M.io_ $ do
            sid <- nextId r
            let descText = T.pack (fromMisoString m.description)
                desc = if T.null descText then Nothing else Just descText
                submission = Submission
                  { id = sid
                  , assignmentId = assignmentId
                  , userId = userId
                  , files = m.files
                  , description = desc
                  , submittedAt = now
                  }
            modifySyncDocument r $ Submissions (OnSubmissions (Create submission))
          M.modify $ \m' -> m' & #files .~ [] & #description .~ ""

    update (RequestDelete sid) =
      M.modify $ \m -> m & #confirmDelete .~ Just sid

    update CancelDelete =
      M.modify $ \m -> m & #confirmDelete .~ Nothing

    update (ConfirmDelete sid) = do
      M.io_ $ modifySyncDocument r $ Submissions (OnSubmissions (Delete sid))
      M.modify $ \m -> m & #confirmDelete .~ Nothing

    view' m =
      MH.div_
        [class_ "space-y-4"]
        [ -- Existing submissions
          if null m.projection.submissions
            then Typography.small $ C.translate' C.LblNoSubmissions
            else MH.div_ [class_ "space-y-2"] (map (viewSubmission m.confirmDelete) m.projection.submissions)
        , -- New submission form
          Card.card
            [ Typography.h4 $ C.translate' C.LblSubmitWork
            , MH.div_
                [class_ "space-y-3"]
                [ -- File upload
                  inlineComponent "submission-file-upload"
                    (fileUploadComponent r m.files #files)
                , -- Description textarea
                  Input.textarea m.description SetDescription
                , -- Submit button
                  if null m.files
                    then Button.primary (Button.button (C.translate' C.LblSubmitWork) Button.Disabled)
                    else Button.primary (Button.button (C.translate' C.LblSubmitWork) SubmitWork)
                ]
            ]
        ]

    viewSubmission confirmingDelete s =
      Card.card
        [ MH.div_
            [class_ "flex items-start justify-between gap-4"]
            [ MH.div_
                [class_ "flex-1 min-w-0 space-y-1"]
                [ -- Timestamp
                  Typography.small $ ms $ show s.submittedAt
                , -- Files list
                  MH.div_ [class_ "space-y-1"] (map viewFileRef s.files)
                , -- Description
                  case s.description of
                    Nothing -> M.text ""
                    Just desc -> MH.div_ [class_ "text-sm text-muted-foreground mt-1"] [M.text $ ms desc]
                ]
            , -- Delete button or confirmation
              case confirmingDelete of
                Just sid | sid == s.id ->
                  MH.div_
                    [class_ "flex gap-2"]
                    [ Button.destructiveSm $ Button.button C.LblDelete (ConfirmDelete s.id)
                    , Button.secondarySm $ Button.button C.LblCancel CancelDelete
                    ]
                _ ->
                  Button.ghostSm $ Button.button C.LblDeleteSubmission (RequestDelete s.id)
            ]
        ]

    viewFileRef ref =
      MH.div_
        [class_ "flex items-center gap-2 text-sm"]
        [ MH.span_ [class_ "font-medium truncate"] [M.text $ ms ref.fileName]
        , MH.span_ [class_ "text-muted-foreground"]
            [M.text $ ms $ "(" <> showFileSize ref.fileSize <> ")"]
        ]
