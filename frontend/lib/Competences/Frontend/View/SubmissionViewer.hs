module Competences.Frontend.View.SubmissionViewer
  ( viewSubmissionsPanel
  , viewSubmissionCard
  , isSubmissionOpen
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Evidence (Evidence)
import Competences.Document.FileRef (FileRef (..))
import Competences.Document.Submission (Submission (..), SubmissionId)
import Competences.Document.User (User (..), UserIxs)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- | Determine if a submission is "open" (not yet reviewed).
-- TODO: Implement proper logic (check evidence dates or explicit references).
isSubmissionOpen :: [Evidence] -> Submission -> Bool
isSubmissionOpen _evidences _submission = True

-- | Render the full right-side submissions panel.
viewSubmissionsPanel
  :: (SubmissionId -> a)
  -- ^ Select action constructor
  -> Maybe SubmissionId
  -- ^ Currently selected
  -> Ix.IxSet UserIxs User
  -- ^ User lookup
  -> [Submission]
  -- ^ Submissions to display
  -> M.View m a
viewSubmissionsPanel selectAction selectedSid users submissions =
  Layout.vFlow
    Layout.gapS
    [ Typography.h4 $ C.translate' C.LblSubmissions
    , Layout.vFlow
        Layout.gapS
        (map (viewSubmissionCard selectAction selectedSid users) submissions)
    ]

-- | Render a single submission card.
viewSubmissionCard
  :: (SubmissionId -> a)
  -> Maybe SubmissionId
  -> Ix.IxSet UserIxs User
  -> Submission
  -> M.View m a
viewSubmissionCard selectAction selectedSid users submission =
  let isSelected = selectedSid == Just submission.id
      studentName = case Ix.getOne (users Ix.@= submission.userId) of
        Just u -> u.name
        Nothing -> T.pack (show submission.userId)
      borderClass =
        if isSelected
          then "border-2 border-sky-500 bg-sky-50"
          else "border border-border hover:border-sky-300 cursor-pointer"
   in MH.div_
        [ class_ $ "rounded-lg p-3 " <> borderClass
        , MH.onClick (selectAction submission.id)
        ]
        [ -- Student name
          MH.div_ [class_ "font-medium text-sm"] [M.text $ ms studentName]
        , -- Timestamp
          Typography.small $ ms $ show submission.submittedAt
        , -- Files
          Layout.vFlow Layout.gapMicro (map viewFileRef submission.files)
        , -- Description
          case submission.description of
            Nothing -> M.text ""
            Just desc ->
              MH.div_
                [class_ "text-sm text-muted-foreground mt-1"]
                [M.text $ ms desc]
        ]

-- | Render a single file reference line.
viewFileRef :: FileRef -> M.View m a
viewFileRef ref =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ MH.span_ [class_ "text-xs font-medium truncate"] [M.text $ ms ref.fileName]
    , MH.span_
        [class_ "text-xs text-muted-foreground"]
        [M.text $ ms $ "(" <> showFileSize ref.fileSize <> ")"]
    ]
