module Competences.Frontend.View.SubmissionViewer
  ( viewSubmissionsPanel
  , viewSubmissionCard
  , isSubmissionOpen
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Evidence (Evidence)
import Competences.Document.FileRef (FileRef (..))
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionKind (..), ownerIds)
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
      owners = ownerIds submission.ownership
      studentName = case owners of
        [uid] -> case Ix.getOne (users Ix.@= uid) of
          Just u -> u.name
          Nothing -> T.pack (show uid)
        _ -> T.intercalate ", " $
          map (\uid -> maybe (T.pack (show uid)) (.name) (Ix.getOne (users Ix.@= uid))) owners
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
        , -- Kind-specific details
          viewKindDetails submission.kind
        , -- Remark
          case submission.remark of
            Nothing -> M.text ""
            Just rmk ->
              MH.div_
                [class_ "text-sm text-muted-foreground mt-1"]
                [M.text $ ms rmk]
        ]

-- | Render kind-specific submission details.
viewKindDetails :: SubmissionKind -> M.View m a
viewKindDetails (DigitalSubmission files) =
  Layout.vFlow Layout.gapMicro (map viewFileRef files)
viewKindDetails (NonDigitalSubmission mLoc) =
  case mLoc of
    Nothing -> M.text ""
    Just loc -> MH.div_ [class_ "text-sm text-muted-foreground"] [M.text $ ms loc]
viewKindDetails (VoidSubmission reason) =
  MH.div_ [class_ "text-sm text-muted-foreground italic"] [M.text $ ms reason]

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
