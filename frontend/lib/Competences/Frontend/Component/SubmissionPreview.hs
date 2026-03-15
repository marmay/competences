-- | Self-contained Miso component for previewing submissions.
--
-- Architecture:
--   submissionPreviewPanel (mounts container)
--   └─ Container component (holds selectedId)
--      ├─ CustomSelect component (derives options, pushes selectedId via binding)
--      └─ Detail component (keyed by selectedId, delegates files to FileGallery)
--
-- When the user picks a different submission in the CustomSelect, the container's
-- selectedId changes via binding, which changes the detail component's key,
-- causing Miso to remount it → fresh file loading.
module Competences.Frontend.Component.SubmissionPreview
  ( submissionPreviewPanel
  , submissionSelectorComponent
  , openSubmissionPeekModal
  , SubmissionPreviewModel
  , SubmissionPreviewAction
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (AssignmentId, Document (..), User (..))
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionKind (..), SubmissionOwnership (..), ownerIds)
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileGallery (fileGalleryComponent)
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding, selectorTransformedLens)
import Competences.Frontend.Component.Selector.CustomSelect
  ( CustomSelectConfig (..)
  , customSelectComponent
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalId (..)
  , ModalHeight (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , inlineComponent
  , openFramedModal
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Text qualified as T
import Data.Ord (Down (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((.~), (&))

-- ===========================================================================
-- Shared view helpers
-- ===========================================================================

kindToBadge :: SubmissionKind -> M.View m a
kindToBadge (DigitalSubmission _) = Badge.primary (Badge.badgeLabel C.LblAbgegeben)
kindToBadge (NonDigitalSubmission _) = Badge.secondary (Badge.badgeLabel C.LblGemacht)
kindToBadge (VoidSubmission _) = Badge.destructive (Badge.badgeLabel C.LblNichtGemacht)

-- | Compact view for the custom select trigger: badge + date/time.
compactSubmission :: Submission -> M.View m a
compactSubmission sub =
  MH.span_
    [class_ "flex items-center gap-2"]
    [ kindToBadge sub.kind
    , MH.span_ [class_ "text-sm"] [M.text $ C.formatDateTime sub.submittedAt]
    ]

-- | Detailed view for the custom select dropdown item.
detailedSubmission :: Submission -> M.View m a
detailedSubmission sub =
  MH.span_
    [class_ "flex items-center gap-2"]
    [ kindToBadge sub.kind
    , MH.span_ [] [M.text $ C.formatDateTime sub.submittedAt]
    , submissionExtra sub
    , ownershipBadge sub.ownership
    ]

-- | Small badge showing ownership type in dropdown items.
ownershipBadge :: SubmissionOwnership -> M.View m a
ownershipBadge (IndividualSubmission _) = M.text ""
ownershipBadge (CollaborativeSubmission uids) =
  Badge.secondary (Badge.badgeText $ ms (show (length uids)) <> " " <> C.translate' C.LblStudents)

-- | Extra info column in dropdown items (file count, location, reason).
submissionExtra :: Submission -> M.View m a
submissionExtra sub = case sub.kind of
  DigitalSubmission files ->
    let n = length files
     in MH.span_
          [class_ "text-muted-foreground"]
          [M.text $ "(" <> ms (show n) <> " " <> C.translate' (if n == 1 then C.LblFile else C.LblFiles) <> ")"]
  NonDigitalSubmission mLoc ->
    case mLoc of
      Nothing -> M.text ""
      Just loc -> MH.span_ [class_ "text-muted-foreground truncate max-w-32"] [M.text $ ms loc]
  VoidSubmission reason ->
    MH.span_ [class_ "text-muted-foreground truncate max-w-32 italic"] [M.text $ ms reason]

-- ===========================================================================
-- Container component (holds selectedId, mounts children)
-- ===========================================================================

data SubmissionPreviewModel = SubmissionPreviewModel
  { selectedId :: !(Maybe SubmissionId)
  }
  deriving (Eq, Show, Generic)

data SubmissionPreviewAction = SPNoOp
  deriving (Eq, Show)

submissionPreviewComponent
  :: SyncContext -> AssignmentId -> UserId
  -> M.Component p SubmissionPreviewModel SubmissionPreviewAction
submissionPreviewComponent r aId uId =
  M.component model update view'
  where
    model = SubmissionPreviewModel {selectedId = Nothing}

    update SPNoOp = pure ()

    selectConfig =
      CustomSelectConfig
        { deriveOptions = \doc ->
            sortOn (Down . (.submittedAt)) $
              Ix.toList $
                doc.submissions Ix.@= aId Ix.@= uId
        , itemKey = (.id)
        , compact = compactSubmission
        , detailed = detailedSubmission
        }

    selectBinding = selectorTransformedLens (.id) id #selectedId

    view' m =
      Layout.vFlow
        Layout.gapM
        [ -- Header: title + custom select
          MH.div_
            [class_ "flex items-center justify-between gap-3"]
            [ Typography.h4 (C.translate' C.LblSubmissions)
            , inlineComponent
                "sub-select"
                (customSelectComponent r selectConfig selectBinding)
            ]
        , -- Preview: keyed by selectedId so it remounts on selection change
          case m.selectedId of
            Nothing ->
              MH.div_
                [class_ "flex items-center justify-center p-8 text-muted-foreground text-sm"]
                [M.text $ C.translate' C.LblNoSubmissionSelected]
            Just sid ->
              inlineComponent
                ("sub-preview-" <> ms (show sid))
                (submissionDetailComponent r sid)
        ]

-- | Binding-aware version of the submission preview component.
-- The selectedId is bound to the parent via the provided lens, so the parent
-- can observe which submission is currently selected.
submissionSelectorComponent
  :: (Eq p)
  => SyncContext -> AssignmentId -> UserId
  -> SelectorTransformedLens p Maybe SubmissionId f t
  -> M.Component p SubmissionPreviewModel SubmissionPreviewAction
submissionSelectorComponent r aId uId binding =
  (submissionPreviewComponent r aId uId)
    { M.bindings = [mkSelectorBinding binding #selectedId]
    }

-- ===========================================================================
-- Detail component (preview for a single submission)
-- ===========================================================================

data DetailProjection = DetailProjection
  { projSubmission :: !(Maybe Submission)
  , ownerNames :: ![T.Text]
  }
  deriving (Eq, Show, Generic)

detailProjection :: SubmissionId -> Document -> Maybe User -> DetailProjection
detailProjection sid doc _mUser =
  let mSub = Ix.getOne (doc.submissions Ix.@= sid)
      names = case mSub of
        Nothing -> []
        Just sub ->
          [ maybe (T.pack (show uid)) (.name) (Ix.getOne (doc.users Ix.@= uid))
          | uid <- ownerIds sub.ownership
          ]
   in DetailProjection {projSubmission = mSub, ownerNames = names}

data DetailModel = DetailModel
  { submission :: !(Maybe Submission)
  , ownerNames :: ![T.Text]
  }
  deriving (Eq, Show, Generic)

data DetailAction
  = DetailProjectionChanged !(ProjectedChange DetailProjection)
  deriving (Eq, Show)

submissionDetailComponent
  :: SyncContext -> SubmissionId
  -> M.Component p DetailModel DetailAction
submissionDetailComponent r sid =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (detailProjection sid) DetailProjectionChanged]
    }
  where
    model = DetailModel {submission = Nothing, ownerNames = []}

    update (DetailProjectionChanged pc) = M.modify $ \m ->
      m & #submission .~ pc.projection.projSubmission
        & #ownerNames .~ pc.projection.ownerNames

    view' m = case m.submission of
      Nothing ->
        MH.div_
          [class_ "flex items-center justify-center p-8 text-muted-foreground text-sm"]
          [M.text $ C.translate' C.LblNoSubmissionSelected]
      Just sub ->
        Layout.vFlow Layout.gapS
          [ viewOwnershipHeader sub m.ownerNames
          , viewSubmissionContent r sub
          ]

-- ---------------------------------------------------------------------------
-- Preview Views
-- ---------------------------------------------------------------------------

-- | Show ownership context: individual vs collaborative with co-participant names.
viewOwnershipHeader :: Submission -> [T.Text] -> M.View m a
viewOwnershipHeader sub names = case sub.ownership of
  CollaborativeSubmission _ ->
    MH.div_
      [class_ "flex items-center gap-2 px-3 py-2 bg-sky-50 border border-sky-200 rounded-md text-sm"]
      [ Icon.iconS Icon.Small Icon.IcnSocialFormGroup
      , MH.span_ [class_ "font-medium text-sky-800"]
          [M.text $ C.translate' C.LblCollaborativeSubmission <> " " <> ms (T.intercalate ", " names)]
      ]
  IndividualSubmission _ ->
    MH.div_
      [class_ "flex items-center gap-2 px-3 py-2 bg-stone-50 border border-stone-200 rounded-md text-sm"]
      [ Icon.iconS Icon.Small Icon.IcnSocialFormIndividual
      , MH.span_ [class_ "text-stone-600"]
          [M.text $ C.translate' C.LblIndividualSubmission <> ": " <> ms (T.intercalate ", " names)]
      ]

viewSubmissionContent :: SyncContext -> Submission -> M.View m DetailAction
viewSubmissionContent r sub = case sub.kind of
  DigitalSubmission files ->
    inlineComponent
      ("gallery-" <> ms (show sub.id))
      (fileGalleryComponent r files)
  NonDigitalSubmission mLoc ->
    MH.div_
      [class_ "p-4 text-sm"]
      [ Badge.secondary (Badge.badgeText (C.translate' C.LblGemacht))
      , case mLoc of
          Nothing -> M.text ""
          Just loc ->
            MH.div_
              [class_ "mt-2 text-muted-foreground"]
              [M.text $ ms loc]
      , viewRemark sub
      ]
  VoidSubmission reason ->
    MH.div_
      [class_ "p-4 text-sm"]
      [ Badge.outline (Badge.badgeText (C.translate' C.LblNichtGemacht))
      , MH.div_
          [class_ "mt-2 text-muted-foreground italic"]
          [M.text $ ms reason]
      , viewRemark sub
      ]

viewRemark :: Submission -> M.View m a
viewRemark sub = case sub.remark of
  Nothing -> M.text ""
  Just rmk ->
    MH.div_
      [class_ "mt-2 pt-2 border-t text-sm text-muted-foreground"]
      [ MH.span_ [class_ "font-medium"] [M.text $ C.translate' C.LblRemark <> ": "]
      , M.text (ms rmk)
      ]

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Mount a submission preview panel as an inline component.
-- Includes title, custom select dropdown with badges, and preview area.
submissionPreviewPanel
  :: (Eq p) => SyncContext -> AssignmentId -> UserId
  -> M.View p a
submissionPreviewPanel r aId uId =
  MH.div_ []
    [ ("submission-preview-" <> ms (show aId) <> "-" <> ms (show uId))
        M.+> submissionPreviewComponent r aId uId
    ]

-- | Open a peek modal showing the full content of a single submission.
openSubmissionPeekModal :: SyncContext -> SubmissionId -> IO ()
openSubmissionPeekModal r sid = do
  let cfg = ModalConfig
        { chrome = WindowChrome (C.translate' C.LblSubmissions) Icon.IcnView
        , modalId = ModalId ("submission-peek-" <> T.pack (show sid))
        , width = ModalWide
        , height = ModalAuto
        , pinnable = Nothing
        }
  openFramedModal r.windowManager cfg (submissionDetailComponent r sid)
