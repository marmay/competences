-- | Student submission component for assignments.
--
-- Opens as a modal dialog. Allows choosing submission kind
-- (digital upload, non-digital, or void), optional collaboration,
-- and managing existing submissions.
module Competences.Frontend.Component.Submission
  ( openSubmissionModal
  , SubmissionSummary (..)
  , submissionSummary
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
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.FileRef (FileRef (..))
import Competences.Document.Submission (SubmissionId, SubmissionKind (..), SubmissionOwnership (..), VoidReason (..), simpleVoidReasons)
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.SearchSelect qualified as SS
import Competences.Document.User (UserId)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.FileUpload (fileUploadComponent)
import Competences.Frontend.Component.SubmissionPreview qualified as SubmissionPreview
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalId (..)
  , ModalHeight (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , WindowMode
  , inlineComponent
  , openFramedModalWith
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tabs qualified as Tabs
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, fromMisoString, ms)
import Optics.Core ((&), (.~))
import Optics.Core qualified as O

-- ============================================================================
-- Submission Summary (for ViewerDetail status button)
-- ============================================================================

-- | Summary of a student's submissions for one assignment, used by status button.
data SubmissionSummary
  = NoSubmissions
  | DigitalOnly !UTCTime        -- ^ Most recent digital submission date
  | NonDigitalOnly !UTCTime     -- ^ Most recent non-digital submission date
  | DigitalAndNonDigital        -- ^ Has both kinds
  | VoidOnly                    -- ^ Only void submissions
  deriving (Eq, Show)

-- | Compute a submission summary from a list of submissions for one assignment+user.
submissionSummary :: [Submission] -> SubmissionSummary
submissionSummary [] = NoSubmissions
submissionSummary subs =
  let hasDigital = any isDigital subs
      hasNonDigital = any isNonDigital subs
      hasNonVoid = hasDigital || hasNonDigital
      latestDigital = maximum' [s.submittedAt | s <- subs, isDigital s]
      latestNonDigital = maximum' [s.submittedAt | s <- subs, isNonDigital s]
   in case (hasDigital, hasNonDigital, hasNonVoid) of
        (True, True, _) -> DigitalAndNonDigital
        (True, False, _) -> DigitalOnly (maybe (error "impossible") id latestDigital)
        (False, True, _) -> NonDigitalOnly (maybe (error "impossible") id latestNonDigital)
        (_, _, False) -> VoidOnly
        _ -> NoSubmissions
  where
    isDigital s = case s.kind of DigitalSubmission _ -> True; _ -> False
    isNonDigital s = case s.kind of NonDigitalSubmission _ -> True; _ -> False
    maximum' [] = Nothing
    maximum' xs = Just (maximum xs)

-- ============================================================================
-- Modal Entry Point
-- ============================================================================

-- | Open the submission modal for a specific assignment and user.
openSubmissionModal :: SyncContext -> AssignmentId -> UserId -> IO ()
openSubmissionModal r assignmentId userId = do
  let cfg = ModalConfig
        { chrome = WindowChrome (C.translate' C.LblAbgabe) Icon.IcnAssignment
        , modalId = ModalId ("submission-" <> T.pack (show assignmentId))
        , width = ModalWide
        , height = ModalAuto
        , pinnable = Nothing
        }
  openFramedModalWith r.windowManager cfg (submissionModalComponent r assignmentId userId)

-- ============================================================================
-- Submission Kind Tab
-- ============================================================================

-- | Which kind of submission the user is creating.
data KindTab
  = TabDigital
  | TabNonDigital
  | TabVoid
  deriving (Eq, Show, Generic)

-- | Columns for the existing submissions table.
data SubCol = SubColKind | SubColDate | SubColDetails | SubColRemark | SubColActions
  deriving (Eq, Show)

-- ============================================================================
-- Component Model & Actions
-- ============================================================================

-- | Projection: existing submissions for this assignment + user
data SubmissionProjection = SubmissionProjection
  { submissions :: ![Submission]
  , groupSubmissionAllowed :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Component model
data SubmissionModel = SubmissionModel
  { projection :: !SubmissionProjection
  , activeTab :: !KindTab
  , files :: ![FileRef]
  , locationText :: !MisoString
  , voidReasonChoice :: !VoidReason
  , voidOtherText :: !MisoString
  , remarkText :: !MisoString
  , holdingDelete :: !(HoldButton.HoldState SubmissionId)
  , collaborators :: ![User]
  }
  deriving (Eq, Generic, Show)

data SubmissionAction
  = ProjectionChanged !(ProjectedChange SubmissionProjection)
  | SetActiveTab !KindTab
  | SetLocationText !MisoString
  | SetVoidReasonChoice !VoidReason
  | SetVoidOtherText !MisoString
  | SetRemarkText !MisoString
  | SubmitWork
  | DoSubmit !UTCTime
  | PeekSubmission !SubmissionId
  | OnHoldDelete !(HoldButton.HoldAction SubmissionId)
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

submissionModalComponent
  :: SyncContext
  -> AssignmentId
  -> UserId
  -> WindowMode
  -> M.Component p SubmissionModel SubmissionAction
submissionModalComponent r assignmentId userId _wm =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (submissionProjection assignmentId userId) ProjectionChanged]
    }
  where
    model = SubmissionModel
      { projection = SubmissionProjection [] False
      , activeTab = TabDigital
      , files = []
      , locationText = ""
      , voidReasonChoice = VoidSick
      , voidOtherText = ""
      , remarkText = ""
      , holdingDelete = HoldButton.emptyHoldState
      , collaborators = []
      }

    submissionProjection :: AssignmentId -> UserId -> Document -> Maybe User -> SubmissionProjection
    submissionProjection aid uid doc _mUser =
      let mAssignment = Ix.getOne (doc.assignments Ix.@= aid)
       in SubmissionProjection
            { submissions = sortOn (Down . (.submittedAt)) $ Ix.toList $ doc.submissions Ix.@= aid Ix.@= uid
            , groupSubmissionAllowed = maybe False (.groupSubmissionAllowed) mAssignment
            }

    update (ProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    update (SetActiveTab tab) =
      M.modify $ \m -> m & #activeTab .~ tab

    update (SetLocationText t) =
      M.modify $ \m -> m & #locationText .~ t

    update (SetVoidReasonChoice r') =
      M.modify $ \m -> m & #voidReasonChoice .~ r'

    update (SetVoidOtherText t) =
      M.modify $ \m -> m & #voidOtherText .~ t

    update (SetRemarkText t) =
      M.modify $ \m -> m & #remarkText .~ t

    update SubmitWork =
      M.io $ DoSubmit <$> getCurrentTime

    update (DoSubmit now) = do
      m <- M.get
      let remarkStr = T.pack (fromMisoString m.remarkText)
          mRemark = if T.null remarkStr then Nothing else Just remarkStr
          mKind = case m.activeTab of
            TabDigital
              | null m.files -> Nothing
              | otherwise -> Just (DigitalSubmission m.files)
            TabNonDigital ->
              let loc = T.pack (fromMisoString m.locationText)
               in Just (NonDigitalSubmission (if T.null loc then Nothing else Just loc))
            TabVoid -> case m.voidReasonChoice of
              VoidOther _ ->
                let t = T.strip (T.pack (fromMisoString m.voidOtherText))
                 in if T.null t then Nothing else Just (VoidSubmission (VoidOther t))
              other -> Just (VoidSubmission other)
      case mKind of
        Nothing -> pure ()  -- Validation failed, do nothing
        Just kind -> do
          M.io_ $ do
            sid <- nextId r
            let ownership = case m.collaborators of
                      [] -> IndividualSubmission userId
                      cs -> CollaborativeSubmission (userId :| map (.id) cs)
                submission = Submission
                  { id = sid
                  , assignmentId = assignmentId
                  , ownership = ownership
                  , kind = kind
                  , remark = mRemark
                  , submittedAt = now
                  }
            modifySyncDocument r $ Submissions (OnSubmissions (Create submission))
          M.modify $ \m' -> m'
            & #files .~ []
            & #locationText .~ ""
            & #voidReasonChoice .~ VoidSick
            & #voidOtherText .~ ""
            & #remarkText .~ ""
            & #collaborators .~ []

    update (PeekSubmission sid) =
      M.io_ $ SubmissionPreview.openSubmissionPeekModal r sid

    update (OnHoldDelete ha) =
      HoldButton.handleHoldAction' #holdingDelete doDelete OnHoldDelete ha
      where
        doDelete sid = modifySyncDocument r $ Submissions (OnSubmissions (Delete sid))

    -- ========================================================================
    -- View
    -- ========================================================================

    view' m =
      Layout.padM $
        Layout.vFlow
          Layout.gapM
          [ viewExistingSubmissions m
          , Typography.h4 $ C.translate' C.LblNewSubmission
          , viewNewSubmissionForm m
          ]

    viewNewSubmissionForm m =
      Tabs.cardWithTabs
        Tabs.Tabs
          { tabs = [TabDigital, TabNonDigital, TabVoid]
          , activeTab = m.activeTab
          , onSelect = SetActiveTab
          , tabSpec = \case
              TabDigital -> Tabs.TabSpec (C.translate' C.LblUploadFiles) False
              TabNonDigital -> Tabs.TabSpec (C.translate' C.LblDoneInNotebook) False
              TabVoid -> Tabs.TabSpec (C.translate' C.LblNichtGemacht) (hasNonVoidSubmission m)
          , tabContent = \case
              TabDigital -> collabSection <> [viewDigitalForm m, viewRemarkAndSubmit m]
              TabNonDigital -> collabSection <> [viewNonDigitalForm m, viewRemarkAndSubmit m]
              TabVoid -> [viewVoidForm m, viewRemarkAndSubmit m]
          }
      where
        collabSection = [viewCollaboratorSelector | m.projection.groupSubmissionAllowed]

    collaboratorConfig :: SS.SearchSelectConfig User UserId
    collaboratorConfig =
      SS.SearchSelectConfig
        { projectItems = \doc -> case Ix.getOne (doc.assignments Ix.@= assignmentId) of
            Nothing -> []
            Just a ->
              filter (\u -> Set.member u.id a.studentIds && u.id /= userId)
                $ Ix.toAscList (Proxy @Text) doc.users
        , itemId = (.id)
        , itemLabel = (.name)
        , metaFilters = []
        , viewTag = \u -> (Icon.IcnSocialFormIndividual, ms u.name)
        , placeholder = fromMisoString $ C.translate' C.LblCollaborativeSubmission
        , selectionOrder = SS.AutoOrder id
        , tagLayout = SS.TagsInline
        , onCreate = Nothing
        }

    viewCollaboratorSelector =
      MH.div_
        [class_ "space-y-1"]
        [ Typography.small $ C.translate' C.LblCollaborativeSubmission
        , inlineComponent "collaborator-selector"
            (SS.searchSelectComponent r "collaborator-selector" collaboratorConfig []
              (selectorLens (O.castOptic #collaborators)))
        ]

    viewRemarkAndSubmit m =
      Layout.vFlow
        Layout.gapS
        [ Input.textInput' (C.translate' C.LblRemark) m.remarkText SetRemarkText
        , viewSubmitButton m
        ]

    viewDigitalForm m =
      MH.div_
        [class_ "space-y-2"]
        [ inlineComponent "submission-file-upload"
            (fileUploadComponent r (Just (C.translate' C.LblFilesForSubmission)) m.files #files)
        ]

    viewNonDigitalForm _m =
      Input.textInput' (C.translate' C.LblLocation) _m.locationText SetLocationText

    viewVoidForm _m =
      Layout.vFlow
        Layout.gapS
        [ MH.div_
            [class_ "flex flex-wrap gap-2"]
            (map (viewVoidReasonButton _m.voidReasonChoice) (simpleVoidReasons <> [VoidOther ""]))
        , case _m.voidReasonChoice of
            VoidOther _ ->
              Input.textInput' (C.translate' C.LblVoidReason) _m.voidOtherText SetVoidOtherText
            _ -> M.text ""
        ]

    viewVoidReasonButton activeChoice reason =
      let isActive = case (activeChoice, reason) of
            (VoidOther _, VoidOther _) -> True
            _ -> activeChoice == reason
          lbl = C.translateVoidReason reason
       in if isActive
            then Button.primarySm (Button.button lbl (SetVoidReasonChoice reason))
            else Button.outlineSm (Button.button lbl (SetVoidReasonChoice reason))

    viewSubmitButton m =
      let canSubmit = case m.activeTab of
            TabDigital -> not (null m.files)
            TabNonDigital -> True
            TabVoid -> case m.voidReasonChoice of
              VoidOther _ -> not (T.null (T.strip (T.pack (fromMisoString m.voidOtherText))))
              _ -> True
       in if canSubmit
            then Button.primary (Button.button (C.translate' C.LblAbgabe) SubmitWork)
            else Button.primary (Button.button (C.translate' C.LblAbgabe) Button.Disabled)

    hasNonVoidSubmission m =
      any (\s -> case s.kind of VoidSubmission _ -> False; _ -> True) m.projection.submissions

    -- ========================================================================
    -- Existing submissions table
    -- ========================================================================

    viewExistingSubmissions m =
      if null m.projection.submissions
        then Typography.small $ C.translate' C.LblNoSubmissions
        else Layout.vFlow
          Layout.gapS
          [ Typography.h4 $ C.translate' C.LblSubmissions
          , Table.viewTable $ Table.defTable
              { Table.columns = [SubColKind, SubColDate, SubColDetails, SubColRemark, SubColActions]
              , Table.rows = m.projection.submissions
              , Table.columnSpec = subColumnSpec
              , Table.rowContents = Table.cellContents (subCell m.holdingDelete)
              }
          ]

    subColumnSpec SubColKind = Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblAbgabe)
    subColumnSpec SubColDate = Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblDate)
    subColumnSpec SubColDetails = Table.TableColumnSpec Table.EqualWidthColumn (C.translate' C.LblDetails)
    subColumnSpec SubColRemark = Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblRemark)
    subColumnSpec SubColActions = Table.TableColumnSpec Table.DoubleActionColumn ""

    subCell _holding s SubColKind =
      MH.div_ [class_ "px-3 py-2"] [kindBadge s.kind]
    subCell _holding s SubColDate =
      MH.div_ [class_ "px-3 py-2 whitespace-nowrap"]
        [Typography.small $ C.formatDateTime s.submittedAt]
    subCell _holding s SubColDetails =
      MH.div_ [class_ "px-3 py-2 truncate"] [viewKindDetails s.kind]
    subCell _holding s SubColRemark =
      MH.div_ [class_ "px-3 py-2"]
        [ case s.remark of
            Nothing -> M.text ""
            Just rmk -> MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ ms rmk]
        ]
    subCell holding s SubColActions =
      Layout.hFlow (Layout.gapS <> Layout.crossCenter)
        [ Button.ghostSm (Button.button Icon.IcnView (PeekSubmission s.id))
        , HoldButton.holdDeleteButtonSm OnHoldDelete holding s.id
        ]

    kindBadge (DigitalSubmission _) = Badge.primary (Badge.badgeText (C.translate' C.LblAbgegeben))
    kindBadge (NonDigitalSubmission _) = Badge.secondary (Badge.badgeText (C.translate' C.LblGemacht))
    kindBadge (VoidSubmission _) = Badge.outline (Badge.badgeText (C.translate' C.LblNichtGemacht))

    viewKindDetails (DigitalSubmission files) =
      let n = length files
          w = if n == 1 then C.translate' C.LblFile else C.translate' C.LblFiles
       in MH.span_ [class_ "text-sm text-muted-foreground"]
            [M.text $ ms $ "(" <> T.pack (show n) <> " " <> fromMisoString w <> ")"]
    viewKindDetails (NonDigitalSubmission mLoc) =
      case mLoc of
        Nothing -> M.text ""
        Just loc -> MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ ms loc]
    viewKindDetails (VoidSubmission reason) =
      MH.span_ [class_ "text-sm text-muted-foreground italic"] [M.text $ C.translateVoidReason reason]
