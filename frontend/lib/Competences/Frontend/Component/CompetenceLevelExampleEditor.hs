-- |
-- Module      : Competences.Frontend.Component.CompetenceLevelExampleEditor
-- Description : Modal editor for competence level examples
--
-- Opened from the competence grid editor. Shows a left panel with a
-- reorderable list of examples, and a right panel with a RichContent
-- editor and file upload for the selected example.
module Competences.Frontend.Component.CompetenceLevelExampleEditor
  ( openExampleEditor
  , exampleEditorComponent
  )
where

import Competences.Command
  ( Command (..)
  , CompetenceLevelExamplesCommand (..)
  , EntityCommand (..)
  , ModifyCommand (..)
  )
import Competences.Command.CompetenceLevelExamples (CompetenceLevelExamplePatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceLevelExample (..)
  , CompetenceLevelExampleId
  , CompetenceLevelExampleIxs
  , Document (..)
  , FileRef
  , Level
  , orderMax
  )
import Competences.Document.Competence (CompetenceId, CompetenceLevelId)
import Competences.Document.Order (orderPosition)
import Competences.Document.Order qualified as O
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (fileUploadComponent)
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), isContentValid, richContentEditorComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , mkCreateAndLock
  , mkLock
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , inlineComponent
  , inlineComponentAttrs
  , openFramedModal
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core (Lens', (.~), (?~))

-- ============================================================================
-- Open modal
-- ============================================================================

-- | Open the example editor as a framed modal.
openExampleEditor :: SyncContext -> T.Text -> CompetenceId -> Level -> IO ()
openExampleEditor r competenceDesc compId lvl =
  let levelLabel = C.translate' (C.LblCompetenceLevelDescription lvl)
      title = C.translate' C.LblExamples <> " '" <> M.ms competenceDesc <> "' / " <> levelLabel
      frameCfg =
        ModalConfig
          (WindowChrome title Icon.IcnInfo)
          (ModalId $ "examples-" <> T.pack (show compId) <> "-" <> T.pack (show lvl))
          ModalWide
          ModalFull
          Nothing
   in openFramedModal r.windowManager frameCfg (exampleEditorComponent r compId lvl)

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { examples :: ![CompetenceLevelExample]
  -- ^ Ordered list of examples for this level
  , selectedIndex :: !(Maybe Int)
  -- ^ Currently selected example index
  , editing :: !Bool
  -- ^ Whether we are in edit mode for the selected example (= entity is locked)
  , pendingEdit :: !Bool
  -- ^ A new example was just created; auto-focus and edit on next DocumentUpdated
  , holdDeleteState :: !(HoldButton.HoldState CompetenceLevelExampleId)
  , reorderFrom :: !(Maybe CompetenceLevelExampleId)
  -- ^ Item being reordered (two-step pick-then-place)
  , contentState :: !(ContentState RichContent)
  -- ^ Synced via binding from richContentEditorComponent
  , attachments :: ![FileRef]
  -- ^ Synced via binding from fileUploadComponent
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = DocumentUpdated !DocumentChange
  | SelectExample !Int
  | AddExample
  | StartEditing
  | CancelEditing
  | FinishEditing
  | StartReorder !CompetenceLevelExampleId
  | CancelReorder
  | ReorderToTop
  | ReorderToBottom
  | ReorderBefore !CompetenceLevelExampleId
  | ReorderAfter !CompetenceLevelExampleId
  | HoldDeleteAction !(HoldButton.HoldAction CompetenceLevelExampleId)
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Component
-- ============================================================================

exampleEditorComponent :: SyncContext -> CompetenceId -> Level -> M.Component p Model Action
exampleEditorComponent r compId lvl =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    levelKey :: CompetenceLevelId
    levelKey = (compId, lvl)

    initialModel :: Model
    initialModel =
      Model
        { examples = []
        , selectedIndex = Nothing
        , editing = False
        , pendingEdit = False
        , holdDeleteState = HoldButton.emptyHoldState
        , reorderFrom = Nothing
        , contentState = Valid mempty
        , attachments = []
        }

    getExamples :: Document -> [CompetenceLevelExample]
    getExamples doc =
      Ix.toAscList (Proxy @O.Order) (doc.competenceLevelExamples Ix.@= levelKey)

    -- | Compute and release patches for the currently selected example.
    releaseCurrent :: Model -> M.Effect p Model Action
    releaseCurrent m = case m.selectedIndex of
      Nothing -> pure ()
      Just idx -> case drop idx m.examples of
        (ex : _) ->
          let contentPatch = case m.contentState of
                Valid rc | rc /= ex.content -> Just (ex.content, rc)
                _ -> Nothing
              attPatch =
                if m.attachments /= ex.attachments
                  then Just (ex.attachments, m.attachments)
                  else Nothing
              patch = CompetenceLevelExamplePatch contentPatch attPatch
           in M.io_ $
                modifySyncDocument r $
                  CompetenceLevelExamples $
                    OnCompetenceLevelExamples $
                      Modify ex.id (Release patch)
        _ -> pure ()

    -- | Release without patches (cancel).
    releaseEmpty :: Model -> M.Effect p Model Action
    releaseEmpty m = case m.selectedIndex of
      Nothing -> pure ()
      Just idx -> case drop idx m.examples of
        (ex : _) ->
          M.io_ $
            modifySyncDocument r $
              CompetenceLevelExamples $
                OnCompetenceLevelExamples $
                  Modify ex.id (Release $ CompetenceLevelExamplePatch Nothing Nothing)
        _ -> pure ()

    -- Select the example at index (no lock — viewer is unlocked)
    selectExample :: Int -> M.Effect p Model Action
    selectExample idx = do
      m <- M.get
      -- Release lock if currently editing
      if m.editing then releaseCurrent m else pure ()
      case drop idx m.examples of
        (ex : _) ->
          M.modify $ \m' ->
            m'
              { selectedIndex = Just idx
              , editing = False
              , contentState = Valid ex.content
              , attachments = ex.attachments
              }
        _ -> pure ()

    update :: Action -> M.Effect p Model Action
    update (DocumentUpdated dc) = do
      let newExamples = getExamples dc.document
      m <- M.get
      -- If a new example was just added, select it and enter editing (already locked)
      if m.pendingEdit && length newExamples > length m.examples
        then do
          let lastIdx = length newExamples - 1
              ex = newExamples !! lastIdx
          M.modify $ \m' ->
            m'
              { examples = newExamples
              , selectedIndex = Just lastIdx
              , editing = True
              , pendingEdit = False
              , contentState = Valid ex.content
              , attachments = ex.attachments
              }
        else do
          -- Preserve selection if possible
          let newIdx = case m.selectedIndex of
                Just i | i < length newExamples -> Just i
                Just _ -> if null newExamples then Nothing else Just (length newExamples - 1)
                Nothing -> Nothing
          -- Only reset contentState/attachments if not editing
          let (newContentState, newAttachments) =
                if m.editing
                  then (m.contentState, m.attachments)
                  else case newIdx of
                    Just i -> case drop i newExamples of
                      (ex : _) -> (Valid ex.content, ex.attachments)
                      _ -> (Valid mempty, [])
                    Nothing -> (Valid mempty, [])
          -- Clear editing if selected example was deleted
          let newEditing = case newIdx of
                Just i -> case drop i newExamples of
                  (_ : _) -> m.editing
                  _ -> False
                Nothing -> False
          -- Clear reorderFrom if source item was deleted
          let newReorderFrom = case m.reorderFrom of
                Just srcId -> case filter (\e -> e.id == srcId) newExamples of
                  _ : _ -> Just srcId
                  [] -> Nothing
                Nothing -> Nothing
          M.modify $ \m' ->
            m'
              { examples = newExamples
              , selectedIndex = newIdx
              , editing = newEditing
              , pendingEdit = False
              , reorderFrom = newReorderFrom
              , contentState = newContentState
              , attachments = newAttachments
              }

    update (SelectExample idx) = selectExample idx

    update AddExample = do
      M.modify $ #pendingEdit .~ True
      M.io_ $ do
        exId <- nextId r
        let ex =
              CompetenceLevelExample
                { id = exId
                , competenceId = compId
                , level = lvl
                , order = orderMax
                , content = mempty
                , attachments = []
                }
        modifySyncDocument r $
          CompetenceLevelExamples $ OnCompetenceLevelExamples $ mkCreateAndLock r ex

    update StartEditing = do
      m <- M.get
      case m.selectedIndex of
        Just idx -> case drop idx m.examples of
          (ex : _) -> do
            M.modify $ \m' ->
              m'
                { editing = True
                , contentState = Valid ex.content
                , attachments = ex.attachments
                }
            M.io_ $
              modifySyncDocument r $
                CompetenceLevelExamples $ OnCompetenceLevelExamples $ Modify ex.id (mkLock r)
          _ -> pure ()
        Nothing -> pure ()

    update CancelEditing = do
      m <- M.get
      releaseEmpty m
      M.modify $ #editing .~ False

    update FinishEditing = do
      m <- M.get
      releaseCurrent m
      M.modify $ #editing .~ False

    update (StartReorder exId) =
      M.modify $ #reorderFrom ?~ exId

    update CancelReorder =
      M.modify $ #reorderFrom .~ Nothing

    update ReorderToTop = issueReorder O.Front
    update ReorderToBottom = issueReorder O.Back
    update (ReorderBefore targetId) = issueReorder (O.Before targetId)
    update (ReorderAfter targetId) = issueReorder (O.After targetId)

    update (HoldDeleteAction ha) =
      HoldButton.handleHoldAction #holdDeleteState doDelete HoldDeleteAction ha
      where
        doDelete exId = modifySyncDocument r $
          CompetenceLevelExamples $ OnCompetenceLevelExamples $ Delete exId

    issueReorder :: O.Reorder CompetenceLevelExample -> M.Effect p Model Action
    issueReorder direction = do
      m <- M.get
      case m.reorderFrom of
        Nothing -> pure ()
        Just srcId ->
          case orderPosition (Ix.fromList m.examples :: Ix.IxSet CompetenceLevelExampleIxs CompetenceLevelExample) srcId of
            Just pos -> do
              M.io_ $ modifySyncDocument r $
                CompetenceLevelExamples $ ReorderCompetenceLevelExample pos direction
              M.modify $ #reorderFrom .~ Nothing
            Nothing -> pure ()

    -- ======================================================================
    -- View
    -- ======================================================================

    view :: Model -> M.View Model Action
    view m =
      MH.div_ [class_ "flex gap-4 h-full p-4"]
        [ -- Left panel: selector
          MH.div_ [class_ "w-64 shrink-0 flex flex-col gap-2 h-full border-r border-border pr-4"]
            [ SL.selectorHeader (C.translate' C.LblExample) (Just AddExample)
            , SL.selectorList $
                if null m.examples
                  then
                    [ MH.div_ [class_ "p-4 text-center"]
                        [Typography.muted $ C.translate' C.LblNoExamples]
                    ]
                  else
                    zipWith (renderSelectorItem m) [0 ..] m.examples
            ]
        , -- Right panel: detail / editor
          MH.div_ [class_ "flex-1 min-w-0 overflow-y-auto"]
            [rightPanel m]
        ]

    renderSelectorItem :: Model -> Int -> CompetenceLevelExample -> M.View Model Action
    renderSelectorItem m idx ex =
      let isSelected = m.selectedIndex == Just idx
       in case m.reorderFrom of
            -- Reorder mode: source item
            Just srcId | ex.id == srcId ->
              MH.div_
                [ class_ "px-3 py-2 rounded min-h-10 bg-primary/10 text-primary"
                ]
                [ Layout.hFlow (Layout.gapS <> Layout.crossCenter)
                    [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] Icon.IcnInfo
                    , MH.span_ [class_ "text-sm truncate flex-1"] [M.text $ previewText ex]
                    , Button.buttonGroup
                        [ Button.secondarySm (Button.button Icon.IcnDoubleArrowUp (Just ReorderToTop))
                        , Button.destructiveSm (Button.button Icon.IcnCancel (Just CancelReorder))
                        , Button.secondarySm (Button.button Icon.IcnDoubleArrowDown (Just ReorderToBottom))
                        ]
                    ]
                ]
            -- Reorder mode: target item
            Just _ ->
              MH.div_
                [ class_ $
                    "px-3 py-2 rounded cursor-pointer transition-colors min-h-10 "
                      <> if isSelected then "bg-primary/10 text-primary" else "hover:bg-muted"
                , MH.onClick (SelectExample idx)
                ]
                [ Layout.hFlow (Layout.gapS <> Layout.crossCenter)
                    [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] Icon.IcnInfo
                    , MH.span_ [class_ "text-sm truncate flex-1"] [M.text $ previewText ex]
                    , Button.buttonGroup
                        [ Button.secondarySm (Button.button Icon.IcnArrowUp (Just (ReorderBefore ex.id)))
                        , Button.secondarySm (Button.button Icon.IcnArrowDown (Just (ReorderAfter ex.id)))
                        ]
                    ]
                ]
            -- Normal mode
            Nothing ->
              MH.div_
                [ class_ $
                    "px-3 py-2 rounded cursor-pointer transition-colors min-h-10 "
                      <> if isSelected then "bg-primary/10 text-primary" else "hover:bg-muted"
                , MH.onClick (SelectExample idx)
                ]
                [ Layout.hFlow (Layout.gapS <> Layout.crossCenter)
                    [ Icon.icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] Icon.IcnInfo
                    , MH.span_ [class_ "text-sm truncate flex-1"] [M.text $ previewText ex]
                    , Button.secondarySm (Button.button Icon.IcnReorder (Just (StartReorder ex.id)))
                    ]
                ]

    rightPanel :: Model -> M.View Model Action
    rightPanel m = case m.selectedIndex of
      Nothing ->
        MH.div_ [class_ "flex items-center justify-center h-full text-stone-400"]
          [Typography.muted $ C.translate' C.LblSelectExample]
      Just idx -> case drop idx m.examples of
        (ex : _)
          | m.editing -> editingView m ex
          | otherwise -> viewerView m ex
        _ -> Layout.empty

    viewerView :: Model -> CompetenceLevelExample -> M.View Model Action
    viewerView m ex =
      Layout.vFlow Layout.gapM
        [ -- Header
          Typography.h3 $ C.translate' C.LblExample
        , -- Content preview
          if ex.content == mempty
            then Typography.placeholder $ C.translate' C.LblNoContent
            else
              MH.div_ [class_ "prose prose-stone prose-sm max-w-none"]
                [renderRichTextWithFiles r.formulaCache r ex.attachments ex.content]
        , -- Buttons at bottom: Edit + Delete
          Layout.hFlow (Layout.gapS <> Layout.mainCenter)
            [ Button.secondary (Button.button (Button.IconTextS, Icon.IcnEdit, C.LblEdit) StartEditing)
            , HoldButton.holdDeleteButton HoldDeleteAction m.holdDeleteState ex.id
            ]
        ]

    editingView :: Model -> CompetenceLevelExample -> M.View Model Action
    editingView m ex =
      Layout.vFlow Layout.gapM
        [ -- Header
          Typography.h3 $ C.translate' C.LblEditExample
        , -- RichContent editor (debounced via binding)
          inlineComponentAttrs ("example-rc-editor-" <> M.ms (show ex.id))
            []
            (richContentEditorComponent r.formulaCache ex.content contentStateLens)
        , -- Attachments
          inlineComponent ("example-attachments-" <> M.ms (show ex.id))
            (fileUploadComponent r (Just $ C.translate' C.LblAttachments) ex.attachments attachmentsLens)
        , -- Buttons at bottom: Apply (disabled while debouncing) + Cancel
          Layout.hFlow (Layout.gapS <> Layout.mainCenter)
            [ Button.primary (Button.button (Button.IconTextS, Icon.IcnApply, C.LblApply) (isContentValid m.contentState, FinishEditing))
            , Button.destructive (Button.button (Button.IconTextS, Icon.IcnCancel, C.LblCancel) CancelEditing)
            ]
        ]

    -- Simple field lenses for bindings
    contentStateLens :: Lens' Model (ContentState RichContent)
    contentStateLens = #contentState

    attachmentsLens :: Lens' Model [FileRef]
    attachmentsLens = #attachments

-- | Extract a short preview text from an example's content.
previewText :: CompetenceLevelExample -> M.MisoString
previewText ex =
  let raw = toRawText ex.content
   in if T.null raw
        then "(leer)"
        else M.ms $ T.take 50 raw
