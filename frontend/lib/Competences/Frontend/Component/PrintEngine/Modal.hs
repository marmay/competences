module Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalModel (..)
  , PrintModalAction (..)
  , initPrintModalModel
  , initFromLayout
  , updatePrintModal
  , printModalView
  , measurementContainer
  , footerMeasureContainer
  , RemeasurePolicy (..)
  , remeasurePolicy
  , reorderedTaskIds
  )
where

import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Layout (Layout (..), LayoutId)
import Competences.Document.Solution (SolutionId, SolutionType (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.CSS (printStyleView)
import Competences.Frontend.Component.PrintEngine.Footer qualified as Footer
import Competences.Frontend.Component.PrintEngine.Measure (PageGroup (..), PageGrouping, emptyPageGroup)
import Competences.Frontend.Component.RichContent (FormulaCache)
import Competences.Frontend.Component.PrintEngine.Page qualified as Page
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentPreset (..)
  , ContentSettings (..)
  , FontFamily (..)
  , GridConfig (..)
  , ImagePrintSetting (..)
  , Orientation (..)
  , PaperSize (..)
  , PrintImagePosition (..)
  , PrintSettings (..)
  , PrintTab (..)
  , TaskContentSetting (..)
  , TaskHeaderStyle (..)
  , TaskInfo (..)
  , TaskLayout (..)
  , applyPreset
  , cellsPerPage
  , defaultGridHeightMm
  , defaultImagePrintSetting
  , defaultPrintSettings
  , pageSizeMm
  , pageMarginMm
  , taskContentSetting
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalHeight (..), ModalId (..), ModalWidth (..), WindowChrome (..))
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tabs qualified as Tabs
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.View.WindowFrame (modalFrame)
import Data.Function ((&))
import Optics.Core ((.~))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, fromMisoString, ms)
import Text.Read (readMaybe)

-- | Modal state
data PrintModalModel = PrintModalModel
  { layoutId :: !LayoutId
  , layoutAssignmentId :: !AssignmentId
  , layoutCreatedAt :: !UTCTime
  , settings :: !PrintSettings
  , contentSettings :: !ContentSettings
  , selectedPreset :: !ContentPreset
  , taskInfos :: ![TaskInfo]
  , activeTab :: !PrintTab
  , previewTaskIndex :: !Int
  , pageGrouping :: !PageGrouping
  , footerDraft :: !(Maybe Text)
  -- ^ Immediate draft for the footer textarea; applied to contentSettings on debounce
  , reorderMode :: !Bool
  -- ^ Whether task reorder mode is active
  , originalTaskOrder :: ![TaskId]
  -- ^ Assignment task order when reorder mode was entered
  }
  deriving (Eq, Show, Generic)

-- | Modal actions
data PrintModalAction
  = SetPaperSize !PaperSize
  | SetOrientation !Orientation
  | SetFontSize !Double
  | SetTaskLayout !TaskLayout
  | SetGridRows !Int
  | SetGridCols !Int
  | SetGroupedCopies !Int
  | SetTotalCopies !Int
  | SetShowTitle !Bool
  | SetShowHeader !Bool
  | SetShowFooter !Bool
  | SetShowNameField !Bool
  | SetTaskHeaderStyle !TaskHeaderStyle
  | SetDuplexLayout !Bool
  | SetDistributeLastPage !Bool
  | SetFontFamily !FontFamily
  | SetCustomFooter !(Maybe Text)
  | SetPoints !TaskId !(Maybe Double)
  | MeasuredPageGrouping !PageGrouping
  | PreviewNext
  | PreviewPrev
  | SaveLayout
  | PrintAndSaveLayout
  | CancelPrint
  | SwitchTab !PrintTab
  | ApplyPreset !ContentPreset
  | ToggleDescription !TaskId
  | ToggleSolution !TaskId !SolutionId
  | ToggleGrid !TaskId
  | SetGridHeight !TaskId !Double
  | ToggleInlineAnswer !TaskId
  | SetItemsPerRow !TaskId !Int
  | RemeasurePages
  | ToggleReorderMode
  | MoveTaskUp !TaskId
  | MoveTaskDown !TaskId
  | OpenRenumberModal
  | SetImageSize !TaskId !Text !Int
  | SetImagePosition !TaskId !Text !PrintImagePosition
  | ToggleImageBackdrop !TaskId !Text
  deriving (Eq, Show)

-- | Initialize modal with task infos, applying Aufgabenblatt preset
initPrintModalModel :: Layout -> [TaskInfo] -> PrintModalModel
initPrintModalModel layout infos = PrintModalModel
  { layoutId = layout.id
  , layoutAssignmentId = layout.assignmentId
  , layoutCreatedAt = layout.createdAt
  , settings = defaultPrintSettings
  , contentSettings = applyPreset Aufgabenblatt infos
  , selectedPreset = Aufgabenblatt
  , taskInfos = infos
  , activeTab = FormatTab
  , previewTaskIndex = 0
  , pageGrouping = []
  , footerDraft = Nothing
  , reorderMode = False
  , originalTaskOrder = []
  }

-- | Initialize modal from an existing Layout entity
initFromLayout :: Layout -> [TaskInfo] -> PrintModalModel
initFromLayout layout infos = PrintModalModel
  { layoutId = layout.id
  , layoutAssignmentId = layout.assignmentId
  , layoutCreatedAt = layout.createdAt
  , settings = layout.printSettings
  , contentSettings = layout.contentSettings
  , selectedPreset = layout.preset
  , taskInfos = infos
  , activeTab = FormatTab
  , previewTaskIndex = 0
  , pageGrouping = []
  , footerDraft = layout.contentSettings.customFooter
  , reorderMode = False
  , originalTaskOrder = []
  }

-- | Pure update for the modal model.
-- 'total' is the number of navigable items (expanded tasks for continuous, pages for grid).
updatePrintModal :: PrintModalAction -> Int -> PrintModalModel -> PrintModalModel
updatePrintModal (SetPaperSize ps) _total m =
  m {settings = m.settings {paperSize = ps}, pageGrouping = []}
updatePrintModal (SetOrientation o) _total m =
  m {settings = m.settings {orientation = o}, pageGrouping = []}
updatePrintModal (SetFontSize fs) _total m =
  m {settings = m.settings {baseFontSize = max 6.0 (min 20.0 fs)}, pageGrouping = []}
updatePrintModal (SetTaskLayout tl) _total m =
  m {settings = m.settings {taskLayout = tl}, pageGrouping = []}
updatePrintModal (SetGridRows r) _total m =
  let gc = currentGridConfig m.settings
      gc' = gc {rows = clampGrid r}
   in m {settings = m.settings {taskLayout = Grid gc'}, pageGrouping = []}
updatePrintModal (SetGridCols c) _total m =
  let gc = currentGridConfig m.settings
      gc' = gc {cols = clampGrid c}
   in m {settings = m.settings {taskLayout = Grid gc'}, pageGrouping = []}
updatePrintModal (SetGroupedCopies n) _total m =
  m {settings = m.settings {groupedCopies = clampCopies n}, pageGrouping = []}
updatePrintModal (SetTotalCopies n) _total m =
  m {settings = m.settings {totalCopies = clampCopies n}, pageGrouping = []}
updatePrintModal (SetShowTitle b) _total m =
  (m & #contentSettings .~ m.contentSettings {showTitle = b}) {pageGrouping = []}
updatePrintModal (SetShowHeader b) _total m =
  m {settings = m.settings {showHeader = b}}
updatePrintModal (SetShowFooter b) _total m =
  m {settings = m.settings {showFooter = b}}
updatePrintModal (SetShowNameField b) _total m =
  (m & #contentSettings .~ m.contentSettings {showNameField = b}) {pageGrouping = []}
updatePrintModal (SetTaskHeaderStyle s) _total m =
  m {settings = m.settings {taskHeaderStyle = s}}
updatePrintModal (SetDuplexLayout b) _total m =
  m {settings = m.settings {duplexLayout = b}}
updatePrintModal (SetDistributeLastPage b) _total m =
  m {settings = m.settings {distributeLastPage = b}, pageGrouping = []}
updatePrintModal (SetFontFamily ff) _total m =
  m {settings = m.settings {fontFamily = ff}, pageGrouping = []}
updatePrintModal (SetCustomFooter mf) _total m =
  m {footerDraft = mf}
updatePrintModal (SetPoints tid mp) _total m =
  m & #contentSettings .~ modifyTaskSetting tid (setPoints mp) m.contentSettings
updatePrintModal RemeasurePages _total m =
  m { pageGrouping = []
    , contentSettings = setCustomFooter m.footerDraft m.contentSettings
    }
updatePrintModal (MeasuredPageGrouping pg) _total m =
  let maxIdx = max 0 (length pg - 1)
   in m {pageGrouping = pg, previewTaskIndex = min m.previewTaskIndex maxIdx}
updatePrintModal PreviewNext total m =
  m {previewTaskIndex = min (total - 1) (m.previewTaskIndex + 1)}
updatePrintModal PreviewPrev _total m =
  m {previewTaskIndex = max 0 (m.previewTaskIndex - 1)}
updatePrintModal SaveLayout _total m = m
updatePrintModal PrintAndSaveLayout _total m = m
updatePrintModal CancelPrint _total m = m
updatePrintModal (SwitchTab tab) _total m =
  m {activeTab = tab}
updatePrintModal (ApplyPreset preset) _total m =
  let cs = applyPreset preset m.taskInfos
   in m {contentSettings = cs, selectedPreset = preset, pageGrouping = [], footerDraft = cs.customFooter}
updatePrintModal (ToggleDescription tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {showDescription = not tcs.showDescription}) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal (ToggleSolution tid sid) _total m =
  m { contentSettings = modifyTaskSetting tid (toggleSolution sid) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal (ToggleGrid tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {gridHeightMm = case tcs.gridHeightMm of Nothing -> Just defaultGridHeightMm; Just _ -> Nothing}) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal (SetGridHeight tid h) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {gridHeightMm = Just (max 5.0 (min 200.0 h))}) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal (ToggleInlineAnswer tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {inlineAnswer = not tcs.inlineAnswer}) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal (SetItemsPerRow tid n) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {itemsPerRow = max 1 (min 4 n)}) m.contentSettings
    , pageGrouping = []
    }
updatePrintModal ToggleReorderMode _total m =
  m {reorderMode = not m.reorderMode}
updatePrintModal (MoveTaskUp tid) _total m =
  m {taskInfos = swapWithPrev (\ti -> ti.taskId == tid) m.taskInfos, pageGrouping = []}
updatePrintModal (MoveTaskDown tid) _total m =
  m {taskInfos = swapWithNext (\ti -> ti.taskId == tid) m.taskInfos, pageGrouping = []}
updatePrintModal OpenRenumberModal _total m = m
-- Image-knob actions deliberately do NOT clear `pageGrouping`. Keeping
-- the prior grouping while a remeasure is in flight keeps the preview
-- coherent (slightly stale layout, but images render at the new size
-- because `wrapImageForPrint` re-reads contentSettings). The next
-- `MeasuredPageGrouping` replaces it with up-to-date heights.
updatePrintModal (SetImageSize tid url pct) _total m =
  m & #contentSettings .~ modifyImageSetting tid url (#sizePct .~ max 10 (min 100 pct)) m.contentSettings
updatePrintModal (SetImagePosition tid url pos) _total m =
  m & #contentSettings .~ modifyImageSetting tid url (#position .~ pos) m.contentSettings
updatePrintModal (ToggleImageBackdrop tid url) _total m =
  m & #contentSettings .~ modifyImageSetting tid url (\ips -> ips & #backdrop .~ not ips.backdrop) m.contentSettings

-- | Modify a task's content setting in the map
modifyTaskSetting :: TaskId -> (TaskContentSetting -> TaskContentSetting) -> ContentSettings -> ContentSettings
modifyTaskSetting tid f cs =
  let current = taskContentSetting cs tid
   in cs {perTask = Map.insert tid (f current) cs.perTask}

-- | Modify a single image's setting within a task
modifyImageSetting :: TaskId -> Text -> (ImagePrintSetting -> ImagePrintSetting) -> ContentSettings -> ContentSettings
modifyImageSetting tid url f =
  modifyTaskSetting tid $ \tcs ->
    let current = Map.findWithDefault defaultImagePrintSetting url tcs.imageSettings
     in tcs {imageSettings = Map.insert url (f current) tcs.imageSettings}

-- | Set custom footer on ContentSettings (avoids ambiguous field update)
setCustomFooter :: Maybe Text -> ContentSettings -> ContentSettings
setCustomFooter mf cs = ContentSettings
  { perTask = cs.perTask
  , showTitle = cs.showTitle
  , showNameField = cs.showNameField
  , customFooter = mf
  }

-- | Set points on TaskContentSetting (avoids ambiguous field update)
setPoints :: Maybe Double -> TaskContentSetting -> TaskContentSetting
setPoints mp tcs = TaskContentSetting
  { showDescription = tcs.showDescription
  , visibleSolutions = tcs.visibleSolutions
  , gridHeightMm = tcs.gridHeightMm
  , inlineAnswer = tcs.inlineAnswer
  , itemsPerRow = tcs.itemsPerRow
  , points = mp
  , imageSettings = tcs.imageSettings
  }

-- | Toggle a solution in/out of the visible set
toggleSolution :: SolutionId -> TaskContentSetting -> TaskContentSetting
toggleSolution sid tcs =
  let vs = tcs.visibleSolutions
   in if Set.member sid vs
        then tcs {visibleSolutions = Set.delete sid vs}
        else tcs {visibleSolutions = Set.insert sid vs}

-- | How a modal action affects page measurement
data RemeasurePolicy = Immediate | Debounced | NoRemeasure
  deriving (Eq, Show)

remeasurePolicy :: PrintModalAction -> RemeasurePolicy
remeasurePolicy (SetPaperSize _) = Immediate
remeasurePolicy (SetOrientation _) = Immediate
remeasurePolicy (SetFontSize _) = Immediate
remeasurePolicy (SetGroupedCopies _) = Immediate
remeasurePolicy (SetTotalCopies _) = Immediate
remeasurePolicy (SetTaskLayout _) = Immediate
remeasurePolicy (SetGridRows _) = Immediate
remeasurePolicy (SetGridCols _) = Immediate
remeasurePolicy (SetShowTitle _) = Immediate
remeasurePolicy (SetShowNameField _) = Immediate
remeasurePolicy (SetFontFamily _) = Immediate
remeasurePolicy (SetDistributeLastPage _) = Immediate
remeasurePolicy (ApplyPreset _) = Immediate
remeasurePolicy (ToggleDescription _) = Immediate
remeasurePolicy (ToggleSolution _ _) = Immediate
remeasurePolicy (ToggleGrid _) = Immediate
remeasurePolicy (SetGridHeight _ _) = Immediate
remeasurePolicy (ToggleInlineAnswer _) = Immediate
remeasurePolicy (SetItemsPerRow _ _) = Immediate
remeasurePolicy RemeasurePages = Immediate
remeasurePolicy (MoveTaskUp _) = Immediate
remeasurePolicy (MoveTaskDown _) = Immediate
remeasurePolicy (SetCustomFooter _) = Debounced
remeasurePolicy (SetPoints _ _) = Debounced
-- Image actions are Immediate so slider drag updates the preview without a
-- 500 ms debounce wait. Combined with NOT clearing pageGrouping (see
-- updatePrintModal above), rapid slider events naturally coalesce — the
-- last-arriving MeasuredPageGrouping wins.
remeasurePolicy (SetImageSize _ _ _) = Immediate
remeasurePolicy (SetImagePosition _ _ _) = Immediate
remeasurePolicy (ToggleImageBackdrop _ _) = Immediate
remeasurePolicy _ = NoRemeasure

-- | Extract grid config from settings, defaulting to 1x1
currentGridConfig :: PrintSettings -> GridConfig
currentGridConfig s = case s.taskLayout of
  Grid gc -> gc
  Continuous -> GridConfig {rows = 1, cols = 1}

clampGrid :: Int -> Int
clampGrid = max 1 . min 4

clampCopies :: Int -> Int
clampCopies = max 1 . min 10

-- | Swap the element matching the predicate with its predecessor
swapWithPrev :: (a -> Bool) -> [a] -> [a]
swapWithPrev _ [] = []
swapWithPrev _ [x] = [x]
swapWithPrev p (x : y : rest)
  | p y = y : x : rest
  | otherwise = x : swapWithPrev p (y : rest)

-- | Swap the element matching the predicate with its successor
swapWithNext :: (a -> Bool) -> [a] -> [a]
swapWithNext _ [] = []
swapWithNext _ [x] = [x]
swapWithNext p (x : y : rest)
  | p x = y : x : rest
  | otherwise = x : swapWithNext p (y : rest)

-- | Extract task IDs in the current order from the modal
reorderedTaskIds :: PrintModalModel -> [TaskId]
reorderedTaskIds m = map (.taskId) m.taskInfos

-- | Construct a ButtonConfig without going through ToAction
-- (avoids overlapping instances when action is polymorphic)
btn :: Button.ToButtonContents c => c -> Maybe action -> Button.ButtonConfig action
btn c a = Button.ButtonConfig
  { Button.contents = Button.toButtonContents c
  , Button.action = a
  }

-- | Render the print preview modal.
printModalView
  :: FormulaCache
  -> (Int -> M.View model action)
  -> Int
  -> MisoString
  -> MisoString
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
printModalView fc renderTask totalTasks title date model wrap =
  modalFrame modalConfig (wrap CancelPrint)
    [ Layout.vFlow Layout.hFull
        [ modalBody fc renderTask totalTasks title date model wrap
        , modalFooter wrap
        ]
    , printStyleView model.settings model.contentSettings
    ]
  where
    modalConfig = ModalConfig
      { chrome = WindowChrome ("Layout \x2014 " <> title) Icon.IcnPrint Nothing
      , modalId = ModalId "print-preview"
      , width = ModalWide
      , height = ModalFull
      , pinnable = Nothing
      }

-- | Modal body: sidebar with selectors + preview pane
modalBody
  :: FormulaCache
  -> (Int -> M.View model action)
  -> Int
  -> MisoString
  -> MisoString
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
modalBody fc renderTask totalTasks title date model wrap =
  Layout.hFlow
    Layout.hFull
    [ -- Left sidebar
      M.div_
        [class_ "w-1/3 border-r border-border"]
        [ Tabs.cardWithTabs Tabs.Tabs
            { tabs = [FormatTab, ContentsTab]
            , activeTab = model.activeTab
            , onSelect = wrap . SwitchTab
            , tabSpec = \case
                FormatTab -> Tabs.TabSpec (C.translate' C.LblFormat) False
                ContentsTab -> Tabs.TabSpec (C.translate' C.LblContents) False
            , tabContent = \case
                FormatTab -> [scrollableTabBody (formatTabContent model wrap)]
                ContentsTab -> [scrollableTabBody (contentsTabContent model wrap)]
            }
        ]
    , -- Right: preview pane with navigation pinned at bottom
      M.div_
        [class_ "w-2/3 p-4 bg-muted/30 overflow-hidden flex flex-col"]
        [ M.div_
            [class_ "flex-1 min-h-0 flex items-center justify-center"]
            [previewPane fc renderTask title date model]
        , M.div_
            [class_ "flex-shrink-0 relative flex justify-center py-2"]
            [ previewNavigation totalTasks model wrap
            , M.div_
                [class_ "absolute right-0 top-1/2 -translate-y-1/2"]
                [Button.ghostSm (btn ("\x21BB" :: MisoString) (Just (wrap RemeasurePages)))]
            ]
        ]
    ]
    & Layout.addClass "flex-1 min-h-0 overflow-hidden"

-- | Scrollable wrapper for tab content. Constrains height so the tab
-- header stays pinned while the body scrolls.
scrollableTabBody :: [M.View model action] -> M.View model action
scrollableTabBody content =
  M.div_
    [ class_ "overflow-y-auto space-y-4"
    , MC.style_ [("max-height", "calc(90vh - 13rem)")]
    ]
    content

-- | Group a label with its control (label above, control below)
field :: MisoString -> M.View model action -> M.View model action
field lbl ctrl =
  M.div_
    [class_ "flex flex-col gap-1"]
    [ Typography.fieldLabel lbl
    , ctrl
    ]

-- | Format tab content: layout controls only (no content decisions)
formatTabContent :: PrintModalModel -> (PrintModalAction -> action) -> [M.View model action]
formatTabContent model wrap =
  [ field (C.translate' C.LblPageSize) (paperSizeSelector model.settings.paperSize wrap)
  , field (C.translate' C.LblOrientation) (orientationSelector model.settings.orientation wrap)
  , field (C.translate' C.LblLayout) (layoutSelector model.settings.taskLayout wrap)
  , field (C.translate' C.LblTaskHeaderStyle) (taskHeaderStyleSelector model.settings.taskHeaderStyle wrap)
  ]
  <> gridSizeControls model.settings wrap
  <> [ field (C.translate' C.LblFontSize) (fontSizeInput model.settings.baseFontSize wrap)
     , field (C.translate' C.LblFontFamily) (fontFamilySelector model.settings.fontFamily wrap)
     , field (C.translate' C.LblGroupedCopies) (copiesInput model.settings.groupedCopies (\n -> wrap (SetGroupedCopies n)))
     , field (C.translate' C.LblTotalCopies) (copiesInput model.settings.totalCopies (\n -> wrap (SetTotalCopies n)))
     ]
  <> continuousOptions model.settings wrap

-- | Contents tab content: global content toggles, presets, custom footer, per-task settings
contentsTabContent :: PrintModalModel -> (PrintModalAction -> action) -> [M.View model action]
contentsTabContent model wrap =
  [ presetButtons model wrap
  , checkboxToggle (C.translate' C.LblShowTitle) model.contentSettings.showTitle (\b -> wrap (SetShowTitle b))
  , checkboxToggle (C.translate' C.LblShowNameField) model.contentSettings.showNameField (\b -> wrap (SetShowNameField b))
  , customFooterInput model.footerDraft wrap
  , reorderButton model.reorderMode wrap
  , renumberButton wrap
  ]
  <> concatMap (\(idx, ti) -> taskSection model.reorderMode idx taskCount model.contentSettings wrap ti) (zip [0 ..] model.taskInfos)
  where
    taskCount = length model.taskInfos

-- | Preset buttons in a 2x2 grid
presetButtons :: PrintModalModel -> (PrintModalAction -> action) -> M.View model action
presetButtons _model wrap =
  M.div_
    [class_ "grid grid-cols-2 gap-1"]
    [ presetButton C.LblPresetAufgabenblatt Aufgabenblatt wrap
    , presetButton C.LblPresetArbeitsblatt Arbeitsblatt wrap
    , presetButton C.LblPresetLoesungsblatt Loesungsblatt wrap
    , presetButton C.LblPresetMusteraufgaben Musteraufgaben wrap
    ]

-- | Reorder toggle button
reorderButton :: Bool -> (PrintModalAction -> action) -> M.View model action
reorderButton active wrap =
  Button.toggleSm active (btn (Icon.IcnReorder, C.translate' C.LblReorder) (Just (wrap ToggleReorderMode)))

-- | Renumber tasks button
renumberButton :: (PrintModalAction -> action) -> M.View model action
renumberButton wrap =
  Button.ghostSm (btn (Icon.IcnReorder, C.translate' C.LblRenumberTasks) (Just (wrap OpenRenumberModal)))

presetButton :: C.Label -> ContentPreset -> (PrintModalAction -> action) -> M.View model action
presetButton lbl preset wrap =
  M.button_
    [ class_ "px-2 py-1 text-xs rounded border border-border hover:bg-accent hover:text-accent-foreground transition-colors"
    , M.onClick (wrap (ApplyPreset preset))
    ]
    [M.text (C.translate' lbl)]

-- | Per-task section with toggles
taskSection :: Bool -> Int -> Int -> ContentSettings -> (PrintModalAction -> action) -> TaskInfo -> [M.View model action]
taskSection reorderActive idx total cs wrap ti =
  let tcs = taskContentSetting cs ti.taskId
      TaskIdentifier ident = ti.identifier
      displayName = let base = if T.null ident then "(Unbenannt)" else ident
                     in if T.null ti.title then base else base <> " \x2014 " <> ti.title
      reorderButtons
        | not reorderActive = []
        | otherwise =
            [ Button.ghostSm (btn Icon.IcnArrowUp (if idx > 0 then Just (wrap (MoveTaskUp ti.taskId)) else Nothing))
            , Button.ghostSm (btn Icon.IcnArrowDown (if idx < total - 1 then Just (wrap (MoveTaskDown ti.taskId)) else Nothing))
            ]
   in [ -- Section header with task identifier and optional reorder buttons
        M.div_
          [class_ "mt-3 pt-2 border-t border-border flex items-center gap-2"]
          ( [Typography.muted (ms displayName)]
              <> reorderButtons
          )
      , -- Description toggle
        checkboxToggle (C.translate' C.LblDescriptionToggle) tcs.showDescription (\_ -> wrap (ToggleDescription ti.taskId))
      ]
      -- Sub-options when description is on and task has letter-list items
      <> ( if tcs.showDescription && ti.hasLetterList
             then
               [ M.div_
                   [class_ "ml-6 space-y-1"]
                   [ itemsPerRowRow tcs wrap ti.taskId
                   , checkboxToggle (C.translate' C.LblInlineField) tcs.inlineAnswer (\_ -> wrap (ToggleInlineAnswer ti.taskId))
                   ]
               ]
             else []
         )
      -- Per-solution toggles
      <> [ solutionToggle cs wrap ti.taskId sid stype
         | (sid, stype) <- ti.solutionInfos
         ]
      -- Grid toggle with height input
      <> [ gridToggleRow tcs wrap ti.taskId ]
      -- Points input
      <> [ pointsInput tcs wrap ti.taskId ]
      -- Per-image print controls
      <> concatMap (imageSettingRow tcs wrap ti.taskId) ti.embedUrls

-- | Toggle for a specific solution
solutionToggle :: ContentSettings -> (PrintModalAction -> action) -> TaskId -> SolutionId -> SolutionType -> M.View model action
solutionToggle cs wrap tid sid stype =
  let tcs = taskContentSetting cs tid
      isOn = Set.member sid tcs.visibleSolutions
      lbl = C.translate' (C.LblSolutionType stype)
   in checkboxToggle lbl isOn (\_ -> wrap (ToggleSolution tid sid))

-- | Items per row control (1–4)
itemsPerRowRow :: TaskContentSetting -> (PrintModalAction -> action) -> TaskId -> M.View model action
itemsPerRowRow tcs wrap tid =
  M.div_
    [class_ "flex items-center gap-2"]
    [ M.span_ [class_ "text-xs text-muted-foreground"] [M.text (C.translate' C.LblItemsPerRow)]
    , M.input_
        [ MP.type_ "number"
        , MP.value_ (ms (show tcs.itemsPerRow))
        , M.onInput (\v -> wrap (SetItemsPerRow tid (parseIntOr tcs.itemsPerRow v)))
        , M.textProp "min" "1"
        , M.textProp "max" "4"
        , M.textProp "step" "1"
        , class_ "input w-14 h-6 text-xs px-1"
        ]
    ]

-- | Grid toggle with height input
gridToggleRow :: TaskContentSetting -> (PrintModalAction -> action) -> TaskId -> M.View model action
gridToggleRow tcs wrap tid =
  let gridOn = case tcs.gridHeightMm of Just _ -> True; Nothing -> False
   in M.div_
        [class_ "space-y-1"]
        [ checkboxToggle (C.translate' C.LblAnswerGrid) gridOn (\_ -> wrap (ToggleGrid tid))
        , case tcs.gridHeightMm of
            Just h ->
              M.div_
                [class_ "ml-6 flex items-center gap-1"]
                [ M.input_
                    [ MP.type_ "number"
                    , MP.value_ (ms (show (round h :: Int)))
                    , M.onInput (\v -> wrap (SetGridHeight tid (parseDoubleOr h v)))
                    , M.textProp "min" "5"
                    , M.textProp "max" "200"
                    , M.textProp "step" "5"
                    , class_ "input w-16 h-6 text-xs px-1"
                    ]
                , M.span_ [class_ "text-xs text-muted-foreground"] [M.text "mm"]
                ]
            Nothing -> M.text ""
        ]

-- | Points input for a task
pointsInput :: TaskContentSetting -> (PrintModalAction -> action) -> TaskId -> M.View model action
pointsInput tcs wrap tid =
  M.div_
    [class_ "flex items-center gap-2"]
    [ M.span_ [class_ "text-xs text-muted-foreground"] [M.text (C.translate' C.LblPoints)]
    , M.input_
        [ MP.type_ "number"
        , MP.value_ (ms (maybe "" show tcs.points))
        , M.onInput (\v -> wrap (SetPoints tid (parseOptionalDouble v)))
        , M.textProp "min" "0"
        , M.textProp "step" "0.5"
        , MP.placeholder_ ""
        , class_ "input w-16 h-6 text-xs px-1"
        ]
    ]

-- | Per-image print settings: size slider, position dropdown, backdrop toggle
imageSettingRow :: TaskContentSetting -> (PrintModalAction -> action) -> TaskId -> Text -> [M.View model action]
imageSettingRow tcs wrap tid url =
  let ips = Map.findWithDefault defaultImagePrintSetting url tcs.imageSettings
      displayName = T.takeWhileEnd (/= ':') url
   in [ M.div_
          [class_ "ml-4 mt-1 space-y-1 border-l-2 border-border pl-2"]
          [ M.div_
              [class_ "text-xs font-medium text-muted-foreground truncate"]
              [M.text (ms displayName)]
          , M.div_
              [class_ "flex items-center gap-2"]
              [ M.span_ [class_ "text-xs text-muted-foreground w-10"] [M.text (ms (show ips.sizePct <> "%"))]
              , M.input_
                  [ MP.type_ "range"
                  , MP.value_ (ms (show ips.sizePct))
                  , M.onInput (\v -> wrap (SetImageSize tid url (parseIntOr ips.sizePct v)))
                  , M.textProp "min" "10"
                  , M.textProp "max" "100"
                  , M.textProp "step" "5"
                  , class_ "flex-1 h-4"
                  ]
              ]
          , M.div_
              [class_ "flex items-center gap-2"]
              [ M.select_
                  [ M.onChange (\v -> wrap (SetImagePosition tid url (parsePosition v)))
                  , class_ "input h-6 text-xs px-1"
                  ]
                  [ M.option_ (posAttrs "inline" PrintInline ips.position) [M.text "Zentriert"]
                  , M.option_ (posAttrs "floatRight" PrintFloatRight ips.position) [M.text "Rechts"]
                  , M.option_ (posAttrs "floatTop" PrintFloatTop ips.position) [M.text "Oben rechts"]
                  ]
              , checkboxToggle "Hintergrund" ips.backdrop (\_ -> wrap (ToggleImageBackdrop tid url))
              ]
          ]
      ]
  where
    posAttrs val pos current = [MP.value_ val, MP.selected_ (current == pos)]

parsePosition :: MisoString -> PrintImagePosition
parsePosition v = case fromMisoString v :: [Char] of
  "floatRight" -> PrintFloatRight
  "floatTop" -> PrintFloatTop
  _ -> PrintInline

-- | Parse an optional double from input (empty string = Nothing)
parseOptionalDouble :: MisoString -> Maybe Double
parseOptionalDouble v =
  let s = fromMisoString v
   in if null s then Nothing else readMaybe s

-- | Font family selector
fontFamilySelector :: FontFamily -> (PrintModalAction -> action) -> M.View model action
fontFamilySelector current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == ff) (btn (fontFamilyLabel ff) (Just (wrap (SetFontFamily ff))))
    | ff <- [minBound .. maxBound]
    ]

fontFamilyLabel :: FontFamily -> MisoString
fontFamilyLabel DefaultFont = C.translate' C.LblFontDefault
fontFamilyLabel IwonaFont = C.translate' C.LblFontIwona

-- | Custom footer textarea input
customFooterInput :: Maybe Text -> (PrintModalAction -> action) -> M.View model action
customFooterInput mFooter wrap =
  M.div_
    [class_ "space-y-1"]
    [ Typography.fieldLabel (C.translate' C.LblCustomFooter)
    , M.textarea_
        [ MP.value_ (ms (maybe "" id mFooter))
        , M.onInput (\v -> wrap (SetCustomFooter (parseOptionalText v)))
        , MP.placeholder_ (C.translate' C.LblCustomFooterPlaceholder)
        , class_ "input w-full h-16 text-xs px-2 py-1 resize-y"
        ]
        []
    ]

-- | Parse optional text (empty = Nothing)
parseOptionalText :: MisoString -> Maybe Text
parseOptionalText v =
  let s = fromMisoString v
   in if T.null s then Nothing else Just s

-- | Modal footer with cancel, save, and print & save buttons
modalFooter :: (PrintModalAction -> action) -> M.View model action
modalFooter wrap =
  Layout.shrink0 $
    Layout.actionFooter
      [ Button.secondary (btn (Icon.IcnCancel, C.LblCancel) (Just (wrap CancelPrint)))
      , Button.secondary (btn (C.translate' C.LblSave) (Just (wrap SaveLayout)))
      , Button.primary (btn (Icon.IcnPrint, C.LblPrint) (Just (wrap PrintAndSaveLayout)))
      ]

-- | Paper size selector (A4 / A5)
paperSizeSelector :: PaperSize -> (PrintModalAction -> action) -> M.View model action
paperSizeSelector current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == size) (btn (paperSizeLabel size) (Just (wrap (SetPaperSize size))))
    | size <- [minBound .. maxBound]
    ]

paperSizeLabel :: PaperSize -> MisoString
paperSizeLabel A4 = "A4"
paperSizeLabel A5 = "A5"

-- | Orientation selector (Portrait / Landscape)
orientationSelector :: Orientation -> (PrintModalAction -> action) -> M.View model action
orientationSelector current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == o) (btn (orientationLabel o) (Just (wrap (SetOrientation o))))
    | o <- [minBound .. maxBound]
    ]

orientationLabel :: Orientation -> MisoString
orientationLabel Portrait = C.translate' C.LblPortrait
orientationLabel Landscape = C.translate' C.LblLandscape

-- | Layout selector (Continuous / Grid)
layoutSelector :: TaskLayout -> (PrintModalAction -> action) -> M.View model action
layoutSelector current wrap =
  let isContinuous = case current of { Continuous -> True; Grid _ -> False }
   in Button.buttonGroup
        [ Button.toggleSm isContinuous (btn (C.translate' C.LblContinuous) (Just (wrap (SetTaskLayout Continuous))))
        , Button.toggleSm (not isContinuous) (btn (C.translate' C.LblGrid) (Just (wrap (SetTaskLayout (Grid (GridConfig {rows = 1, cols = 1}))))))
        ]

-- | Task header style selector (Number / Title / Both)
taskHeaderStyleSelector :: TaskHeaderStyle -> (PrintModalAction -> action) -> M.View model action
taskHeaderStyleSelector current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == s) (btn (headerStyleLabel s) (Just (wrap (SetTaskHeaderStyle s))))
    | s <- [minBound .. maxBound]
    ]

headerStyleLabel :: TaskHeaderStyle -> MisoString
headerStyleLabel HeaderNumber = C.translate' C.LblHeaderNumber
headerStyleLabel HeaderTitle = C.translate' C.LblHeaderTitle
headerStyleLabel HeaderBoth = C.translate' C.LblHeaderBoth

-- | Grid size controls (rows and cols) — only shown when Grid is selected
gridSizeControls :: PrintSettings -> (PrintModalAction -> action) -> [M.View model action]
gridSizeControls settings wrap = case settings.taskLayout of
  Continuous -> []
  Grid gc ->
    [ field (C.translate' C.LblRows) (gridNumberInput gc.rows (\n -> wrap (SetGridRows n)))
    , field (C.translate' C.LblColumns) (gridNumberInput gc.cols (\n -> wrap (SetGridCols n)))
    ]

-- | Continuous-only options: header, footer, duplex, distribute last page toggles
continuousOptions :: PrintSettings -> (PrintModalAction -> action) -> [M.View model action]
continuousOptions settings wrap = case settings.taskLayout of
  Continuous ->
    [ checkboxToggle (C.translate' C.LblShowHeader) settings.showHeader (\b -> wrap (SetShowHeader b))
    , checkboxToggle (C.translate' C.LblShowFooter) settings.showFooter (\b -> wrap (SetShowFooter b))
    , checkboxToggle (C.translate' C.LblDuplexLayout) settings.duplexLayout (\b -> wrap (SetDuplexLayout b))
    , checkboxToggle (C.translate' C.LblDistributeLastPage) settings.distributeLastPage (\b -> wrap (SetDistributeLastPage b))
    ]
  Grid _ -> []

-- | Basecoat-styled switch toggle with integrated label
checkboxToggle :: MisoString -> Bool -> (Bool -> action) -> M.View model action
checkboxToggle labelText current toAction =
  M.div_
    [class_ "field"]
    [ M.label_
        [class_ "flex items-center gap-2 text-sm font-medium select-none cursor-pointer"]
        [ M.input_
            [ MP.type_ "checkbox"
            , M.textProp "role" "switch"
            , MP.checked_ current
            , M.onClick (toAction (not current))
            , class_ "input"
            ]
        , M.text labelText
        ]
    ]

-- | Number input for grid dimensions (1–4)
gridNumberInput :: Int -> (Int -> action) -> M.View model action
gridNumberInput current toAction =
  Input.renderInput
    $ Input.withOnInput (\v -> toAction (parseIntOr current v))
    $ Input.withValue (ms (show current))
    $ Input.defaultInput
      { Input.inputType = "number"
      , Input.attrs =
          [ M.textProp "min" "1"
          , M.textProp "max" "4"
          , M.textProp "step" "1"
          ]
      }

-- | Number input for copies (1–10)
copiesInput :: Int -> (Int -> action) -> M.View model action
copiesInput current toAction =
  Input.renderInput
    $ Input.withOnInput (\v -> toAction (parseIntOr current v))
    $ Input.withValue (ms (show current))
    $ Input.defaultInput
      { Input.inputType = "number"
      , Input.attrs =
          [ M.textProp "min" "1"
          , M.textProp "max" "10"
          , M.textProp "step" "1"
          ]
      }

-- | Parse integer from input, defaulting to given value
parseIntOr :: Int -> MisoString -> Int
parseIntOr def v = case readMaybe (fromMisoString v) of
  Just n -> n
  Nothing -> def

-- | Parse double from input, defaulting to given value
parseDoubleOr :: Double -> MisoString -> Double
parseDoubleOr def v = case readMaybe (fromMisoString v) of
  Just d -> d
  Nothing -> def

-- | Preview navigation: previous / "Task 1 / N" or "Page 1 / N" / next
previewNavigation :: Int -> PrintModalModel -> (PrintModalAction -> action) -> M.View model action
previewNavigation totalTasks model wrap =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ Button.ghostSm (btn ("\x2039" :: MisoString) prevAction)
    , Typography.muted $
        navigationLabel model.settings model.previewTaskIndex navTotal
    , Button.ghostSm (btn ("\x203A" :: MisoString) nextAction)
    ]
  where
    navTotal = navigationTotal model.settings model.pageGrouping totalTasks
    prevAction
      | model.previewTaskIndex <= 0 = Nothing
      | otherwise = Just (wrap PreviewPrev)
    nextAction
      | model.previewTaskIndex >= navTotal - 1 = Nothing
      | otherwise = Just (wrap PreviewNext)

-- | Total number of navigable items (pages for continuous with grouping, pages for grid)
navigationTotal :: PrintSettings -> PageGrouping -> Int -> Int
navigationTotal settings pageGrp expandedCount = case settings.taskLayout of
  Continuous
    | not (null pageGrp) -> length pageGrp
    | otherwise -> max 1 expandedCount
  Grid gc ->
    let cpp = cellsPerPage gc
     in if expandedCount <= 0 then 1 else (expandedCount + cpp - 1) `div` cpp

-- | Navigation label ("Task 1 / N" or "Seite 1 / N")
navigationLabel :: PrintSettings -> Int -> Int -> MisoString
navigationLabel settings idx total = case settings.taskLayout of
  Continuous ->
    ms (show (idx + 1)) <> " / " <> ms (show total)
  Grid _ ->
    ms (show (idx + 1)) <> " / " <> ms (show total)

-- | Font size number input
fontSizeInput :: Double -> (PrintModalAction -> action) -> M.View model action
fontSizeInput current wrap =
  Input.renderInput
    $ Input.withOnInput (\v -> wrap (SetFontSize (parseFontSize v)))
    $ Input.withValue (ms (show current))
    $ Input.defaultInput
      { Input.inputType = "number"
      , Input.attrs =
          [ M.textProp "min" "6"
          , M.textProp "max" "20"
          , M.textProp "step" "0.5"
          ]
      }

-- | Parse font size from input, defaulting to 11.0
parseFontSize :: MisoString -> Double
parseFontSize v = case readMaybe (fromMisoString v) of
  Just fs -> fs
  Nothing -> 11.0

-- | Preview pane: renders based on layout mode
previewPane :: FormulaCache -> (Int -> M.View model action) -> MisoString -> MisoString -> PrintModalModel -> M.View model action
previewPane fc renderTask title date model = case model.settings.taskLayout of
  Continuous -> continuousPreview fc renderTask title date model
  Grid gc -> gridPreview renderTask model gc

-- | Continuous preview: renders all tasks for the current page using
-- the shared Page.renderContinuousPage (mm-based), wrapped in a CSS
-- scale transform to fit the preview pane.
continuousPreview :: FormulaCache -> (Int -> M.View model action) -> MisoString -> MisoString -> PrintModalModel -> M.View model action
continuousPreview fc renderTask title date model =
  let settings = model.settings
      cs = model.contentSettings
      (wMm, hMm) = pageSizeMm settings.paperSize settings.orientation
      -- Browser renders mm at 96 DPI on screen
      mmToPx mm = mm * 96.0 / 25.4
      pageWPx = mmToPx wMm
      pageHPx = mmToPx hMm
      -- Scale to fit available space (both width and height constrained)
      previewMaxW = 600.0 :: Double
      previewMaxH = 700.0 :: Double
      scaleW = previewMaxW / pageWPx
      scaleH = previewMaxH / pageHPx
      scaleFactor = min scaleW scaleH
      scaledW = pageWPx * scaleFactor
      scaledH = pageHPx * scaleFactor
      -- Get current page group (or synthesize one for unmeasured state)
      currentPage = case model.pageGrouping of
        [] -> PageGroup {indices = [model.previewTaskIndex], gapPx = 0}
        pgs -> case drop model.previewTaskIndex pgs of
          [] -> emptyPageGroup
          (pg : _) -> pg
      totalPages = case model.pageGrouping of
        [] -> 1
        pgs -> length pgs
      -- Real custom footer in preview
      customFooterPreview = case cs.customFooter of
        Just footer ->
          Just (Footer.renderCustomFooter fc footer cs (map (.taskId) model.taskInfos))
        Nothing -> Nothing
      -- Task render callback
      renderFn idx = M.div_ [class_ "print-task"] [renderTask idx]
      -- Render the mm-based page
      pageView =
        Page.renderContinuousPage
          settings
          cs
          title
          date
          totalPages
          renderFn
          customFooterPreview
          model.previewTaskIndex
          currentPage
   in M.div_
        [ MC.style_
            [ ("width", Page.showPx scaledW)
            , ("height", Page.showPx scaledH)
            , ("overflow", "hidden")
            ]
        , class_ "rounded shadow-md page-print-content"
        ]
        [ M.div_
            [ MC.style_
                [ ("transform", ms $ "scale(" <> T.pack (show scaleFactor) <> ")")
                , ("transform-origin", "top left")
                ]
            , class_ "bg-white text-black"
            ]
            [pageView]
        ]

-- | Grid preview: one page with CSS grid cells, using the shared
-- Page.renderGridPage (mm-based), wrapped in a CSS scale transform.
gridPreview :: (Int -> M.View model action) -> PrintModalModel -> GridConfig -> M.View model action
gridPreview renderTask model gc =
  let (wMm, hMm) = pageSizeMm model.settings.paperSize model.settings.orientation
      mmToPx mm = mm * 96.0 / 25.4
      pageWPx = mmToPx wMm
      pageHPx = mmToPx hMm
      previewMaxW = 600.0 :: Double
      previewMaxH = 700.0 :: Double
      scaleW = previewMaxW / pageWPx
      scaleH = previewMaxH / pageHPx
      scaleFactor = min scaleW scaleH
      scaledW = pageWPx * scaleFactor
      scaledH = pageHPx * scaleFactor
      cpp = cellsPerPage gc
      pageStart = model.previewTaskIndex * cpp
      taskIndices = [pageStart .. pageStart + cpp - 1]
      renderFn idx = M.div_ [class_ "print-cell"] [renderTask idx]
      gridPage = Page.renderGridPage model.settings.paperSize model.settings.orientation gc renderFn taskIndices
   in M.div_
        [ MC.style_
            [ ("width", Page.showPx scaledW)
            , ("height", Page.showPx scaledH)
            , ("overflow", "hidden")
            ]
        , class_ "rounded shadow-md"
        ]
        [ M.div_
            [ MC.style_
                [ ("transform", ms $ "scale(" <> T.pack (show scaleFactor) <> ")")
                , ("transform-origin", "top left")
                ]
            , class_ "bg-white text-black page-print-content"
            ]
            [gridPage]
        ]

-- | Off-screen measurement container for DOM-based page grouping.
-- Renders each task as a bare child div (no spacing classes) so that
-- getBoundingClientRect returns pure content height.  The grouping
-- algorithm adds gaps separately.
measurementContainer
  :: (Int -> M.View model action)
  -> Int
  -> PrintModalModel
  -> M.View model action
measurementContainer renderTask taskCount model =
  let (wMm, _hMm) = pageSizeMm model.settings.paperSize model.settings.orientation
      margin = pageMarginMm model.settings.paperSize
      mmToPx mm = mm * 96.0 / 25.4
      contentWPx = mmToPx (wMm - 2.0 * margin)
   in M.div_
        [ MC.style_
            [ ("position", "absolute")
            , ("left", "-9999px")
            , ("top", "0")
            , ("visibility", "hidden")
            , ("width", Page.showPx contentWPx)
            ]
        , M.textProp "id" "print-measure-container"
        , class_ "page-print-content"
        ]
        [ M.div_ [class_ "print-task"] [renderTask idx]
        | idx <- [0 .. taskCount - 1]
        ]

-- | Off-screen measurement container for the custom footer.
-- Renders the footer at the same width as the task measurement container
-- so that getBoundingClientRect returns the correct height.
footerMeasureContainer :: FormulaCache -> PrintModalModel -> M.View model action
footerMeasureContainer fc model =
  let cs = model.contentSettings
      (wMm, _hMm) = pageSizeMm model.settings.paperSize model.settings.orientation
      margin = pageMarginMm model.settings.paperSize
      mmToPx mm = mm * 96.0 / 25.4
      contentWPx = mmToPx (wMm - 2.0 * margin)
   in case cs.customFooter of
        Nothing -> M.text ""
        Just footer ->
          M.div_
            [ MC.style_
                [ ("position", "absolute")
                , ("left", "-9999px")
                , ("top", "0")
                , ("visibility", "hidden")
                , ("width", Page.showPx contentWPx)
                ]
            , M.textProp "id" "print-footer-measure"
            , class_ "page-print-content"
            ]
            [Footer.renderCustomFooter fc footer cs (map (.taskId) model.taskInfos)]

