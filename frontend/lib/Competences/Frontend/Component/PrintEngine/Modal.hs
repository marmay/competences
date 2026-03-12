module Competences.Frontend.Component.PrintEngine.Modal
  ( PrintModalModel (..)
  , PrintModalAction (..)
  , initPrintModalModel
  , updatePrintModal
  , printModalView
  , measurementContainer
  , needsRemeasure
  , renderFirstPageHeader
  , renderCompactHeader
  , renderPageFooter
  , renderNameField
  )
where

import Competences.Document.Solution (SolutionId, SolutionType (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.CSS (printStyleView)
import Competences.Frontend.Component.PrintEngine.Measure (PageGroup (..), PageGrouping)
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentPreset (..)
  , ContentSettings (..)
  , GridConfig (..)
  , Orientation (..)
  , PaperSize (..)
  , PrintSettings (..)
  , PrintTab (..)
  , TaskContentSetting (..)
  , TaskHeaderStyle (..)
  , TaskInfo (..)
  , TaskLayout (..)
  , applyPreset
  , cellsPerPage
  , defaultGridHeightMm
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
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.View.WindowFrame (modalFrame)
import Data.Function ((&))
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
  { settings :: !PrintSettings
  , contentSettings :: !ContentSettings
  , taskInfos :: ![TaskInfo]
  , activeTab :: !PrintTab
  , previewTaskIndex :: !Int
  , pageGrouping :: !PageGrouping
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
  | MeasuredPageGrouping !PageGrouping
  | PreviewNext
  | PreviewPrev
  | ConfirmPrint
  | CancelPrint
  | SwitchTab !PrintTab
  | ApplyPreset !ContentPreset
  | ToggleDescription !TaskId
  | ToggleSolution !TaskId !SolutionId
  | ToggleGrid !TaskId
  | SetGridHeight !TaskId !Double
  | ToggleInlineAnswer !TaskId
  | SetItemsPerRow !TaskId !Int
  deriving (Eq, Show)

-- | Initialize modal with task infos, applying Aufgabenblatt preset
initPrintModalModel :: [TaskInfo] -> PrintModalModel
initPrintModalModel infos = PrintModalModel
  { settings = defaultPrintSettings
  , contentSettings = applyPreset Aufgabenblatt infos
  , taskInfos = infos
  , activeTab = FormatTab
  , previewTaskIndex = 0
  , pageGrouping = []
  }

-- | Pure update for the modal model.
-- 'total' is the number of navigable items (expanded tasks for continuous, pages for grid).
updatePrintModal :: PrintModalAction -> Int -> PrintModalModel -> PrintModalModel
updatePrintModal (SetPaperSize ps) _total m =
  m {settings = m.settings {paperSize = ps}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetOrientation o) _total m =
  m {settings = m.settings {orientation = o}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetFontSize fs) _total m =
  m {settings = m.settings {baseFontSize = max 6.0 (min 20.0 fs)}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetTaskLayout tl) _total m =
  m {settings = m.settings {taskLayout = tl}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetGridRows r) _total m =
  let gc = currentGridConfig m.settings
      gc' = gc {rows = clampGrid r}
   in m {settings = m.settings {taskLayout = Grid gc'}, previewTaskIndex = 0}
updatePrintModal (SetGridCols c) _total m =
  let gc = currentGridConfig m.settings
      gc' = gc {cols = clampGrid c}
   in m {settings = m.settings {taskLayout = Grid gc'}, previewTaskIndex = 0}
updatePrintModal (SetGroupedCopies n) _total m =
  m {settings = m.settings {groupedCopies = clampCopies n}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetTotalCopies n) _total m =
  m {settings = m.settings {totalCopies = clampCopies n}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetShowTitle b) _total m =
  m {settings = m.settings {showTitle = b}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetShowHeader b) _total m =
  m {settings = m.settings {showHeader = b}}
updatePrintModal (SetShowFooter b) _total m =
  m {settings = m.settings {showFooter = b}}
updatePrintModal (SetShowNameField b) _total m =
  m {settings = m.settings {showNameField = b}, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (SetTaskHeaderStyle s) _total m =
  m {settings = m.settings {taskHeaderStyle = s}}
updatePrintModal (MeasuredPageGrouping pg) _total m =
  m {pageGrouping = pg, previewTaskIndex = 0}
updatePrintModal PreviewNext total m =
  m {previewTaskIndex = min (total - 1) (m.previewTaskIndex + 1)}
updatePrintModal PreviewPrev _total m =
  m {previewTaskIndex = max 0 (m.previewTaskIndex - 1)}
updatePrintModal ConfirmPrint _total m = m
updatePrintModal CancelPrint _total m = m
updatePrintModal (SwitchTab tab) _total m =
  m {activeTab = tab}
updatePrintModal (ApplyPreset preset) _total m =
  m {contentSettings = applyPreset preset m.taskInfos, pageGrouping = [], previewTaskIndex = 0}
updatePrintModal (ToggleDescription tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {showDescription = not tcs.showDescription}) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }
updatePrintModal (ToggleSolution tid sid) _total m =
  m { contentSettings = modifyTaskSetting tid (toggleSolution sid) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }
updatePrintModal (ToggleGrid tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {gridHeightMm = case tcs.gridHeightMm of Nothing -> Just defaultGridHeightMm; Just _ -> Nothing}) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }
updatePrintModal (SetGridHeight tid h) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {gridHeightMm = Just (max 5.0 (min 200.0 h))}) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }
updatePrintModal (ToggleInlineAnswer tid) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {inlineAnswer = not tcs.inlineAnswer}) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }
updatePrintModal (SetItemsPerRow tid n) _total m =
  m { contentSettings = modifyTaskSetting tid (\tcs -> tcs {itemsPerRow = max 1 (min 4 n)}) m.contentSettings
    , pageGrouping = []
    , previewTaskIndex = 0
    }

-- | Modify a task's content setting in the map
modifyTaskSetting :: TaskId -> (TaskContentSetting -> TaskContentSetting) -> ContentSettings -> ContentSettings
modifyTaskSetting tid f cs =
  let current = taskContentSetting cs tid
   in cs {perTask = Map.insert tid (f current) cs.perTask}

-- | Toggle a solution in/out of the visible set
toggleSolution :: SolutionId -> TaskContentSetting -> TaskContentSetting
toggleSolution sid tcs =
  let vs = tcs.visibleSolutions
   in if Set.member sid vs
        then tcs {visibleSolutions = Set.delete sid vs}
        else tcs {visibleSolutions = Set.insert sid vs}

-- | Whether a modal action requires re-measurement of task heights
needsRemeasure :: PrintModalAction -> Bool
needsRemeasure (SetPaperSize _) = True
needsRemeasure (SetOrientation _) = True
needsRemeasure (SetFontSize _) = True
needsRemeasure (SetGroupedCopies _) = True
needsRemeasure (SetTotalCopies _) = True
needsRemeasure (SetTaskLayout _) = True
needsRemeasure (SetShowTitle _) = True
needsRemeasure (SetShowNameField _) = True
needsRemeasure (ApplyPreset _) = True
needsRemeasure (ToggleDescription _) = True
needsRemeasure (ToggleSolution _ _) = True
needsRemeasure (ToggleGrid _) = True
needsRemeasure (SetGridHeight _ _) = True
needsRemeasure (ToggleInlineAnswer _) = True
needsRemeasure (SetItemsPerRow _ _) = True
needsRemeasure _ = False

-- | Extract grid config from settings, defaulting to 1x1
currentGridConfig :: PrintSettings -> GridConfig
currentGridConfig s = case s.taskLayout of
  Grid gc -> gc
  Continuous -> GridConfig {rows = 1, cols = 1}

clampGrid :: Int -> Int
clampGrid = max 1 . min 4

clampCopies :: Int -> Int
clampCopies = max 1 . min 10

-- | Construct a ButtonConfig without going through ToAction
-- (avoids overlapping instances when action is polymorphic)
btn :: Button.ToButtonContents c => c -> Maybe action -> Button.ButtonConfig action
btn c a = Button.ButtonConfig
  { Button.contents = Button.toButtonContents c
  , Button.action = a
  }

-- | Render the print preview modal.
printModalView
  :: (Int -> M.View model action)
  -> Int
  -> MisoString
  -> MisoString
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
printModalView renderTask totalTasks title date model wrap =
  modalFrame modalConfig (wrap CancelPrint)
    [ Layout.vFlow Layout.hFull
        [ modalBody renderTask totalTasks title date model wrap
        , modalFooter wrap
        ]
    , printStyleView model.settings
    ]
  where
    modalConfig = ModalConfig
      { chrome = WindowChrome (C.translate' C.LblPrintPreview) Icon.IcnPrint
      , modalId = ModalId "print-preview"
      , width = ModalWide
      , height = ModalFull
      , pinnable = Nothing
      }

-- | Modal body: sidebar with selectors + preview pane
modalBody
  :: (Int -> M.View model action)
  -> Int
  -> MisoString
  -> MisoString
  -> PrintModalModel
  -> (PrintModalAction -> action)
  -> M.View model action
modalBody renderTask totalTasks title date model wrap =
  Layout.hFlow
    Layout.hFull
    [ -- Left sidebar
      M.div_
        [class_ "w-1/3 border-r border-border p-4 overflow-y-auto overflow-x-hidden"]
        [ Layout.vFlow
            Layout.gapM
            ( tabSwitcher model.activeTab wrap
              : case model.activeTab of
                  FormatTab -> formatTabContent model wrap
                  ContentsTab -> contentsTabContent model wrap
            )
        ]
    , -- Right: preview pane with navigation
      M.div_
        [class_ "w-2/3 p-6 bg-muted/30 overflow-hidden"]
        [ Layout.vFlow
            (Layout.gapM <> Layout.crossCenter <> Layout.mainCenter)
            [ previewPane renderTask title date model
            , previewNavigation totalTasks model wrap
            ]
        ]
    ]
    & Layout.addClass "flex-1 min-h-0 overflow-hidden"

-- | Tab switcher at top of sidebar
tabSwitcher :: PrintTab -> (PrintModalAction -> action) -> M.View model action
tabSwitcher current wrap =
  Button.buttonGroup
    [ Button.toggleSm (current == FormatTab) (btn (C.translate' C.LblFormat) (Just (wrap (SwitchTab FormatTab))))
    , Button.toggleSm (current == ContentsTab) (btn (C.translate' C.LblContents) (Just (wrap (SwitchTab ContentsTab))))
    ]

-- | Format tab content (current sidebar, unchanged)
formatTabContent :: PrintModalModel -> (PrintModalAction -> action) -> [M.View model action]
formatTabContent model wrap =
  [ Typography.fieldLabel (C.translate' C.LblPageSize)
  , paperSizeSelector model.settings.paperSize wrap
  , Typography.fieldLabel (C.translate' C.LblOrientation)
  , orientationSelector model.settings.orientation wrap
  , Typography.fieldLabel (C.translate' C.LblLayout)
  , layoutSelector model.settings.taskLayout wrap
  , Typography.fieldLabel (C.translate' C.LblTaskHeaderStyle)
  , taskHeaderStyleSelector model.settings.taskHeaderStyle wrap
  ]
  <> gridSizeControls model.settings wrap
  <> [ Typography.fieldLabel (C.translate' C.LblFontSize)
     , fontSizeInput model.settings.baseFontSize wrap
     , Typography.fieldLabel (C.translate' C.LblGroupedCopies)
     , copiesInput model.settings.groupedCopies (\n -> wrap (SetGroupedCopies n))
     , Typography.fieldLabel (C.translate' C.LblTotalCopies)
     , copiesInput model.settings.totalCopies (\n -> wrap (SetTotalCopies n))
     ]
  <> continuousOptions model.settings wrap

-- | Contents tab content: presets + per-task toggles
contentsTabContent :: PrintModalModel -> (PrintModalAction -> action) -> [M.View model action]
contentsTabContent model wrap =
  [ presetButtons model wrap
  ]
  <> concatMap (taskSection model.contentSettings wrap) model.taskInfos

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

presetButton :: C.Label -> ContentPreset -> (PrintModalAction -> action) -> M.View model action
presetButton lbl preset wrap =
  M.button_
    [ class_ "px-2 py-1 text-xs rounded border border-border hover:bg-accent hover:text-accent-foreground transition-colors"
    , M.onClick (wrap (ApplyPreset preset))
    ]
    [M.text (C.translate' lbl)]

-- | Per-task section with toggles
taskSection :: ContentSettings -> (PrintModalAction -> action) -> TaskInfo -> [M.View model action]
taskSection cs wrap ti =
  let tcs = taskContentSetting cs ti.taskId
      TaskIdentifier ident = ti.identifier
      displayName = let base = if T.null ident then "(Unbenannt)" else ident
                     in if T.null ti.title then base else base <> " \x2014 " <> ti.title
   in [ -- Section header with task identifier
        M.div_
          [class_ "mt-3 pt-2 border-t border-border"]
          [ Typography.muted (ms displayName)
          ]
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

-- | Modal footer with cancel and print buttons
modalFooter :: (PrintModalAction -> action) -> M.View model action
modalFooter wrap =
  Layout.shrink0 $
    Layout.actionFooter
      [ Button.secondary (btn (Icon.IcnCancel, C.LblCancel) (Just (wrap CancelPrint)))
      , Button.primary (btn (Icon.IcnPrint, C.LblPrint) (Just (wrap ConfirmPrint)))
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
    [ Typography.fieldLabel (C.translate' C.LblRows)
    , gridNumberInput gc.rows (\n -> wrap (SetGridRows n))
    , Typography.fieldLabel (C.translate' C.LblColumns)
    , gridNumberInput gc.cols (\n -> wrap (SetGridCols n))
    ]

-- | Continuous-only options: header, footer, name field toggles
continuousOptions :: PrintSettings -> (PrintModalAction -> action) -> [M.View model action]
continuousOptions settings wrap = case settings.taskLayout of
  Continuous ->
    [ checkboxToggle (C.translate' C.LblShowTitle) settings.showTitle (\b -> wrap (SetShowTitle b))
    , checkboxToggle (C.translate' C.LblShowHeader) settings.showHeader (\b -> wrap (SetShowHeader b))
    , checkboxToggle (C.translate' C.LblShowFooter) settings.showFooter (\b -> wrap (SetShowFooter b))
    , checkboxToggle (C.translate' C.LblShowNameField) settings.showNameField (\b -> wrap (SetShowNameField b))
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
previewPane :: (Int -> M.View model action) -> MisoString -> MisoString -> PrintModalModel -> M.View model action
previewPane renderTask title date model = case model.settings.taskLayout of
  Continuous -> continuousPreview renderTask title date model
  Grid gc -> gridPreview renderTask model gc

-- | Continuous preview: renders all tasks for the current page (based on page grouping)
-- Uses the same 3-section layout as real print: margin-top (header),
-- content-area (name field + tasks), margin-bottom (footer).
continuousPreview :: (Int -> M.View model action) -> MisoString -> MisoString -> PrintModalModel -> M.View model action
continuousPreview renderTask title date model =
  let settings = model.settings
      (wMm, hMm) = pageSizeMm settings.paperSize settings.orientation
      margin = pageMarginMm settings.paperSize
      -- Convert mm to px at 96 DPI (1 inch = 25.4mm)
      mmToPx mm = mm * 96.0 / 25.4
      pageWPx = mmToPx wMm
      pageHPx = mmToPx hMm
      marginPx = mmToPx margin
      -- Scale to fit available preview width (~440px considering modal padding)
      previewMaxW = 440.0 :: Double
      scaleFactor = previewMaxW / pageWPx
      scaledW = pageWPx * scaleFactor
      scaledH = pageHPx * scaleFactor
      -- Get current page from page grouping
      isFirstPage = model.previewTaskIndex == 0
      currentPage = case model.pageGrouping of
        [] -> Nothing
        pgs -> case drop model.previewTaskIndex pgs of
          [] -> Nothing
          (pg : _) -> Just pg
      pageIndices = case currentPage of
        Nothing -> [model.previewTaskIndex] -- Not yet measured: show single task
        Just pg -> pg.indices
      gapStyle = case currentPage of
        Nothing -> []
        Just pg -> [("gap", ms (showPx pg.gapPx))]
      totalPages = case model.pageGrouping of
        [] -> 1
        pgs -> length pgs
      marginTopContent
        | isFirstPage && settings.showTitle = []
        | not settings.showHeader = []
        | otherwise = [renderCompactHeader title date]
      firstPageTitleView
        | settings.showTitle && isFirstPage = [renderFirstPageHeader title date]
        | otherwise = []
      nameView
        | settings.showNameField && isFirstPage = [renderNameField]
        | otherwise = []
      marginBottomContent
        | settings.showFooter = [renderPageFooter (model.previewTaskIndex + 1) totalPages]
        | otherwise = []
   in M.div_
        [ MC.style_
            [ ("width", ms (showPx scaledW))
            , ("height", ms (showPx scaledH))
            , ("overflow", "hidden")
            ]
        , class_ "rounded shadow-md page-print-content"
        ]
        [ M.div_
            [ MC.style_
                [ ("width", ms (showPx pageWPx))
                , ("height", ms (showPx pageHPx))
                , ("padding-left", ms (showPx marginPx))
                , ("padding-right", ms (showPx marginPx))
                , ("transform", ms $ "scale(" <> T.pack (show scaleFactor) <> ")")
                , ("transform-origin", "top left")
                ]
            , class_ "bg-white text-black print-page"
            ]
            [ -- Top margin area: header sits at bottom edge
              M.div_
                [ class_ "print-margin-top"
                , MC.style_ [("height", ms (showPx marginPx))]
                ]
                marginTopContent
            , -- Content area: title (first page), name field, tasks
              M.div_
                [class_ "print-content-area"]
                ( firstPageTitleView
                    <> nameView
                    <> [ M.div_
                           [ MC.style_ gapStyle
                           , class_ "flex flex-col"
                           ]
                           [ M.div_
                               [class_ "print-task"]
                               [renderTask idx]
                           | idx <- pageIndices
                           ]
                       ]
                )
            , -- Bottom margin area: footer sits at top edge
              M.div_
                [ class_ "print-margin-bottom"
                , MC.style_ [("height", ms (showPx marginPx))]
                ]
                marginBottomContent
            ]
        ]

-- | Grid preview: one page with CSS grid cells
gridPreview :: (Int -> M.View model action) -> PrintModalModel -> GridConfig -> M.View model action
gridPreview renderTask model gc =
  let (wMm, hMm) = pageSizeMm model.settings.paperSize model.settings.orientation
      mmToPx mm = mm * 96.0 / 25.4
      pageWPx = mmToPx wMm
      pageHPx = mmToPx hMm
      previewMaxW = 440.0 :: Double
      scaleFactor = previewMaxW / pageWPx
      scaledW = pageWPx * scaleFactor
      scaledH = pageHPx * scaleFactor
      cpp = cellsPerPage gc
      -- Tasks for this page
      pageStart = model.previewTaskIndex * cpp
      taskIndices = [pageStart .. pageStart + cpp - 1]
   in M.div_
        [ MC.style_
            [ ("width", ms (showPx scaledW))
            , ("height", ms (showPx scaledH))
            , ("overflow", "hidden")
            ]
        , class_ "rounded shadow-md"
        ]
        [ M.div_
            [ MC.style_
                [ ("width", ms (showPx pageWPx))
                , ("height", ms (showPx pageHPx))
                , ("display", "grid")
                , ("grid-template-columns", ms $ "repeat(" <> T.pack (show gc.cols) <> ", 1fr)")
                , ("grid-template-rows", ms $ "repeat(" <> T.pack (show gc.rows) <> ", 1fr)")
                , ("transform", ms $ "scale(" <> T.pack (show scaleFactor) <> ")")
                , ("transform-origin", "top left")
                ]
            , class_ "bg-white text-black page-print-content"
            ]
            [ M.div_
                [class_ "print-cell"]
                [renderTask idx]
            | idx <- taskIndices
            ]
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
            , ("width", ms (showPx contentWPx))
            ]
        , M.textProp "id" "print-measure-container"
        , class_ "page-print-content"
        ]
        [ M.div_ [] [renderTask idx]
        | idx <- [0 .. taskCount - 1]
        ]

-- | First-page header: large title + date below
renderFirstPageHeader :: MisoString -> MisoString -> M.View model action
renderFirstPageHeader title date =
  M.div_
    [class_ "print-page-header"]
    [ M.div_ [class_ "print-page-header-title"] [M.text title]
    , M.div_ [class_ "print-page-header-date"] [M.text date]
    ]

-- | Compact header: title + date on same line
renderCompactHeader :: MisoString -> MisoString -> M.View model action
renderCompactHeader title date =
  M.div_
    [class_ "print-page-header-compact"]
    [ M.span_ [] [M.text title]
    , M.span_ [] [M.text date]
    ]

-- | Page footer: centered page number "X / Y"
renderPageFooter :: Int -> Int -> M.View model action
renderPageFooter pageNum totalPages =
  M.div_
    [class_ "print-page-footer"]
    [M.text $ ms (show pageNum) <> " / " <> ms (show totalPages)]

-- | Name field: "Name: ________________"
renderNameField :: M.View model action
renderNameField =
  M.div_
    [class_ "print-name-field"]
    [ M.text $ C.translate' C.LblStudentName <> ": "
    , M.span_ [class_ "print-name-field-line"] [M.text "\xA0"]
    ]

showPx :: Double -> T.Text
showPx d = T.pack (show (round d :: Int)) <> "px"
