module Competences.Frontend.Component.PrintEngine.Types
  ( PaperSize (..)
  , Orientation (..)
  , TaskLayout (..)
  , TaskHeaderStyle (..)
  , GridConfig (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  , cellsPerPage
  , cellSizeMm
  , expandTaskSequence
  , chunksOf
  , taskNumFromIdx
    -- * Content settings
  , PrintTab (..)
  , TaskContentSetting (..)
  , ContentSettings (..)
  , ContentPreset (..)
  , TaskInfo (..)
  , defaultGridHeightMm
  , mkTaskInfos
  , applyPreset
  , isTaskVisible
  , taskContentSetting
  )
where

import Competences.Document.Id (Id)
import Competences.Document.Solution (Solution (..), SolutionId, SolutionType (..))
import Competences.Document.Task (Task (..), TaskId, TaskIdentifier)
import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Supported paper sizes
data PaperSize = A4 | A5
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Page orientation
data Orientation = Portrait | Landscape
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | How the task header (h2) is rendered
data TaskHeaderStyle = HeaderNumber | HeaderTitle | HeaderBoth
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | How tasks are laid out on the page
data TaskLayout
  = -- | Tasks flow continuously, no forced page breaks
    Continuous
  | -- | Tasks arranged in a grid (rows x cols per page)
    Grid !GridConfig
  deriving (Eq, Show, Generic)

-- | Grid dimensions (rows and columns per page)
data GridConfig = GridConfig
  { rows :: !Int
  , cols :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Print configuration
data PrintSettings = PrintSettings
  { paperSize :: !PaperSize
  , orientation :: !Orientation
  , baseFontSize :: !Double
  , taskLayout :: !TaskLayout
  , groupedCopies :: !Int
  , totalCopies :: !Int
  , showTitle :: !Bool
  , showHeader :: !Bool
  , showFooter :: !Bool
  , showNameField :: !Bool
  , taskHeaderStyle :: !TaskHeaderStyle
  }
  deriving (Eq, Show, Generic)

-- | Default: A4 portrait, continuous layout, 1 copy each
defaultPrintSettings :: PrintSettings
defaultPrintSettings =
  PrintSettings
    { paperSize = A4
    , orientation = Portrait
    , baseFontSize = 10.0
    , taskLayout = Continuous
    , groupedCopies = 1
    , totalCopies = 1
    , showTitle = True
    , showHeader = True
    , showFooter = True
    , showNameField = True
    , taskHeaderStyle = HeaderBoth
    }

-- | CSS @page size value (e.g. "A5", "A4 landscape")
pageSizeCSS :: PaperSize -> Orientation -> Text
pageSizeCSS ps Portrait = paperSizeName ps
pageSizeCSS ps Landscape = paperSizeName ps <> " landscape"

paperSizeName :: PaperSize -> Text
paperSizeName A4 = "A4"
paperSizeName A5 = "A5"

-- | Page dimensions in mm (width, height), accounting for orientation
pageSizeMm :: PaperSize -> Orientation -> (Double, Double)
pageSizeMm ps Portrait = portraitDimensions ps
pageSizeMm ps Landscape = let (w, h) = portraitDimensions ps in (h, w)

portraitDimensions :: PaperSize -> (Double, Double)
portraitDimensions A4 = (210.0, 297.0)
portraitDimensions A5 = (148.0, 210.0)

-- | Page margin in mm (depends on paper size only)
pageMarginMm :: PaperSize -> Double
pageMarginMm A4 = 20.0
pageMarginMm A5 = 15.0

-- | Number of cells (tasks) per page in grid mode
cellsPerPage :: GridConfig -> Int
cellsPerPage gc = gc.rows * gc.cols

-- | Cell size in mm (width, height) for a grid layout.
-- Each cell is (pageWidth / cols, pageHeight / rows).
cellSizeMm :: PaperSize -> Orientation -> GridConfig -> (Double, Double)
cellSizeMm ps orient gc =
  let (pw, ph) = pageSizeMm ps orient
   in (pw / fromIntegral gc.cols, ph / fromIntegral gc.rows)

-- | Expand a task sequence according to grouped and total copies.
-- Given tasks [A,B,C], groupedCopies=2, totalCopies=2:
-- Result: [A,A,B,B,C,C, A,A,B,B,C,C]
expandTaskSequence :: Int -> Int -> [a] -> [a]
expandTaskSequence grouped total tasks =
  let gc = max 1 grouped
      tc = max 1 total
      groupExpanded = concatMap (replicate gc) tasks
   in concat (replicate tc groupExpanded)

-- | Split a list into chunks of at most n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let n' = max 1 n
      (chunk, rest) = splitAt n' xs
   in chunk : chunksOf n' rest

-- | Compute the 1-based original task number from an expanded index.
-- With groupedCopies=2 and tasks [A,B,C], expanded is [A,A,B,B,C,C,...].
-- Both copies of task A get number 1, both copies of B get number 2, etc.
taskNumFromIdx :: Int -> Int -> Int -> Int
taskNumFromIdx groupedCopies originalCount idx =
  let gc = max 1 groupedCopies
   in (idx `mod` (originalCount * gc)) `div` gc + 1

-- ============================================================================
-- Content Settings (what to include per task in print output)
-- ============================================================================

-- | Tab selection for the print modal sidebar
data PrintTab = FormatTab | ContentsTab
  deriving (Eq, Show, Generic)

-- | Per-task content settings for print output
data TaskContentSetting = TaskContentSetting
  { showDescription :: !Bool
  , visibleSolutions :: !(Set SolutionId)
  , gridHeightMm :: !(Maybe Double)
  , inlineAnswer :: !Bool
  , itemsPerRow :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Content settings: per-task map of what to include
newtype ContentSettings = ContentSettings
  { perTask :: Map TaskId TaskContentSetting
  }
  deriving (Eq, Show, Generic)

-- | Preset configurations for quick setup
data ContentPreset
  = Aufgabenblatt
  | Arbeitsblatt
  | Loesungsblatt
  | Musteraufgaben
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Summary info about a task for modal UI rendering
data TaskInfo = TaskInfo
  { taskId :: !TaskId
  , identifier :: !TaskIdentifier
  , title :: !Text
  , solutionInfos :: ![(SolutionId, SolutionType)]
  , hasLetterList :: !Bool
  }
  deriving (Eq, Show, Generic)

-- | Default grid height in mm
defaultGridHeightMm :: Double
defaultGridHeightMm = 40.0

-- | Build TaskInfo list from tasks with solutions and content
mkTaskInfos :: [(Task, [Solution], Maybe RichContent)] -> [TaskInfo]
mkTaskInfos = map $ \(task, sols, mContent) ->
  TaskInfo
    { taskId = task.id
    , identifier = task.identifier
    , title = task.title
    , solutionInfos = map (\s -> (s.id, s.solutionType)) sols
    , hasLetterList = containsLetterList mContent
    }

-- | Check whether rich content contains a LetterList block
containsLetterList :: Maybe RichContent -> Bool
containsLetterList Nothing = False
containsLetterList (Just rc) = case Markdown.parseMarkdown (toRawText rc) of
  Left _ -> False
  Right (MD.Document blocks) -> any hasLetterListBlock blocks

hasLetterListBlock :: MD.Block -> Bool
hasLetterListBlock (MD.LetterList _) = True
hasLetterListBlock (MD.OrderedList _ items) = any (any hasLetterListBlock) items
hasLetterListBlock (MD.BulletList items) = any (any hasLetterListBlock) items
hasLetterListBlock (MD.Admonition _ _ blocks) = any hasLetterListBlock blocks
hasLetterListBlock (MD.NotesGrid c1 c2 c3 c4) = any hasLetterListBlock (c1 ++ c2 ++ c3 ++ c4)
hasLetterListBlock _ = False

-- | Apply a preset to produce content settings for the given tasks
applyPreset :: ContentPreset -> [TaskInfo] -> ContentSettings
applyPreset preset infos = ContentSettings
  { perTask = Map.fromList $ map (\ti -> (ti.taskId, presetForTask preset ti)) infos
  }

presetForTask :: ContentPreset -> TaskInfo -> TaskContentSetting
presetForTask Aufgabenblatt ti = TaskContentSetting
  { showDescription = True
  , visibleSolutions = solutionsOfType Hint ti
  , gridHeightMm = Nothing
  , inlineAnswer = False
  , itemsPerRow = 1
  }
presetForTask Arbeitsblatt ti = TaskContentSetting
  { showDescription = True
  , visibleSolutions = solutionsOfType Hint ti
  , gridHeightMm = Just defaultGridHeightMm
  , inlineAnswer = False
  , itemsPerRow = 1
  }
presetForTask Loesungsblatt ti = TaskContentSetting
  { showDescription = False
  , visibleSolutions = solutionsOfType Results ti
  , gridHeightMm = Nothing
  , inlineAnswer = False
  , itemsPerRow = 1
  }
presetForTask Musteraufgaben ti =
  let completeIds = solutionsOfType Complete ti
      resultIds = solutionsOfType Results ti
      hintIds = solutionsOfType Hint ti
      visible = if Set.null completeIds then resultIds else completeIds
   in TaskContentSetting
        { showDescription = True
        , visibleSolutions = Set.union hintIds visible
        , gridHeightMm = Nothing
        , inlineAnswer = False
        , itemsPerRow = 1
        }

solutionsOfType :: SolutionType -> TaskInfo -> Set (Id Solution)
solutionsOfType stype ti =
  Set.fromList [sid | (sid, st) <- ti.solutionInfos, st == stype]

-- | Whether a task should be visible at all (has any content to show)
isTaskVisible :: ContentSettings -> TaskId -> Bool
isTaskVisible cs tid = case Map.lookup tid cs.perTask of
  Nothing -> False
  Just tcs ->
    tcs.showDescription
      || not (Set.null tcs.visibleSolutions)
      || isJust tcs.gridHeightMm

-- | Look up content setting for a task, defaulting to everything off
taskContentSetting :: ContentSettings -> TaskId -> TaskContentSetting
taskContentSetting cs tid = case Map.lookup tid cs.perTask of
  Nothing -> TaskContentSetting
    { showDescription = False
    , visibleSolutions = Set.empty
    , gridHeightMm = Nothing
    , inlineAnswer = False
    , itemsPerRow = 1
    }
  Just tcs -> tcs
