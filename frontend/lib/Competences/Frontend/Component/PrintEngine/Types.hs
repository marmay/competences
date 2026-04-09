module Competences.Frontend.Component.PrintEngine.Types
  ( -- * Re-exports from common
    PaperSize (..)
  , Orientation (..)
  , TaskLayout (..)
  , TaskHeaderStyle (..)
  , GridConfig (..)
  , FontFamily (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , TaskContentSetting (..)
  , ContentSettings (..)
  , defaultContentSettings
  , ContentPreset (..)
  , PrintImagePosition (..)
  , ImagePrintSetting (..)
  , defaultImagePrintSetting
    -- * Frontend-only utilities
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  , cellsPerPage
  , cellSizeMm
  , expandTaskSequence
  , chunksOf
  , taskNumFromIdx
    -- * Content settings (frontend-only)
  , PrintTab (..)
  , TaskInfo (..)
  , defaultGridHeightMm
  , mkTaskInfos
  , applyPreset
  , isTaskVisible
  , taskContentSetting
  )
where

import Competences.Document.Id (Id)
import Competences.Document.Layout.Settings
  ( ContentPreset (..)
  , ContentSettings (..)
  , FontFamily (..)
  , GridConfig (..)
  , ImagePrintSetting (..)
  , Orientation (..)
  , PaperSize (..)
  , PrintImagePosition (..)
  , PrintSettings (..)
  , TaskContentSetting (..)
  , TaskHeaderStyle (..)
  , TaskLayout (..)
  , defaultContentSettings
  , defaultImagePrintSetting
  , defaultPrintSettings
  )
import Competences.Document.Solution (Solution (..), SolutionId, SolutionType (..))
import Competences.Document.Task (Task (..), TaskId, TaskIdentifier)
import Competences.Markdown.AST qualified as MD
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, toRawText)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)

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
-- Frontend-only types
-- ============================================================================

-- | Tab selection for the print modal sidebar
data PrintTab = FormatTab | ContentsTab
  deriving (Eq, Show, Generic)

-- | Summary info about a task for modal UI rendering
data TaskInfo = TaskInfo
  { taskId :: !TaskId
  , identifier :: !TaskIdentifier
  , title :: !Text
  , solutionInfos :: ![(SolutionId, SolutionType)]
  , hasLetterList :: !Bool
  , embedUrls :: ![Text]
  }
  deriving (Eq, Show, Generic)

-- | Default grid height in mm
defaultGridHeightMm :: Double
defaultGridHeightMm = 40.0

-- | Build TaskInfo list from tasks with solutions and content
mkTaskInfos :: [(Task, [Solution], Maybe RichContent)] -> [TaskInfo]
mkTaskInfos = map $ \(task, sols, mContent) ->
  let (letterList, urls) = analyzeContent mContent
   in TaskInfo
        { taskId = task.id
        , identifier = task.identifier
        , title = task.title
        , solutionInfos = map (\s -> (s.id, s.solutionType)) sols
        , hasLetterList = letterList
        , embedUrls = urls
        }

-- | Parse content once and extract both hasLetterList and embedUrls in a single pass.
analyzeContent :: Maybe RichContent -> (Bool, [Text])
analyzeContent Nothing = (False, [])
analyzeContent (Just rc) = case Markdown.parseMarkdown (toRawText rc) of
  Left _ -> (False, [])
  Right (MD.Document blocks) ->
    ( any hasLetterListBlock blocks
    , concatMap collectUrlsFromBlock blocks
    )

hasLetterListBlock :: MD.Block -> Bool
hasLetterListBlock (MD.LetterList _) = True
hasLetterListBlock (MD.OrderedList _ items) = any (any hasLetterListBlock) items
hasLetterListBlock (MD.BulletList items) = any (any hasLetterListBlock) items
hasLetterListBlock (MD.Admonition _ _ blocks) = any hasLetterListBlock blocks
hasLetterListBlock (MD.NotesGrid c1 c2 c3 c4) = any hasLetterListBlock (c1 ++ c2 ++ c3 ++ c4)
hasLetterListBlock (MD.ClozeBlock body opts) =
  any hasLetterListBlock body || case opts of
    MD.ClozeNoOptions -> False
    MD.ClozeWordBank bs -> any hasLetterListBlock bs
    MD.ClozePerBlankOptions groups -> any (any hasLetterListBlock) groups
hasLetterListBlock (MD.ChoiceBlock _ items) = any (any hasLetterListBlock) items
hasLetterListBlock (MD.MappingBlock l r) = any (any hasLetterListBlock) l || any (any hasLetterListBlock) r
hasLetterListBlock _ = False

collectUrlsFromBlock :: MD.Block -> [Text]
collectUrlsFromBlock = \case
  MD.Paragraph inlines -> concatMap collectUrlsFromInline inlines
  MD.Heading _ inlines -> concatMap collectUrlsFromInline inlines
  MD.OrderedList _ items -> concatMap (concatMap collectUrlsFromBlock) items
  MD.BulletList items -> concatMap (concatMap collectUrlsFromBlock) items
  MD.LetterList items -> concatMap (concatMap collectUrlsFromBlock) items
  MD.Admonition _ mTitle bs ->
    maybe [] (concatMap collectUrlsFromInline) mTitle
      ++ concatMap collectUrlsFromBlock bs
  MD.NotesGrid c1 c2 c3 c4 -> concatMap collectUrlsFromBlock (c1 ++ c2 ++ c3 ++ c4)
  MD.ClozeBlock body opts ->
    concatMap collectUrlsFromBlock body ++ case opts of
      MD.ClozeNoOptions -> []
      MD.ClozeWordBank bs -> concatMap collectUrlsFromBlock bs
      MD.ClozePerBlankOptions groups -> concatMap (concatMap collectUrlsFromBlock) groups
  MD.ChoiceBlock _ items -> concatMap (concatMap collectUrlsFromBlock) items
  MD.MappingBlock l r ->
    concatMap (concatMap collectUrlsFromBlock) l
      ++ concatMap (concatMap collectUrlsFromBlock) r
  _ -> []

collectUrlsFromInline :: MD.Inline -> [Text]
collectUrlsFromInline = \case
  MD.FileEmbed url _ _ _ -> [url]
  MD.Emph inlines -> concatMap collectUrlsFromInline inlines
  MD.Strong inlines -> concatMap collectUrlsFromInline inlines
  MD.Link _ inlines _ -> concatMap collectUrlsFromInline inlines
  _ -> []

-- | Apply a preset to produce content settings for the given tasks
applyPreset :: ContentPreset -> [TaskInfo] -> ContentSettings
applyPreset preset infos = ContentSettings
  { perTask = Map.fromList $ map (\ti -> (ti.taskId, presetForTask preset ti)) infos
  , showTitle = presetShowTitle preset
  , showNameField = presetShowNameField preset
  , customFooter = Nothing
  }

presetShowTitle :: ContentPreset -> Bool
presetShowTitle _ = True

presetShowNameField :: ContentPreset -> Bool
presetShowNameField Aufgabenblatt = True
presetShowNameField Arbeitsblatt = True
presetShowNameField Loesungsblatt = False
presetShowNameField Musteraufgaben = False

presetForTask :: ContentPreset -> TaskInfo -> TaskContentSetting
presetForTask Aufgabenblatt ti = TaskContentSetting
  { showDescription = True
  , visibleSolutions = solutionsOfType Hint ti
  , gridHeightMm = Nothing
  , inlineAnswer = False
  , itemsPerRow = 1
  , points = Nothing
  , imageSettings = Map.empty
  }
presetForTask Arbeitsblatt ti = TaskContentSetting
  { showDescription = True
  , visibleSolutions = solutionsOfType Hint ti
  , gridHeightMm = Just defaultGridHeightMm
  , inlineAnswer = False
  , itemsPerRow = 1
  , points = Nothing
  , imageSettings = Map.empty
  }
presetForTask Loesungsblatt ti = TaskContentSetting
  { showDescription = False
  , visibleSolutions = solutionsOfType Results ti
  , gridHeightMm = Nothing
  , inlineAnswer = False
  , itemsPerRow = 1
  , points = Nothing
  , imageSettings = Map.empty
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
        , points = Nothing
        , imageSettings = Map.empty
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
    , points = Nothing
    , imageSettings = Map.empty
    }
  Just tcs -> tcs
