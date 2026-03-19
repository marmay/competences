module Competences.Frontend.Component.RenumberModal
  ( RenumberTaskInfo (..)
  , openRenumberModal
  )
where

import Competences.Command (Command (..))
import Competences.Command.Common (EntityCommand (..))
import Competences.Command.Common qualified as Cmd (ModifyCommand (..))
import Competences.Command.Tasks (TaskPatch (..), TasksCommand (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , modifySyncDocument
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , WindowMode
  , closeWindow
  , openFramedModalWith
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (def)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)
import Optics.Core ((&), (.~))
import Text.Read (readMaybe)

-- ============================================================================
-- Public types
-- ============================================================================

-- | Info about a task to be renumbered, provided by the caller.
-- Tasks are in assignment order.
data RenumberTaskInfo = RenumberTaskInfo
  { taskId :: !TaskId
  , identifier :: !TaskIdentifier
  , title :: !Text
  , isMultiAssignment :: !Bool
  }

-- ============================================================================
-- Internal types
-- ============================================================================

data RenumberEntry = RenumberEntry
  { taskId :: !TaskId
  , oldIdentifier :: !TaskIdentifier
  , newIdentifier :: !TaskIdentifier
  , displayName :: !Text
  , prefix :: !Text
  , isMultiAssignment :: !Bool
  , included :: !Bool
  }
  deriving (Eq, Generic, Show)

data RenumberModel = RenumberModel
  { entries :: ![RenumberEntry]
  , skippedCount :: !Int
  }
  deriving (Eq, Generic, Show)

data RenumberAction
  = ToggleEntry !TaskId
  | ApplyRenumber
  | CancelRenumber
  deriving (Eq, Show)

-- ============================================================================
-- Identifier parsing
-- ============================================================================

-- | Last-dot split: "M.3" → Just ("M", 3), "M.3.a" → Nothing, "noprefix" → Nothing
parseIdentifier :: TaskIdentifier -> Maybe (Text, Int)
parseIdentifier (TaskIdentifier t) =
  case T.breakOnEnd "." t of
    ("", _) -> Nothing -- no dot
    (prefixDot, numPart) ->
      let prefix = T.dropEnd 1 prefixDot -- remove trailing dot
       in case readMaybe (T.unpack numPart) of
            Just n | n > 0 -> Just (prefix, n)
            _ -> Nothing

-- ============================================================================
-- Entry computation
-- ============================================================================

-- | Compute renumber entries from ordered task list.
-- Groups by prefix, assigns 1,2,3... per group in assignment order.
computeEntries :: [RenumberTaskInfo] -> ([RenumberEntry], Int)
computeEntries infos =
  let -- First pass: try to parse each task's identifier
      parsed =
        [ (info, parseIdentifier info.identifier)
        | info <- infos
        ]

      -- Count tasks that couldn't be parsed
      skipped = length [() | (_, Nothing) <- parsed]

      -- Pre-count totals per prefix for zero-padding width
      prefixTotals =
        Map.fromListWith (+) [(prefix, 1 :: Int) | (_, Just (prefix, _)) <- parsed]

      padNum total n =
        let width = max 2 (length (show total))
         in T.justifyRight width '0' (T.pack (show n))

      -- Build a counter per prefix: assign sequential numbers
      -- We go through tasks in order, incrementing a per-prefix counter
      (revEntries, _) = foldl step ([], Map.empty :: Map.Map Text Int) parsed

      step (acc, counters) (info, mParsed) = case mParsed of
        Nothing -> (acc, counters) -- skip unparseable
        Just (prefix, _oldNum) ->
          let count = Map.findWithDefault 0 prefix counters + 1
              counters' = Map.insert prefix count counters
              total = Map.findWithDefault 0 prefix prefixTotals
              newIdent = TaskIdentifier (prefix <> "." <> padNum total count)
              TaskIdentifier identText = info.identifier
              displayName =
                if T.null info.title
                  then identText
                  else identText <> " \x2014 " <> info.title
              entry =
                RenumberEntry
                  { taskId = info.taskId
                  , oldIdentifier = info.identifier
                  , newIdentifier = newIdent
                  , displayName = displayName
                  , prefix = prefix
                  , isMultiAssignment = info.isMultiAssignment
                  , included = not info.isMultiAssignment
                  }
           in (entry : acc, counters')
   in (reverse revEntries, skipped)

-- ============================================================================
-- Modal opening
-- ============================================================================

-- | Open the renumber modal via WindowManager
openRenumberModal :: SyncContext -> (Command -> Command) -> [RenumberTaskInfo] -> IO ()
openRenumberModal r wrapCmd infos =
  let cfg =
        ModalConfig
          { chrome = WindowChrome (C.translate' C.LblRenumberTasks) Icon.IcnReorder
          , modalId = ModalId "renumber-tasks"
          , width = ModalNarrow
          , height = ModalAuto
          , pinnable = Nothing
          }
   in openFramedModalWith r.windowManager cfg (renumberModalComponent r wrapCmd infos)

-- ============================================================================
-- Modal component
-- ============================================================================

renumberModalComponent :: SyncContext -> (Command -> Command) -> [RenumberTaskInfo] -> WindowMode -> M.Component p RenumberModel RenumberAction
renumberModalComponent r wrapCmd infos wm =
  M.component initialModel update view'
  where
    (initialEntries, skipped) = computeEntries infos

    initialModel =
      RenumberModel
        { entries = initialEntries
        , skippedCount = skipped
        }

    update (ToggleEntry tid) =
      M.modify $ \m ->
        m
          { entries =
              map
                (\e -> if e.taskId == tid then e {included = not e.included} else e)
                m.entries
          }
    update ApplyRenumber = do
      m <- M.get
      let toRename =
            [ e
            | e <- m.entries
            , e.included
            , e.oldIdentifier /= e.newIdentifier
            ]
      M.io_ $ do
        mapM_
          ( \e -> do
              modifySyncDocument r $ wrapCmd $ Tasks (OnTasks (Modify e.taskId Cmd.Lock))
              let patch = def & #identifier .~ Just (e.oldIdentifier, e.newIdentifier) :: TaskPatch
              modifySyncDocument r $ wrapCmd $ Tasks (OnTasks (Modify e.taskId (Cmd.Release patch)))
          )
          toRename
        closeWindow wm
    update CancelRenumber =
      M.io_ $ closeWindow wm

    view' m =
      MH.div_
        [class_ "p-4 space-y-4 max-h-[70vh] overflow-y-auto"]
        ( viewEntries m.entries
            <> viewFooter m
        )

-- ============================================================================
-- View helpers
-- ============================================================================

-- | Render entries grouped by prefix. Uses 'groupBy' which groups consecutive
-- runs — interleaved prefixes will produce separate visual groups, preserving
-- assignment order.
viewEntries :: [RenumberEntry] -> [M.View model RenumberAction]
viewEntries entries =
  let grouped = groupBy (\a b -> a.prefix == b.prefix) entries
   in concatMap viewGroup grouped

viewGroup :: [RenumberEntry] -> [M.View model RenumberAction]
viewGroup [] = []
viewGroup grp@(first : _) =
  [ -- Group header
    MH.div_
      [class_ "mt-2 pt-2 border-t border-border"]
      [ Typography.small $
          C.translate' (C.LblRenumberPrefix first.prefix (length grp))
      ]
  ]
    <> map viewEntry grp

viewEntry :: RenumberEntry -> M.View model RenumberAction
viewEntry entry =
  let TaskIdentifier oldText = entry.oldIdentifier
      TaskIdentifier newText = entry.newIdentifier
      noChange = entry.oldIdentifier == entry.newIdentifier
      dimmedClass = if noChange then " opacity-50" else ""
   in MH.div_
        [class_ $ "flex items-center gap-2 py-1" <> dimmedClass]
        [ MH.input_
            [ MP.type_ "checkbox"
            , MP.checked_ entry.included
            , MH.onClick (ToggleEntry entry.taskId)
            , class_ "input"
            ]
        , MH.span_
            [class_ "font-mono text-sm"]
            [ M.text (ms oldText)
            , MH.span_ [class_ "text-muted-foreground mx-1"] [M.text "\x2192"]
            , MH.span_
                [class_ $ if noChange then "text-muted-foreground" else "font-semibold"]
                [M.text (ms newText)]
            ]
        , MH.span_
            [class_ "text-xs text-muted-foreground truncate"]
            [M.text (ms entry.displayName)]
        , if entry.isMultiAssignment
            then Badge.outline (Badge.badgeText "Multi")
            else M.text ""
        ]

viewFooter :: RenumberModel -> [M.View model RenumberAction]
viewFooter m =
  let changesToApply =
        length
          [ ()
          | e <- m.entries
          , e.included
          , e.oldIdentifier /= e.newIdentifier
          ]
      skippedNote =
        [ Typography.muted $ C.translate' (C.LblRenumberSkipped m.skippedCount)
        | m.skippedCount > 0
        ]
      summaryNote =
        [ Typography.muted $ C.translate' (C.LblRenumberSummary changesToApply)
        ]
   in skippedNote
        <> summaryNote
        <> [ Layout.actionFooter
              [ Button.secondary $
                  Button.button (Icon.IcnCancel, C.LblCancel) CancelRenumber
              , if changesToApply > 0
                  then
                    Button.primary $
                      Button.button (Icon.IcnApply, C.LblApply) ApplyRenumber
                  else
                    Button.primary $
                      Button.button (Icon.IcnApply, C.LblApply) Button.Disabled
              ]
           ]
