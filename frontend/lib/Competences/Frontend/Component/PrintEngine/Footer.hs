module Competences.Frontend.Component.PrintEngine.Footer
  ( renderCustomFooter
  , collectTaskPoints
  , showPoints
  )
where

import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentSettings
  , TaskContentSetting (..)
  , taskContentSetting
  )
import Competences.Frontend.Component.RichContent (FormulaCache, renderMarkdownText)
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as T
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as M
import Miso.String (ms)
import Text.Read (readMaybe)

-- | Render custom footer template with placeholder substitution.
-- Takes the template text, content settings, and the list of task IDs
-- (de-duplicated, in original order).
renderCustomFooter :: FormulaCache -> T.Text -> ContentSettings -> [TaskId] -> M.View model action
renderCustomFooter fc template cs taskIds =
  let taskPoints = collectTaskPoints cs taskIds
      totalPts = sum (map snd taskPoints)
      rendered = substituteTemplate fc totalPts taskPoints template
   in M.div_
        [class_ "mt-4 print-custom-footer"]
        rendered

-- | Collect per-task points from visible tasks.
-- Returns (1-based task number, points) for tasks that have points set.
collectTaskPoints :: ContentSettings -> [TaskId] -> [(Int, Double)]
collectTaskPoints cs taskIds =
  [ (i, p)
  | (i, tid) <- zip [1 ..] taskIds
  , let tcs = taskContentSetting cs tid
  , Just p <- [tcs.points]
  ]

-- | Format points for display: show as integer if whole, otherwise one decimal
showPoints :: Double -> T.Text
showPoints p
  | p == fromIntegral (round p :: Int) = T.pack (show (round p :: Int))
  | otherwise = T.pack (show p)

-- | Substitute template placeholders with rendered HTML views.
-- Text between placeholders is rendered as markdown (supports bold, italic, etc.).
substituteTemplate :: FormulaCache -> Double -> [(Int, Double)] -> T.Text -> [M.View model action]
substituteTemplate fc totalPts taskPoints tmpl = go tmpl
  where
    go t
      | T.null t = []
      | Just rest <- T.stripPrefix "{{points table:kl fehler}}" t =
          renderPointsTable True taskPoints totalPts : go rest
      | Just rest <- T.stripPrefix "{{points table}}" t =
          renderPointsTable False taskPoints totalPts : go rest
      | Just rest <- T.stripPrefix "{{signature}}" t =
          renderSignatureLine : go rest
      | Just rest <- T.stripPrefix "{{grade}}" t =
          renderGradeField : go rest
      | "{{point distribution:" `T.isPrefixOf` t =
          let afterPrefix = T.drop (T.length "{{point distribution:") t
           in case T.breakOn "}}" afterPrefix of
                (params, rest')
                  | not (T.null rest') ->
                      renderPointDistribution totalPts params : go (T.drop 2 rest')
                _ -> [M.text (ms t)] -- malformed, render as-is
      | otherwise =
          let (before, after) = T.breakOn "{{" t
           in if T.null before
                then [M.text (ms (T.take 2 after))] <> go (T.drop 2 after) -- skip unrecognized {{
                else [renderTextChunk before] <> go after
    renderTextChunk txt =
      M.div_
        [class_ "prose prose-sm max-w-none"]
        [renderMarkdownText fc txt]

-- | Render a horizontal points table.
-- When 'klFehler' is True, an extra "Kl. Fehler" row is appended.
renderPointsTable :: Bool -> [(Int, Double)] -> Double -> M.View model action
renderPointsTable klFehler taskPoints totalPts =
  let blankRow borderBottom label =
        M.nodeHtml "tr" [class_ $ if borderBottom then "border-b border-black" else ""]
          ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-black"]
                [M.text label]
            ]
            <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-black"]
                   [M.text "\xA0"]
               | _ <- taskPoints
               ]
            <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center"]
                   [M.text "\xA0"]
               ]
          )
   in M.nodeHtml "table"
        [class_ "text-xs border-collapse mx-auto mt-2 border border-black"]
        ( [ -- Row 1: Task headers
            M.nodeHtml "tr" [class_ "border-b border-black"]
              ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-black"]
                    [M.text $ C.translate' C.LblTaskWord]
                ]
                <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-black"]
                       [M.text $ ms (show n)]
                   | (n, _) <- taskPoints
                   ]
                <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center font-medium"]
                       [M.text "Gesamt"]
                   ]
              )
          , -- Row 2: Punkte (max points per task)
            M.nodeHtml "tr" [class_ "border-b border-black"]
              ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-black"]
                    [M.text "Punkte"]
                ]
                <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-black"]
                       [M.text $ ms (showPoints p)]
                   | (_, p) <- taskPoints
                   ]
                <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center font-medium"]
                       [M.text $ ms (showPoints totalPts)]
                   ]
              )
          , -- Row 3: Erreicht (blank, to be filled in)
            blankRow klFehler "Erreicht"
          ]
          <> [blankRow False "Kl. Fehler" | klFehler]
        )

-- | Render a grade threshold table from inline parameters
renderPointDistribution :: Double -> T.Text -> M.View model action
renderPointDistribution totalPts params =
  let entries = parseGradeEntries params
      computed = computeGradeThresholds totalPts entries
   in M.nodeHtml "table"
        [class_ "text-xs border-collapse mx-auto mt-2 border border-black"]
        [ M.nodeHtml "tr" [class_ "border-b border-black"]
            [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-black"]
                [M.text "Note"]
            , M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium"]
                [M.text "Ab Punkten"]
            ]
        , M.nodeHtml "tbody" []
            [ M.nodeHtml "tr" [class_ "border-b border-black"]
                [ M.nodeHtml "td" [class_ "px-2 py-0.5 border-r border-black"]
                    [M.text $ ms grade]
                , M.nodeHtml "td" [class_ "px-2 py-0.5 text-center"]
                    [M.text $ ms threshold]
                ]
            | (grade, threshold) <- computed
            ]
        ]

-- | Parse colon-separated grade entries like "90% Sehr gut:80% Gut:- Nicht genuegend"
parseGradeEntries :: T.Text -> [(Maybe Double, T.Text)]
parseGradeEntries = map parseEntry . T.splitOn ":"
  where
    parseEntry entry =
      let trimmed = T.strip entry
       in case T.breakOn " " trimmed of
            (pct, name)
              | "%" `T.isSuffixOf` pct ->
                  case readMaybeT (T.dropEnd 1 pct) of
                    Just p -> (Just (p / 100.0), T.strip name)
                    Nothing -> (Nothing, trimmed)
              | pct == "-" -> (Nothing, T.strip name)
              | otherwise -> (Nothing, trimmed)

readMaybeT :: T.Text -> Maybe Double
readMaybeT = readMaybe . T.unpack

-- | Compute point thresholds from percentages and total points
computeGradeThresholds :: Double -> [(Maybe Double, T.Text)] -> [(T.Text, T.Text)]
computeGradeThresholds totalPts = map $ \(mPct, grade) ->
  case mPct of
    Just pct ->
      let pts = pct * totalPts
          rounded = fromIntegral (ceiling pts :: Int) :: Double
       in (grade, showPoints rounded)
    Nothing -> (grade, "-")

-- | Render a labeled line: a horizontal rule with a label centered below
labeledLine :: T.Text -> M.View model action
labeledLine label =
  M.div_ [class_ "flex-1 text-center text-xs"]
    [ M.div_
        [class_ "border-b border-black", MC.style_ [("min-width", "8em")]]
        [M.text "\xA0"]
    , M.div_ [] [M.text (ms label)]
    ]

-- | Render grade field: two side-by-side labeled lines for points and grade.
-- Points line is ~1/3 the width of grade line, with ~10% spacing around and between.
renderGradeField :: M.View model action
renderGradeField =
  M.div_
    [ class_ "mt-4 flex"
    , MC.style_ [("padding-left", "10%"), ("padding-right", "10%"), ("gap", "10%")]
    ]
    [ M.div_ [MC.style_ [("flex", "1")]] [labeledLine "Erreichte Punkte"]
    , M.div_ [MC.style_ [("flex", "3")]] [labeledLine "Note"]
    ]

-- | Render a signature line using label-under-line format, right-aligned
renderSignatureLine :: M.View model action
renderSignatureLine =
  M.div_ [class_ "mt-4 max-w-xs ml-auto"]
    [labeledLine "Unterschrift Erziehungsberechtigte/r"]
