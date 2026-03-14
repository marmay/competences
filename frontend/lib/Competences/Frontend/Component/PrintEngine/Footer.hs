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
renderCustomFooter :: T.Text -> ContentSettings -> [TaskId] -> M.View model action
renderCustomFooter template cs taskIds =
  let taskPoints = collectTaskPoints cs taskIds
      totalPts = sum (map snd taskPoints)
      rendered = substituteTemplate totalPts taskPoints template
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

-- | Substitute template placeholders with rendered HTML views
substituteTemplate :: Double -> [(Int, Double)] -> T.Text -> [M.View model action]
substituteTemplate totalPts taskPoints tmpl = go tmpl
  where
    go t
      | T.null t = []
      | Just rest <- T.stripPrefix "{{points table}}" t =
          renderPointsTable taskPoints totalPts : go rest
      | Just rest <- T.stripPrefix "{{signature}}" t =
          renderSignatureLine : go rest
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
                else [M.text (ms before)] <> go after

-- | Render a horizontal points table
renderPointsTable :: [(Int, Double)] -> Double -> M.View model action
renderPointsTable taskPoints totalPts =
  M.nodeHtml "table"
    [class_ "text-xs border-collapse mx-auto mt-2", MC.style_ [("border", "1px solid #999")]]
    [ M.nodeHtml "tr" [class_ "border-b border-stone-400"]
        ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-stone-300"]
              [M.text $ C.translate' C.LblTaskWord]
          ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-stone-300"]
                 [M.text $ ms (show n)]
             | (n, _) <- taskPoints
             ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center font-medium"]
                 [M.text "Gesamt"]
             ]
        )
    , M.nodeHtml "tr" [class_ "border-b border-stone-400"]
        ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-stone-300"]
              [M.text "Erreicht"]
          ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-stone-300"]
                 [M.text "\xA0"]
             | _ <- taskPoints
             ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center"]
                 [M.text "\xA0"]
             ]
        )
    , M.nodeHtml "tr" []
        ( [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-stone-300"]
              [M.text "Von"]
          ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center border-r border-stone-300"]
                 [M.text $ ms (showPoints p)]
             | (_, p) <- taskPoints
             ]
          <> [ M.nodeHtml "td" [class_ "px-2 py-0.5 text-center font-medium"]
                 [M.text $ ms (showPoints totalPts)]
             ]
        )
    ]

-- | Render a grade threshold table from inline parameters
renderPointDistribution :: Double -> T.Text -> M.View model action
renderPointDistribution totalPts params =
  let entries = parseGradeEntries params
      computed = computeGradeThresholds totalPts entries
   in M.nodeHtml "table"
        [class_ "text-xs border-collapse mx-auto mt-2", MC.style_ [("border", "1px solid #999")]]
        [ M.nodeHtml "tr" [class_ "border-b border-stone-400"]
            [ M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium border-r border-stone-300"]
                [M.text "Note"]
            , M.nodeHtml "td" [class_ "px-2 py-0.5 font-medium"]
                [M.text "Ab Punkten"]
            ]
        , M.nodeHtml "tbody" []
            [ M.nodeHtml "tr" [class_ "border-b border-stone-200"]
                [ M.nodeHtml "td" [class_ "px-2 py-0.5 border-r border-stone-300"]
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

-- | Render a signature line
renderSignatureLine :: M.View model action
renderSignatureLine =
  M.div_
    [class_ "mt-4 text-sm"]
    [ M.text "Unterschrift Erziehungsberechtigte/r: "
    , M.span_
        [MC.style_ [("display", "inline-block"), ("border-bottom", "1px solid #333"), ("width", "50%"), ("vertical-align", "bottom")]]
        [M.text "\xA0"]
    ]
