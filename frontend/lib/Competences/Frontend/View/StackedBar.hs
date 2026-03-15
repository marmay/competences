module Competences.Frontend.View.StackedBar
  ( BarSegment (..)
  , StackedBarConfig (..)
  , stackedBar
  , stackedBarOnly
  )
where

import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Data.Text (Text)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH

-- | A single segment in a stacked bar
data BarSegment m action = BarSegment
  { count :: !Int
  , colorClass :: !Text
  -- ^ bg-* class for the segment
  , tooltip :: !(Tooltip m action)
  -- ^ tooltip to show on the indicator
  }

-- | Configuration for a stacked bar display
data StackedBarConfig m action = StackedBarConfig
  { total :: !Int
  -- ^ denominator for width percentages
  , segments :: ![BarSegment m action]
  }

-- | Render a stacked horizontal bar with legend row below
stackedBar :: StackedBarConfig m action -> M.View m action
stackedBar config =
  MH.div_
    [class_ "mt-1"]
    [ Layout.vFlow
        Layout.gapT
        [ barView config
        , -- Count labels below
          MH.div_
            [class_ "text-xs"]
            [ Layout.addClass "gap-x-2" $
                Layout.hFlow'
                  (map renderIndicator config.segments)
            ]
        ]
    ]
  where
    renderIndicator seg =
      let isZero = seg.count == 0
          opacityClass = if isZero then " opacity-30" else ""
          textColorClass = if isZero then "text-stone-400" else "text-stone-600"
          tip = if isZero then NoTooltip else seg.tooltip
       in withTooltip tip $
            MH.div_
              [class_ opacityClass]
              [ Layout.addClass "gap-0.5" $
                  Layout.hFlow
                    (Layout.hFull <> Layout.crossCenter)
                    [ MH.div_ [class_ $ "w-2 h-2 rounded-sm " <> seg.colorClass] []
                    , MH.span_ [class_ textColorClass] [M.text $ M.ms $ show seg.count]
                    ]
              ]

-- | Render just the stacked horizontal bar without legend
stackedBarOnly :: StackedBarConfig m action -> M.View m action
stackedBarOnly = barView

-- | Internal: the bar itself
barView :: StackedBarConfig m action -> M.View m action
barView config =
  MH.div_
    [class_ "h-3 rounded overflow-hidden bg-stone-100"]
    [ Layout.hFlow
        Layout.hFull
        (map renderSegment config.segments)
    ]
  where
    percentage count =
      if config.total > 0
        then (fromIntegral count * 100.0 / fromIntegral config.total) :: Double
        else 0.0

    renderSegment seg =
      let pct = percentage seg.count
       in if seg.count > 0
            then
              MH.div_
                [ class_ $ seg.colorClass <> " h-full"
                , MC.style_ [("width", M.ms $ show pct <> "%")]
                ]
                []
            else M.text ""
