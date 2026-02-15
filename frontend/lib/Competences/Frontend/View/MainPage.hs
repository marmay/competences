module Competences.Frontend.View.MainPage
  ( mainPage
  )
where

import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Miso (View)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

-- | Main page layout with single-row nav banner and scrollable content.
--
-- Structure:
-- - Single-row nav banner: [burger] Title [category icons] StudentName [connection status]
-- - Content area (flex-1, fills available space)
mainPage
  :: View m a -- ^ Burger menu button (or empty for students)
  -> MisoString -- ^ Page title
  -> [View m a] -- ^ Category icon buttons
  -> View m a -- ^ Focused user view
  -> View m a -- ^ Connection status indicator
  -> View m a -- ^ Main content
  -> View m a
mainPage burgerBtn title categoryIcons focusedUserView connectionStatus content =
  MH.div_
    [class_ "flex-1 min-h-0"]
    [ Layout.vFlow Layout.hFull
        [ navBanner
        , contentArea
        ]
    ]
  where
    navBanner =
      MH.nav_
        [class_ "bg-primary text-primary-foreground py-1.5 flex-shrink-0 print:hidden"]
        [ MH.div_
            [class_ "max-w-4xl mx-auto w-full px-3"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter <> Layout.gapM)
                [ burgerBtn
                , MH.h1_ [class_ "text-lg font-bold"] [M.text title]
                , Layout.flowSpring
                , Layout.hFlow (Layout.gapS <> Layout.crossCenter) categoryIcons
                , Layout.flowSpring
                , focusedUserView
                , connectionStatus
                ]
            ]
        ]

    contentArea =
      MH.main_
        [class_ "flex-1 min-h-0 p-4 flex bg-background"]
        [content]
