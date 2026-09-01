module Competences.Frontend.View.MainPage
  ( mainPage
  , mainPageEmbedded
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
        , mainContentArea content
        ]
    ]
  where
    navBanner =
      MH.nav_
        [class_ "bg-primary text-primary-foreground py-1.5 flex-shrink-0 print-hide"]
        [ MH.div_
            [class_ "max-w-4xl mx-auto w-full px-3"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter <> Layout.gapM)
                [ burgerBtn
                , MH.h1_ [class_ "text-base lg:text-lg font-bold min-w-0"] [M.text title]
                , MH.div_ [class_ "hidden md:block flex-grow"] [] -- spring, hidden on mobile
                , MH.div_ [class_ "hidden md:flex md:items-center md:gap-2"] categoryIcons
                , Layout.flowSpring
                , focusedUserView
                , connectionStatus
                ]
            ]
        ]

-- | Embedded variant (?embedded, e.g. inside a Teams tab): the content
-- area without the nav banner. The host context provides the framing.
mainPageEmbedded :: View m a -> View m a
mainPageEmbedded content =
  MH.div_
    [class_ "flex-1 min-h-0"]
    [Layout.vFlow Layout.hFull [mainContentArea content]]

mainContentArea :: View m a -> View m a
mainContentArea content =
  MH.main_
    [class_ "flex-1 min-h-0 p-4 flex bg-background"]
    [content]
