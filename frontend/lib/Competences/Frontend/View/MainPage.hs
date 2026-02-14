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

-- | Main page layout with single-row nav banner, scrollable content, and footer.
--
-- Structure:
-- - Single-row nav banner: [burger] Title [category icons] StudentName
-- - Content area (flex-1, fills available space)
-- - Footer at bottom (fixed, always visible)
mainPage
  :: View m a -- ^ Burger menu button (or empty for students)
  -> MisoString -- ^ Page title
  -> [View m a] -- ^ Category icon buttons
  -> View m a -- ^ Focused user view
  -> View m a -- ^ Main content
  -> View m a -- ^ Footer content
  -> View m a
mainPage burgerBtn title categoryIcons focusedUserView content footerContent =
  MH.div_
    [class_ "flex-1 min-h-0"]
    [ Layout.vFlow Layout.hFull
        [ navBanner
        , contentArea
        , footer
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
                ]
            ]
        ]

    contentArea =
      MH.main_
        [class_ "flex-1 min-h-0 p-4 flex bg-background"]
        [content]

    footer =
      MH.footer_
        [class_ "flex-shrink-0 bg-muted px-4 py-2 text-center text-sm text-muted-foreground print:hidden"]
        [footerContent]
