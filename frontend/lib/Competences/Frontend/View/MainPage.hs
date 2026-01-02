module Competences.Frontend.View.MainPage
  ( mainPage
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)

-- | Main page layout with banner, navigation, scrollable content, and footer
--
-- Structure:
-- - Nav banner at top (fixed, with title and navigation items)
-- - Scrollable area containing main content and footer
-- - Footer scrolls with content (not fixed at bottom)
mainPage
  :: MisoString -- ^ Page title (shown in banner)
  -> [M.View m a] -- ^ Navigation items (buttons)
  -> M.View m a -- ^ Main content
  -> M.View m a -- ^ Footer content
  -> M.View m a
mainPage title navItems content footerContent =
  M.div_
    [class_ "h-screen flex flex-col"]
    [ navBanner
    , scrollableArea
    ]
  where
    navBanner =
      M.nav_
        [class_ "bg-card border-b border-border px-4 py-3 flex-shrink-0"]
        [ M.div_
            [class_ "space-y-2"]
            [ M.h1_ [class_ "text-xl font-bold text-foreground"] [M.text title]
            , M.div_ [class_ "flex flex-wrap gap-2"] navItems
            ]
        ]

    scrollableArea =
      M.div_
        [class_ "flex-1 overflow-y-auto min-h-0"]
        [ M.main_ [class_ "p-4"] [content]
        , M.footer_
            [class_ "border-t border-border px-4 py-2 text-center text-sm text-muted-foreground"]
            [footerContent]
        ]
