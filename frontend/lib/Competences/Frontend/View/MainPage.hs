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
-- - Nav banner at top (fixed, with title, focused user view, and navigation items)
-- - Content area (flex-1, fills available space)
-- - Footer at bottom (fixed, always visible)
mainPage
  :: MisoString -- ^ Page title (shown in banner, left-aligned)
  -> M.View m a -- ^ Focused user view (right-aligned in header)
  -> [M.View m a] -- ^ Navigation items (buttons, centered below title)
  -> M.View m a -- ^ Main content
  -> M.View m a -- ^ Footer content
  -> M.View m a
mainPage title focusedUserView navItems content footerContent =
  M.div_
    [class_ "h-screen flex flex-col"]
    [ navBanner
    , contentArea
    , footer
    ]
  where
    navBanner =
      M.nav_
        [class_ "bg-primary text-primary-foreground px-4 py-3 flex-shrink-0"]
        [ -- Top row: title left, focused user right (centered, max-width constrained)
          M.div_
            [class_ "max-w-4xl mx-auto w-full flex items-center justify-between mb-2"]
            [ M.h1_ [class_ "text-2xl font-bold"] [M.text title]
            , focusedUserView
            ]
        , -- Bottom row: navigation buttons centered
          M.div_ [class_ "flex flex-wrap gap-2 justify-center"] navItems
        ]

    contentArea =
      M.main_
        [class_ "flex-1 min-h-0 p-4 flex bg-background"]
        [content]

    footer =
      M.footer_
        [class_ "flex-shrink-0 bg-muted px-4 py-2 text-center text-sm text-muted-foreground"]
        [footerContent]
