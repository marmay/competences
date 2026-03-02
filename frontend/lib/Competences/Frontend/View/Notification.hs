-- | Reusable notification banner component.
--
-- Fixed-position banner (bottom-right) for transient notifications.
module Competences.Frontend.View.Notification
  ( notificationBanner
  )
where

import Competences.Frontend.View.Color (PaletteColor (..), PaletteName, bgClass, borderClass, textClass)
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as M

-- | Fixed-position notification banner (bottom-right, above modals at z-200).
-- Takes a palette for coloring and children for content.
notificationBanner :: PaletteName -> [M.View model action] -> M.View model action
notificationBanner p children =
  M.div_
    [ class_ $
        T.unwords
          [ "fixed bottom-4 right-4 z-200 flex items-center gap-2"
          , "px-4 py-3 rounded-lg border shadow-lg"
          , bgClass Base p
          , borderClass Accent p
          , textClass Accent p
          ]
    ]
    children
