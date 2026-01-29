-- | Stateless disclosure (expand/collapse) rendering helpers.
--
-- Provides an animated chevron indicator and a clickable header
-- that pairs the chevron with arbitrary content.
module Competences.Frontend.View.Disclosure
  ( disclosureChevron
  , disclosureHeader
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Animated chevron indicator for expand/collapse.
-- Renders a @▶@ character that rotates 90° when @isExpanded@ is 'True'.
disclosureChevron :: Bool -> M.View m a
disclosureChevron isExpanded =
  MH.span_
    [class_ $ "transition-transform duration-200 " <> if isExpanded then "rotate-90" else ""]
    [M.text "▶"]

-- | Clickable disclosure header: chevron + content.
-- Expands the click target to the full parent width via negative margin.
-- The parent element should use @flex items-center gap-3@.
disclosureHeader :: a -> Bool -> [M.View m a] -> M.View m a
disclosureHeader toggleAction isExpanded content =
  MH.div_
    [ class_ "flex items-center gap-3 flex-1 cursor-pointer hover:bg-muted -m-3 p-3"
    , MH.onClick toggleAction
    ]
    (disclosureChevron isExpanded : content)
