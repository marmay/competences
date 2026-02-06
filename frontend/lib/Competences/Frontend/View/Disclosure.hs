-- | Stateless disclosure (expand/collapse) rendering helpers.
--
-- Provides a chevron indicator and collapsible card components
-- for bordered, expandable sections with clickable headers.
module Competences.Frontend.View.Disclosure
  ( disclosureChevron
  , collapsible
  , collapsibleStyled
  , collapsibleWithActions
  , collapsibleWithActionsStyled
  )
where

import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Chevron indicator for expand/collapse.
-- Shows a right-pointing arrow when collapsed, down-pointing when expanded.
disclosureChevron :: Bool -> M.View m a
disclosureChevron isExpanded =
  Icon.icon [] (if isExpanded then Icon.IcnArrowDown else Icon.IcnExpandShrinkArrowRight)

-- | Collapsible card: bordered container, clickable header with chevron, conditional content.
collapsible :: Bool -> a -> M.View m a -> M.View m a -> M.View m a
collapsible = collapsibleStyled "bg-muted/50"

-- | Like 'collapsible' but with a custom header background class.
collapsibleStyled :: Text -> Bool -> a -> M.View m a -> M.View m a -> M.View m a
collapsibleStyled headerBg isExpanded toggleAction title content =
  MH.div_
    [class_ "border rounded-lg overflow-hidden"]
    [ MH.div_
        [ class_ $ "flex items-center gap-2 px-3 py-2 cursor-pointer hover:bg-muted transition-colors " <> headerBg
        , MH.onClick toggleAction
        ]
        [ disclosureChevron isExpanded
        , title
        ]
    , if isExpanded
        then MH.div_ [class_ "px-3 py-2 border-t"] [content]
        else M.text ""
    ]

-- | Collapsible card with action buttons in the header (right-aligned).
-- Buttons should use @stopPropagation@ since the entire header is clickable.
collapsibleWithActions :: Bool -> a -> M.View m a -> [M.View m a] -> M.View m a -> M.View m a
collapsibleWithActions = collapsibleWithActionsStyled "bg-muted/50"

-- | Like 'collapsibleWithActions' but with a custom header background class.
collapsibleWithActionsStyled :: Text -> Bool -> a -> M.View m a -> [M.View m a] -> M.View m a -> M.View m a
collapsibleWithActionsStyled headerBg isExpanded toggleAction title actions content =
  MH.div_
    [class_ "border rounded-lg overflow-hidden"]
    [ MH.div_
        [ class_ $ "flex items-center gap-3 px-3 py-2 cursor-pointer hover:bg-muted transition-colors " <> headerBg
        , MH.onClick toggleAction
        ]
        [ disclosureChevron isExpanded
        , MH.div_ [class_ "flex-1"] [title]
        , MH.div_ [class_ "flex gap-1"] actions
        ]
    , if isExpanded
        then MH.div_ [class_ "px-3 py-2 border-t"] [content]
        else M.text ""
    ]
