-- | Reusable tab components using Basecoat's @.tabs@ CSS component.
--
-- Parameterised by a user-defined tab sum type so that 'tabSpec' and
-- 'tabContent' are total functions checked by the compiler.
--
-- @
-- Tabs.cardWithTabs Tabs.Tabs
--   { tabs = [TabA, TabB]
--   , activeTab = model.activeTab
--   , onSelect = SetTab
--   , tabSpec = \\case
--       TabA -> Tabs.TabSpec "Alpha" False
--       TabB -> Tabs.TabSpec "Beta"  False
--   , tabContent = \\case
--       TabA -> [M.text "Alpha content"]
--       TabB -> [M.text "Beta content"]
--   }
-- @
module Competences.Frontend.View.Tabs
  ( TabSpec (..)
  , Tabs (..)
  , tabs
  , cardWithTabs
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)

-- | Specification for a single tab.
data TabSpec = TabSpec
  { label :: !MisoString
  , disabled :: !Bool
  }

-- | Full tab-strip configuration.
data Tabs tab model action = Tabs
  { tabs :: [tab]
  , activeTab :: tab
  , onSelect :: tab -> action
  , tabSpec :: tab -> TabSpec
  , tabContent :: tab -> [M.View model action]
  }

-- | Standalone tabs (tablist + tabpanel, no surrounding card).
tabs :: (Eq tab) => Tabs tab model action -> M.View model action
tabs cfg =
  MH.div_
    [class_ "tabs"]
    [ viewTabList cfg
    , viewTabPanel cfg
    ]

-- | Card with tabs integrated into the top border.
--
-- The card wraps the Basecoat @.tabs@ component, overriding the
-- tablist to stretch full-width with a bottom border separator.
cardWithTabs :: (Eq tab) => Tabs tab model action -> M.View model action
cardWithTabs cfg =
  MH.div_
    [class_ "bg-card text-card-foreground rounded-xl border border-border shadow-sm overflow-hidden"]
    [ MH.div_
        [class_ "tabs"]
        [ viewTabList cfg
        , viewTabPanel cfg
        ]
    ]

-- | Render the tablist row.
viewTabList :: (Eq tab) => Tabs tab model action -> M.View model action
viewTabList cfg =
  MH.div_
    [MP.role_ "tablist", class_ "w-full flex rounded-none border-b border-border"]
    (map (viewTab cfg) cfg.tabs)

-- | Render a single tab button.
viewTab :: (Eq tab) => Tabs tab model action -> tab -> M.View model action
viewTab cfg tab =
  let spec = cfg.tabSpec tab
      isActive = tab == cfg.activeTab
      ariaAttrs = [M.textProp "aria-selected" (if isActive then "true" else "false")]
      disabledAttr = [class_ "opacity-50 cursor-not-allowed" | spec.disabled]
      clickAttr = [MH.onClick (cfg.onSelect tab) | not spec.disabled && not isActive]
      baseClass = "flex-1"
   in MH.button_
        ( [MP.role_ "tab", class_ baseClass]
            <> ariaAttrs
            <> disabledAttr
            <> clickAttr
        )
        [M.text spec.label]

-- | Render the active tab's content panel.
viewTabPanel :: (Eq tab) => Tabs tab model action -> M.View model action
viewTabPanel cfg =
  MH.div_
    [MP.role_ "tabpanel", class_ "p-6 space-y-4"]
    (cfg.tabContent cfg.activeTab)
