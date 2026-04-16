-- | Reusable entity context menu (hover dropdown with ⋮ trigger).
module Competences.Frontend.View.EntityMenu
  ( EntityMenuConfig (..)
  , entityMenu
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Miso qualified as M

data EntityMenuConfig a = EntityMenuConfig
  { onEdit :: !(Maybe a)
  , onPin :: !(Maybe a)
  , onGoTo :: !(Maybe a)
  , onDelete :: !(Maybe a)
  }

entityMenu :: EntityMenuConfig a -> M.View m a
entityMenu cfg =
  let trigger = Icon.iconVS Icon.Ghost Icon.Small Icon.IcnMoreVertical
      primary =
        [ HoverMenu.hoverMenuEntry True Icon.IcnEdit (C.translate' C.LblEdit) a | Just a <- [cfg.onEdit]
        ]
          <> [ HoverMenu.hoverMenuEntry True Icon.IcnPin (C.translate' C.LblPin) a | Just a <- [cfg.onPin]
             ]
          <> [ HoverMenu.hoverMenuEntry True Icon.IcnOpenModal (C.translate' C.LblGoTo) a | Just a <- [cfg.onGoTo]
             ]
      destructive =
        [ HoverMenu.hoverMenuEntry True Icon.IcnDelete (C.translate' C.LblDelete) a | Just a <- [cfg.onDelete]
        ]
      items = case destructive of
        [] -> primary
        _ -> primary <> [HoverMenu.hoverMenuSeparator] <> destructive
   in HoverMenu.hoverMenuRight trigger items
