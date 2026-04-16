-- | Reusable entity context menu (hover dropdown with ⋮ trigger).
module Competences.Frontend.View.EntityMenu
  ( EntityMenuEntry (..)
  , EntityMenuStyle (..)
  , menuEdit
  , menuPin
  , menuGoTo
  , menuDelete
  , entityMenu
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Data.List (partition)
import Miso qualified as M
import Miso.String (MisoString)

data EntityMenuStyle = MenuPrimary | MenuDestructive
  deriving (Eq, Show)

data EntityMenuEntry a = EntityMenuEntry
  { style :: !EntityMenuStyle
  , icon :: !Icon.Icon
  , label :: !MisoString
  , action :: !a
  }

menuEdit :: a -> EntityMenuEntry a
menuEdit = EntityMenuEntry MenuPrimary Icon.IcnEdit (C.translate' C.LblEdit)

menuPin :: a -> EntityMenuEntry a
menuPin = EntityMenuEntry MenuPrimary Icon.IcnPin (C.translate' C.LblPin)

menuGoTo :: a -> EntityMenuEntry a
menuGoTo = EntityMenuEntry MenuPrimary Icon.IcnOpenModal (C.translate' C.LblGoTo)

menuDelete :: a -> EntityMenuEntry a
menuDelete = EntityMenuEntry MenuDestructive Icon.IcnDelete (C.translate' C.LblDelete)

entityMenu :: [EntityMenuEntry a] -> M.View m a
entityMenu entries =
  let trigger = Icon.iconVS Icon.Ghost Icon.Small Icon.IcnMoreVertical
      (primary, destructive) = partition (\e -> e.style == MenuPrimary) entries
      renderEntry e = HoverMenu.hoverMenuEntry True e.icon e.label e.action
      items = case destructive of
        [] -> map renderEntry primary
        _ -> map renderEntry primary <> [HoverMenu.hoverMenuSeparator] <> map renderEntry destructive
   in HoverMenu.hoverMenuRight trigger items
