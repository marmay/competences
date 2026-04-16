-- | Reusable entity context menu (hover dropdown with ⋮ trigger).
--
-- The stateful 'entityMenu' takes a dismissed flag and reset action.
-- The Embed layer sets dismissed after handling an entry action;
-- mouseLeave fires the reset.
module Competences.Frontend.View.EntityMenu
  ( menuEdit
  , menuPin
  , menuGoTo
  , menuDelete
  , menuCustom
  , menuSeparator
  , entityMenu
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Icon qualified as Icon
import Miso qualified as M
import Miso.String (MisoString)

menuEdit :: a -> M.View m a
menuEdit = HoverMenu.hoverMenuEntry True Icon.IcnEdit (C.translate' C.LblEdit)

menuPin :: a -> M.View m a
menuPin = HoverMenu.hoverMenuEntry True Icon.IcnPin (C.translate' C.LblPin)

menuGoTo :: a -> M.View m a
menuGoTo = HoverMenu.hoverMenuEntry True Icon.IcnOpenModal (C.translate' C.LblGoTo)

menuDelete :: a -> M.View m a
menuDelete = HoverMenu.hoverMenuEntry True Icon.IcnDelete (C.translate' C.LblDelete)

menuCustom :: Icon.Icon -> MisoString -> a -> M.View m a
menuCustom = HoverMenu.hoverMenuEntry True

menuSeparator :: M.View m a
menuSeparator = HoverMenu.hoverMenuSeparator

entityMenu :: Bool -> a -> [M.View m a] -> M.View m a
entityMenu dismissed resetAction items =
  let trigger = Icon.iconVS Icon.Ghost Icon.Small Icon.IcnMoreVertical
   in HoverMenu.hoverMenuStatefulRight dismissed resetAction trigger items
