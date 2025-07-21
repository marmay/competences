module Competences.Frontend.Common.Button
  ( iconButton
  , iconLabelButton
  )
where

import Competences.Frontend.Common.Icon (Icon, icon)
import Competences.Frontend.Common.Style
import Miso (Attribute, View, text)
import Miso.Html (button_, title_)
import Miso.String (MisoString)

iconButton :: [Attribute action] -> Icon -> MisoString -> View action
iconButton attrs iconId label =
  button_ (styledClass ClsButton : title_ label : attrs) [icon iconId]

iconLabelButton :: [Attribute action] -> Icon -> MisoString -> View action
iconLabelButton attrs iconId label =
  button_ (styledClass ClsLabelButton : attrs) [icon iconId, text label]
