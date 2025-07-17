module Competences.Frontend.View.Button
  ( iconButton
  )
where

import Competences.Frontend.View.Icon (Icon, icon)
import Miso (Attribute, View, text)
import Miso.String (MisoString)
import Competences.Frontend.View.Style
import Miso.Html (button_, title_)

iconButton :: [Attribute action] -> Icon -> MisoString -> View action
iconButton attrs iconId label =
  button_ (styledClass ClsButton : title_ label : attrs) [icon iconId]
