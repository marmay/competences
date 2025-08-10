module Competences.Frontend.View.Button
  ( iconButton
  , iconLabelButton
  , buttonRow
  , buttonColumn
  , applyButton
  , cancelButton
  , applyLabelButton
  , deleteButton
  , editButton
  , cancelLabelButton
  , ButtonStyle (..)
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Layout qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Competences.Frontend.View.Text qualified as V
import Miso qualified as M
import Miso.Html (button_, title_)
import Miso.String (MisoString)

iconButton :: [M.Attribute action] -> ButtonStyle -> Icon -> MisoString -> M.View m action
iconButton attrs s iconId label =
  button_ (attributesFor s [T.IconButton, T.SizeFit] : title_ label : attrs) [icon [] iconId]

buttonRow :: [M.View m action] -> M.View m action
buttonRow = V.hBox_ (V.Expand V.Center) V.NoExpand V.TinyGap

buttonColumn :: [M.View m action] -> M.View m action
buttonColumn = V.vBox_ (V.Expand V.Center) V.NoExpand V.TinyGap

iconLabelButton :: [M.Attribute action] -> ButtonStyle -> Icon -> MisoString -> M.View m action
iconLabelButton attrs s iconId label =
  button_
    (attributesFor s [T.IconLabelButton, T.SizeFit] : attrs)
    [V.hBox_ V.NoExpand (V.Expand V.End) V.SmallGap [icon [] iconId, V.buttonText_ label]]

applyButton :: [M.Attribute action] -> M.View m action
applyButton attrs = iconButton attrs RegularButton IcnApply (translate' LblApply)

cancelButton :: [M.Attribute action] -> M.View m action
cancelButton attrs = iconButton attrs AlertButton IcnCancel (translate' LblCancel)

deleteButton :: [M.Attribute action] -> M.View m action
deleteButton attrs = iconButton attrs AlertButton IcnDelete (translate' LblDelete)

editButton :: [M.Attribute action] -> M.View m action
editButton attrs = iconButton attrs RegularButton IcnEdit (translate' LblEdit)

applyLabelButton :: [M.Attribute action] -> M.View m action
applyLabelButton attrs = iconLabelButton attrs RegularButton IcnApply (translate' LblApply)

cancelLabelButton :: [M.Attribute action] -> M.View m action
cancelLabelButton attrs = iconLabelButton attrs AlertButton IcnCancel (translate' LblCancel)

data ButtonStyle
  = RegularButton
  | AlertButton
  deriving (Eq, Show)

attributesFor :: ButtonStyle -> [T.TailwindCls] -> M.Attribute action
attributesFor RegularButton clss = T.tailwind $ T.RegularButtonColors : clss
attributesFor AlertButton clss = T.tailwind $ T.AlertButtonColors : clss
