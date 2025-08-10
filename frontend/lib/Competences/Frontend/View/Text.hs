module Competences.Frontend.View.Text
  ( buttonText_
  , text_
  , title_
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import Miso qualified as M
import Miso.String qualified as M

title_ :: M.MisoString -> M.View m a
title_ t = M.span_ [T.tailwind [T.TextXl, T.FontBold, T.TextCenter]] [M.text t]

text_ :: M.MisoString -> M.View m a
text_ t = M.span_ [T.tailwind [T.TextSm]] [M.text_ [t]]

buttonText_ :: M.MisoString -> M.View m a
buttonText_ t = M.span_ [T.tailwind [T.ButtonText]] [M.text t]
