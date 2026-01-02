module Competences.Frontend.View.Text
  ( buttonText_
  , coloredText_
  , text_
  , title_
  )
where

import Competences.Frontend.View.Tailwind (class_, tailwindColors, Color, ColorStep, Opacity, ColorUtility(Text))
import Miso qualified as M
import Miso.Html qualified as M

title_ :: M.MisoString -> M.View m a
title_ t = M.span_ [class_ "text-xl font-bold text-center"] [M.text t]

text_ :: M.MisoString -> M.View m a
text_ t = M.span_ [class_ "text-sm"] [M.text_ [t]]

coloredText_ :: (Color, ColorStep, Opacity) -> M.MisoString -> M.View m a
coloredText_ (c, s, o) t = M.span_ [class_ "text-sm", tailwindColors [(Text, c, s, o)]] [M.text_ [t]]

buttonText_ :: M.MisoString -> M.View m a
buttonText_ t = M.span_ [class_ "text-sm/5"] [M.text t]
