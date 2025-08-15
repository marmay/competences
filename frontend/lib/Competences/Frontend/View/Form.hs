module Competences.Frontend.View.Form
  ( form_
  , formField_
  , textarea_
  )
where

import Competences.Frontend.View.Layout qualified as V
import Competences.Frontend.View.Text qualified as V
import Miso qualified as M
import qualified Competences.Frontend.View.Tailwind as T

form_ :: M.MisoString -> [M.View m a] -> [M.View m a] -> M.View m a
form_ title fields buttons =
  V.withMargin_ $ V.vBox_ V.NoExpand V.NoExpand V.SmallGap
    [ V.title_ title
    , V.withMargin_ $ V.vBox_ V.NoExpand V.NoExpand V.SmallGap fields
    , V.hBox_ (V.Expand V.Center) V.NoExpand V.MediumGap buttons
    ]

formField_ :: M.MisoString -> M.View m a -> M.View m a
formField_ label field =
  V.hBox_ (V.Expand V.Start) V.NoExpand V.SmallGap
    [ label_ label
    , field
    ]
  where
    label_ :: M.MisoString -> M.View m a
    label_ t = M.span_ [ T.tailwind [T.WThird] ] [V.text_ t]

textarea_ :: [M.Attribute a] -> M.View m a
textarea_ as = M.textarea_ (T.tailwind [T.FlexGrow, T.RegularBorder] : as) []
