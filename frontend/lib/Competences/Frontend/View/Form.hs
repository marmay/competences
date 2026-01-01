module Competences.Frontend.View.Form
  ( -- * Form layouts
    form_
  , formField_
  , formLayout

    -- * Input components (re-exported from View.Input for convenience)
  , Input.textInput
  , Input.passwordInput
  , Input.emailInput
  , Input.numberInput
  , Input.dateInput
  , Input.textarea
  , Input.label
  , Input.fieldWrapper
  , Input.fieldWrapperHorizontal
  , Input.helperText

    -- * Legacy (use View.Input instead)
  , textarea_
  )
where

import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout (formLayout)
import Competences.Frontend.View.Layout qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Competences.Frontend.View.Text qualified as V
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

form_ :: M.MisoString -> [M.View m a] -> [M.View m a] -> M.View m a
form_ title fields buttons =
  V.viewFlow
    ( V.vFlow
        & (#gap .~ V.SmallSpace)
    )
    [ V.title_ title
    , V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
        )
        fields
    , V.viewFlow
        ( V.hFlow
            & (#expandDirection .~ V.Expand V.Center)
            & (#gap .~ V.MediumSpace)
        )
        buttons
    ]

formField_ :: M.MisoString -> M.View m a -> M.View m a
formField_ label field =
  V.viewFlow
    ( V.hFlow
        & (#expandDirection .~ V.Expand V.Start)
        & (#gap .~ V.SmallSpace)
    )
    [label_ label, V.flexGrow field]
  where
    label_ :: M.MisoString -> M.View m a
    label_ t = M.span_ [T.tailwind [T.WThird, T.TextRight]] [V.text_ t]

textarea_ :: [M.Attribute a] -> M.View m a
textarea_ as = M.textarea_ (T.tailwind [T.FlexGrow, T.RegularBorder] : as) []
