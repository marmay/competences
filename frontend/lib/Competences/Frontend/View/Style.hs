module Competences.Frontend.View.Style
  ( ClassName(..)
  , styleSheet
  , styledClass
  )
where

import Miso (Attribute)
import Miso.Html (class_)
import Miso.String (MisoString)
import Miso.Style qualified as M

data ClassName
  = ClsButton
  | ClsIcon
  deriving (Bounded, Enum, Eq, Show)

styledClass :: ClassName -> Attribute action
styledClass = class_ . className

className :: ClassName -> MisoString
className = \case
  ClsButton -> "button"
  ClsIcon -> "icon"

classSelector :: ClassName -> MisoString
classSelector = ("." <>) . className

styleSheet :: M.StyleSheet
styleSheet =
  M.sheet_
    [ M.selector_ (classSelector ClsButton)
      [ M.borderWidth "1px"
      , M.borderStyle "solid"
      , M.borderColor (M.rgb 0 0 0)
      , M.borderRadius "50%"]
    , M.selector_ (classSelector ClsIcon)
      [ M.display "inline-block"
      , M.width "24px"
      , M.height "24px"
      ]
    ]
