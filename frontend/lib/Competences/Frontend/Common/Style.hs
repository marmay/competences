module Competences.Frontend.Common.Style
  ( ClassName (..)
  , styleSheet
  , styledClass
  )
where

import Miso (Attribute)
import Miso.Html (class_)
import Miso.String (MisoString)
import Miso.Style qualified as M

data ClassName
  = ClsApp
  | ClsButton
  | ClsDescription
  | ClsEditableContainer
  | ClsEditableContent
  | ClsEditableButtons
  | ClsIcon
  | ClsLabelButton
  | ClsTitle
  deriving (Bounded, Enum, Eq, Show)

styledClass :: ClassName -> Attribute action
styledClass = class_ . className

className :: ClassName -> MisoString
className = \case
  ClsApp -> "app"
  ClsButton -> "button"
  ClsDescription -> "description"
  ClsEditableContainer -> "editable-container"
  ClsEditableContent -> "editable-content"
  ClsEditableButtons -> "editable-buttons"
  ClsIcon -> "icon"
  ClsLabelButton -> "label-button"
  ClsTitle -> "title"

classSelector :: ClassName -> MisoString
classSelector = ("." <>) . className

styleSheet :: M.StyleSheet
styleSheet =
  M.sheet_
    [ M.selector_
        (classSelector ClsButton)
        [ M.borderWidth "0px"
        , M.width "24px"
        , M.height "24px"
        , M.padding "0px"
        ]
    , M.selector_
        (classSelector ClsEditableContainer)
        [ M.display "flex"
        , M.flexDirection "row"
        , M.width "100%"
        ]
    , M.selector_
        (classSelector ClsEditableContent)
        [ M.flexGrow "1"
        ]
    , M.selector_
        (classSelector ClsEditableButtons)
        []
    , M.selector_
        (classSelector ClsIcon)
        [ M.display "inline-block"
        , M.margin "0px"
        , M.width "24px"
        , M.height "24px"
        ]
    , M.selector_
        (classSelector ClsTitle)
        [ M.fontSize "24px"
        , M.fontWeight "bold"
        , M.textAlign "center"
        ]
    ]
