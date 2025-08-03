module Competences.Frontend.Common.Style
  ( ClassName (..)
  , styleSheet
  , styledClass
  , styledClasses
  , styledMetaClass
  )
where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Miso (Attribute)
import Miso.Html (class_)
import Miso.String (MisoString)
import Miso.String qualified as M
import Miso.Style qualified as M

data ClassName
  = -- | Container for the entire app
    ClsApp
  | -- | Base class for all kinds of buttons
    ClsButton
  | -- | Face, size and weight of title fonts
    ClsFontTitle
  | -- | Face, size and weight of subtitle fonts
    ClsFontSubTitle
  | -- | Face, size and weight of regular text
    ClsFontRegular
  | ClsFontStyleHighlighted
  | ClsFontStylePlaceholder
  | -- | Container for modal dialogs
    ClsModal
  | ClsCompetences
  | ClsCompetenceDescription
  | ClsDescription
  | ClsEditableContainer
  | ClsEditableContent
  | ClsEditableButtons
  | ClsNewCompetenceRow
  | -- | Base class for all icons
    ClsIcon
  | ClsLabelButton
  | ClsTitle
  | ClsCompetenceGridTable
  | ClsSingleActionColumn
  | ClsCompetenceDescriptionColumn
  | ClsCompetenceLevelDescriptionColumn
  | ClsFill
  deriving (Bounded, Enum, Eq, Ord, Show)

data MetaClass
  = ClsCompetenceTitle
  deriving (Eq, Show)

toClasses :: MetaClass -> [ClassName]
toClasses _ = []

styledClass :: ClassName -> Attribute action
styledClass = class_ . className

styledClasses :: [ClassName] -> [Attribute action]
styledClasses = map styledClass

styledMetaClass :: MetaClass -> [Attribute action]
styledMetaClass = styledClasses . toClasses

className :: ClassName -> MisoString
className c =
  fromMaybe "cls-unknown" $ Map.lookup c m
  where
    m :: Map.Map ClassName MisoString
    m = Map.fromList $ zipWith (\c' n -> (c', classId n)) [minBound ..] [0 ..]
    classId :: Int -> MisoString
    classId n = M.toMisoString $ "cls-" <> show n

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
    , M.selector_
        (classSelector ClsModal)
        [ M.zIndex "1"
        , M.position "absolute"
        , M.left "0px"
        , M.top "0px"
        , M.width "100%"
        , M.height "100%"
        , M.backgroundColor $ M.rgba 0 0 0 0.5
        ]
    , M.selector_
        (classSelector ClsCompetenceGridTable)
        [ M.width "100%"
        , ("table-layout", "fixed")
        , ("border-collapse", "collapse")
        , M.borderWidth "1px"
        , M.borderStyle "solid"
        ]
    , M.selector_
        ( classSelector ClsCompetenceGridTable
            <> " td, "
            <> classSelector ClsCompetenceGridTable
            <> " tr, "
            <> classSelector ClsCompetenceGridTable
            <> " th"
        )
        [ M.borderStyle "solid"
        , M.borderWidth (M.px 1)
        , M.height (M.px 1)
        ]
    , M.selector_
        (classSelector ClsSingleActionColumn)
        [M.width "24px"]
    , M.selector_
        (classSelector ClsCompetenceDescriptionColumn)
        [M.width "34%"]
    , M.selector_
        (classSelector ClsCompetenceLevelDescriptionColumn)
        [M.width "22%"]
    , M.selector_
        (classSelector ClsFill)
        [ M.width (M.pct 100)
        , M.height (M.pct 100)
        ]
    ]
