module Competences.Frontend.Component.Selector.EnumSelector
  ( enumSelectorComponent
  , enumSelectorComponent'
  , SelectionStyle (..)
  )
where

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (fromMisoString)
import Optics.Core (Lens', (.~))
import Optics.Core qualified as O

data SelectionStyle
  = ButtonsCompact
  | SelectDropdown
  deriving (Eq, Show)

newtype Model a = Model
  { selected :: a
  }
  deriving (Eq, Generic, Show)

newtype Action a
  = Select a
  deriving (Eq, Show)

enumSelectorComponent
  :: (Eq a)
  => NonEmpty a
  -> SelectionStyle
  -> (a -> M.MisoString)
  -> Lens' p a
  -> M.Component p (Model a) (Action a)
enumSelectorComponent choices@(defaultChoice :| _) =
  enumSelectorComponent' defaultChoice (toList choices)

enumSelectorComponent'
  :: (Eq a)
  => a
  -> [a]
  -> SelectionStyle
  -> (a -> M.MisoString)
  -> Lens' p a
  -> M.Component p (Model a) (Action a)
enumSelectorComponent' defaultChoice choices style showValue parentLens =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<---> O.toLensVL #selected]
    }
  where
    model = Model defaultChoice

    update (Select a) = M.modify $ #selected .~ a

    view m = case style of
      ButtonsCompact ->
        Button.buttonGroup (map (mkButton m.selected) choices')
      SelectDropdown ->
        M.select_
          [ class_ "w-full h-8 rounded-md border border-input bg-background px-2 text-sm"
          , M.onChange (\v -> Select (lookupByLabel (fromMisoString v)))
          ]
          (map (mkOption m.selected) choices')

    mkButton s a = Button.toggleSm (a == s) (Button.button (showValue a) (Select a))

    mkOption s a =
      M.option_
        ( [ M.textProp "value" (showValue a) ]
          <> [ M.boolProp "selected" True | a == s ]
        )
        [M.text (showValue a)]

    lookupByLabel v = Map.findWithDefault defaultChoice v labelMap

    labelMap = Map.fromList [(showValue a, a) | a <- choices']

    choices' = if defaultChoice `elem` choices then choices else defaultChoice : choices
