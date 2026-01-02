module Competences.Frontend.Component.Selector.EnumSelector
  ( enumSelectorComponent
  , enumSelectorComponent'
  , SelectionStyle (..)
  )
where

import Competences.Frontend.View.Button qualified as Button
import Data.List.NonEmpty (NonEmpty (..), toList)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', (.~))
import Optics.Core qualified as O

data SelectionStyle
  = ButtonsCompact
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

    mkButton s a = Button.toggleButton (a == s) (showValue a) (Select a)

    choices' = if defaultChoice `elem` choices then choices else defaultChoice : choices
