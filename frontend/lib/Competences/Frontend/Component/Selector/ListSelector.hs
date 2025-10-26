module Competences.Frontend.Component.Selector.ListSelector
  ( singleListSelectorComponent
  , multiListSelectorComponent
  , SingleSelectionStyle (..)
  , MultiSelectionStyle (..)
  , SingleModel
  , MultiModel
  , Action
  )
where

import Competences.Document (Document)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', (%~), (&), (.~))
import Optics.Core qualified as O

data SingleSelectionStyle
  = SButtons
  | SButtonsCompact
  deriving (Eq, Show)

data SingleModel a = SingleModel
  { possibleValues :: ![a]
  , selectedValue :: !(Maybe a)
  }
  deriving (Eq, Generic, Show)

data MultiSelectionStyle
  = MButtons
  | MButtonsCompact
  deriving (Eq, Show)

data MultiModel a = MultiModel
  { possibleValues :: ![a]
  , selectedValues :: ![a]
  }
  deriving (Eq, Generic, Show)

data Action a
  = UpdateDocument !DocumentChange
  | Toggle a
  deriving (Eq, Show)

singleListSelectorComponent
  :: forall a p
   . (Eq a)
  => SyncDocumentRef
  -> (Document -> [a])
  -> (a -> M.MisoString)
  -> Lens' p (Maybe a)
  -> SingleSelectionStyle
  -> M.Component p (SingleModel a) (Action a)
singleListSelectorComponent r getValues showValue parentLens style =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #selectedValue]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = SingleModel [] Nothing

    update (UpdateDocument (DocumentChange d _)) =
      let possibleValues = getValues d
       in M.modify $ \m ->
            m
              & (#possibleValues .~ possibleValues)
              & ( #selectedValue %~ \case
                    Nothing -> Nothing
                    Just v -> if v `elem` possibleValues then Just v else Nothing
                )
    update (Toggle v) = M.modify (#selectedValue %~ \s -> if s == Just v then Nothing else Just v)

    view m = case style of
      SButtons -> viewButtons V.hButtons m.possibleValues showValue (\v -> Just v == m.selectedValue)
      SButtonsCompact ->
        viewButtons
          (V.hButtons & (#compact .~ True))
          m.possibleValues
          showValue
          (\v -> Just v == m.selectedValue)

multiListSelectorComponent
  :: forall a p
   . (Ord a)
  => SyncDocumentRef
  -> (Document -> [a])
  -> (a -> M.MisoString)
  -> Lens' p [a]
  -> MultiSelectionStyle
  -> M.Component p (MultiModel a) (Action a)
multiListSelectorComponent r getValues showValue parentLens selectionMode =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #selectedValues]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = MultiModel [] []

    update (UpdateDocument (DocumentChange d _)) =
      let possibleValues = getValues d
       in M.modify $ \m ->
            m
              & (#possibleValues .~ possibleValues)
              & (#selectedValues %~ filter (`Set.member` Set.fromList possibleValues))
    update (Toggle a) = M.modify $ \m ->
      let selected = Set.fromList m.selectedValues
       in m
            & ( #selectedValues
                  .~ Set.toList
                    ( if a `Set.member` selected
                        then Set.delete a selected
                        else Set.insert a selected
                    )
              )

    view m = case selectionMode of
      MButtons -> viewButtons V.hButtons m.possibleValues showValue (`Set.member` Set.fromList m.selectedValues)
      MButtonsCompact ->
        viewButtons
          (V.hButtons & (#compact .~ True))
          m.possibleValues
          showValue
          (`Set.member` Set.fromList m.selectedValues)

viewButtons
  :: forall a m. V.Buttons -> [a] -> (a -> M.MisoString) -> (a -> Bool) -> M.View m (Action a)
viewButtons buttons possibleValues showValue isSelected =
  V.viewButtons buttons $ map viewButton possibleValues
  where
    viewButton a = V.textButton (showValue a) (isSelected a) (Toggle a)
