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
import Optics.Core (Setter', (%~), (&), (.~), (^.))
import Optics.Core qualified as O
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)

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
  :: forall p a t
   . (Eq a)
  => SyncDocumentRef
  -> (Document -> [a])
  -> (a -> M.MisoString)
  -> SelectorTransformedLens p (Maybe a) t 
  -> SingleSelectionStyle
  -> M.Component p (SingleModel a) (Action a)
singleListSelectorComponent r getValues showValue t style =
  (M.component model update view)
    { M.bindings = [ mkSelectorBinding t #selectedValue ]
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
  :: forall p a t
   . (Ord a)
  => SyncDocumentRef
  -> (Document -> [a])
  -> (a -> M.MisoString)
  -> SelectorTransformedLens p [a] t
  -> MultiSelectionStyle
  -> M.Component p (MultiModel a) (Action a)
multiListSelectorComponent r getValues showValue s selectionMode =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding s (O.castOptic #selectedValues)]
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
