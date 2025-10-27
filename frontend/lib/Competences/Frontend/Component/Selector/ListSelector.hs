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
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument, isInitialUpdate)
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((%~), (&), (.~), (^.))
import Optics.Core qualified as O
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)
import Data.List (find)

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
  -> (a -> Bool)
  -> SingleSelectionStyle
  -> M.Component p (SingleModel a) (Action a)
singleListSelectorComponent r getValues showValue t selectInitialValue style =
  (M.component model update view)
    { M.bindings = [ mkSelectorBinding t #selectedValue ]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = SingleModel [] Nothing

    update (UpdateDocument (DocumentChange d info)) =
      let possibleValues = getValues d
       in M.modify $ \m ->
            let selectedValue
                  | isInitialUpdate info = find selectInitialValue possibleValues
                  | otherwise = do
                                  v <- m ^. #selectedValue
                                  if v `elem` possibleValues then Just v else Nothing
            in m
              & (#possibleValues .~ possibleValues)
              & (#selectedValue .~ selectedValue)
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
  -> (a -> Bool)
  -> MultiSelectionStyle
  -> M.Component p (MultiModel a) (Action a)
multiListSelectorComponent r getValues showValue s selectInitialValues selectionMode =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding s (O.castOptic #selectedValues)]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = MultiModel [] []

    update (UpdateDocument (DocumentChange d info)) =
      let possibleValues = getValues d
       in M.modify $ \m ->
            let selectedValues =
                  if isInitialUpdate info
                  then filter selectInitialValues possibleValues
                  else filter (`Set.member` Set.fromList possibleValues) (m ^. #selectedValues)
            in m
              & (#possibleValues .~ possibleValues)
              & (#selectedValues .~ selectedValues)
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
