module Competences.Frontend.Component.Selector.ListSelector
  ( singleListSelectorComponent
  , multiListSelectorComponent
  , listSelectorConfig
  , ListSelectorConfig (..)
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

data ListSelectorConfig a t = ListSelectorConfig
  { listValues :: !(Document -> [a])
  , showValue :: !(a -> M.MisoString)
  , isInitialValue :: !(a -> Bool)
  }

listSelectorConfig :: (Document -> [a]) -> (a -> M.MisoString) -> ListSelectorConfig a f
listSelectorConfig listValues showValue =
  ListSelectorConfig
    { listValues = listValues
    , showValue = showValue
    , isInitialValue = const False
    }

data SingleSelectionStyle
  = SButtons
  | SButtonsCompact
  | SComboBox
  | SShow
  deriving (Eq, Show)

data SingleModel a = SingleModel
  { possibleValues :: ![a]
  , selectedValue :: !(Maybe a)
  }
  deriving (Eq, Generic, Show)

data MultiSelectionStyle
  = MButtons
  | MButtonsCompact
  | MShow
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
  :: forall p a f t
   . (Eq a)
  => SyncDocumentRef
  -> ListSelectorConfig a t
  -> SingleSelectionStyle
  -> SelectorTransformedLens p Maybe a f t
  -> M.Component p (SingleModel a) (Action a)
singleListSelectorComponent r config style t =
  (M.component model update view)
    { M.bindings = [ mkSelectorBinding t #selectedValue ]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = SingleModel [] Nothing

    update (UpdateDocument (DocumentChange d info)) =
      let possibleValues = config.listValues d
       in M.modify $ \m ->
            let selectedValue
                  | isInitialUpdate info = find config.isInitialValue possibleValues
                  | otherwise = do
                                  v <- m ^. #selectedValue
                                  if v `elem` possibleValues then Just v else Nothing
            in m
              & (#possibleValues .~ possibleValues)
              & (#selectedValue .~ selectedValue)
    update (Toggle v) = M.modify (#selectedValue %~ \s -> if s == Just v then Nothing else Just v)

    view m = case style of
      SButtons -> viewButtons V.hButtons m.possibleValues config.showValue (\v -> Just v == m.selectedValue)
      SButtonsCompact ->
        viewButtons
          (V.hButtons & (#compact .~ True))
          m.possibleValues
          config.showValue
          (\v -> Just v == m.selectedValue)
      SComboBox -> V.empty
      SShow -> V.text_ $ maybe "" config.showValue m.selectedValue

multiListSelectorComponent
  :: forall p a f t
   . (Ord a, Show a)
  => SyncDocumentRef
  -> ListSelectorConfig a t
  -> MultiSelectionStyle
  -> SelectorTransformedLens p [] a f t
  -> M.Component p (MultiModel a) (Action a)
multiListSelectorComponent r config style s =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding s (O.castOptic #selectedValues)]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = MultiModel [] []

    update (UpdateDocument (DocumentChange d info)) =
      let possibleValues = config.listValues d
       in M.modify $ \m ->
            let selectedValues =
                  if isInitialUpdate info
                  then filter config.isInitialValue possibleValues
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

    view m = case style of
      MButtons -> viewButtons V.hButtons m.possibleValues config.showValue (`Set.member` Set.fromList m.selectedValues)
      MButtonsCompact ->
        viewButtons
          (V.hButtons & (#compact .~ True))
          m.possibleValues
          config.showValue
          (`Set.member` Set.fromList m.selectedValues)
      MShow -> V.text_ $ case m.selectedValues of
        [] -> ""
        _  -> foldl1 (\a b -> a <> ", " <> b) $ map config.showValue m.selectedValues

viewButtons
  :: forall a m. V.Buttons -> [a] -> (a -> M.MisoString) -> (a -> Bool) -> M.View m (Action a)
viewButtons buttons possibleValues showValue isSelected =
  V.viewButtons buttons $ map viewButton possibleValues
  where
    viewButton a = V.textButton (showValue a) (isSelected a) (Toggle a)
