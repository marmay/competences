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
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , isInitialUpdate
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Data.List (find)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core ((%~), (&), (.~), (?~), (^.))
import Optics.Core qualified as O

data ListSelectorConfig a t = ListSelectorConfig
  { listValues :: !(Document -> [a])
  , showValue :: !(a -> M.MisoString)
  , isInitialValue :: !(a -> Bool)
  , showSelectAll :: !Bool
  -- ^ Whether to show select all button (for multi-select)
  }

listSelectorConfig :: (Document -> [a]) -> (a -> M.MisoString) -> ListSelectorConfig a f
listSelectorConfig listValues showValue =
  ListSelectorConfig
    { listValues = listValues
    , showValue = showValue
    , isInitialValue = const False
    , showSelectAll = True
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
  | Set a
  | Reset
  deriving (Eq, Show)

singleListSelectorComponent
  :: forall p a f t
   . (Eq a)
  => SyncContext
  -> ListSelectorConfig a t
  -> SingleSelectionStyle
  -> SelectorTransformedLens p Maybe a f t
  -> M.Component p (SingleModel a) (Action a)
singleListSelectorComponent r config style t =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding t #selectedValue]
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
    update (Set v) = M.modify (#selectedValue ?~ v)
    update Reset = M.modify (#selectedValue .~ Nothing)

    view m = case style of
      SButtons -> viewToggleButtons False m.possibleValues config.showValue (\v -> Just v == m.selectedValue)
      SButtonsCompact ->
        viewToggleButtons
          True
          m.possibleValues
          config.showValue
          (\v -> Just v == m.selectedValue)
      SComboBox ->
        M.select_
          [M.onInput findValue]
          ( M.option_ [M.value_ ""] [M.text_ [C.translate' C.LblPleaseSelectItemShort]]
              : map mkOption m.possibleValues
          )
        where
          mkOption v = let s = config.showValue v in M.option_ [M.value_ s] [M.text_ [s]]
          findValue s = case find (\v -> config.showValue v == s) m.possibleValues of
            (Just v) -> Set v
            Nothing -> Reset
      SShow -> V.text_ $ maybe "" config.showValue m.selectedValue

multiListSelectorComponent
  :: forall p a f t
   . (Ord a, Show a)
  => SyncContext
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
    update (Set a) = M.modify $ #selectedValues .~ [a]
    update Reset = M.modify $ #selectedValues .~ []

    view m = case style of
      MButtons ->
        viewToggleButtons
          False
          m.possibleValues
          config.showValue
          (`Set.member` Set.fromList m.selectedValues)
      MButtonsCompact ->
        viewToggleButtons
          True
          m.possibleValues
          config.showValue
          (`Set.member` Set.fromList m.selectedValues)
      MShow -> V.text_ $ case m.selectedValues of
        [] -> ""
        _ -> foldl1 (\a b -> a <> ", " <> b) $ map config.showValue m.selectedValues

-- | Render toggle buttons as a button group
-- compact=True uses buttonGroup (connected edges), False uses flow layout
viewToggleButtons
  :: forall a m. Bool -> [a] -> (a -> M.MisoString) -> (a -> Bool) -> M.View m (Action a)
viewToggleButtons compact possibleValues showValue isSelected =
  (if compact then Button.buttonGroup else V.viewFlow V.hFlow) (map mkButton possibleValues)
  where
    mkButton a = Button.toggleButton (isSelected a) (showValue a) (Toggle a)
