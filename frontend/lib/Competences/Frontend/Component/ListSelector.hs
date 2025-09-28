module Competences.Frontend.Component.ListSelector
  ( listSelectorComponent
  )
where

import Competences.Document (Document)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', (%~), (&), (.~))

data SelectionMode
  = SingleComboBox
  | SingleButtons !Bool
  | MultipleButtons !Bool
  deriving (Eq, Show)

isSingleSelection :: SelectionMode -> Bool
isSingleSelection SingleComboBox = True
isSingleSelection (SingleButtons _) = True
isSingleSelection (MultipleButtons _) = False

data Model a = Model
  { possibleValues :: ![a]
  , selectedValues :: !(Set.Set a)
  }
  deriving (Eq, Generic, Show)

data Action a
  = UpdateDocument !DocumentChange
  | Toggle a
  deriving (Eq, Show)

listSelectorComponent
  :: forall a p
   . (Ord a)
  => SyncDocumentRef
  -> (Document -> [a])
  -> (a -> M.MisoString)
  -> Lens' p [a]
  -> SelectionMode
  -> M.Component p (Model a) (Action a)
listSelectorComponent r getValues showValue l selectionMode =
  M.component model update view
  where
    model = Model [] Set.empty

    update (UpdateDocument (DocumentChange d _)) =
      let possibleValues = getValues d
       in M.modify $ \m ->
            m
              & (#possibleValues .~ possibleValues)
              & (#selectedValues %~ (`Set.intersection` Set.fromList possibleValues))

    view = case selectionMode of
      SingleComboBox -> viewSingleComboBox
      SingleButtons compact -> viewButtons compact
      MultipleButtons compact -> viewButtons compact

    viewSingleComboBox _m = M.text_ [M.ms "Not implemented"]
    viewButtons c m =
      V.viewButtons (V.hButtons & (#compact .~ c)) $ map (viewButton m.selectedValues) m.possibleValues
    viewButton selectedValues v =
      V.textButton (showValue v) s a
      where
        s = Set.member v selectedValues
        a = Toggle v
