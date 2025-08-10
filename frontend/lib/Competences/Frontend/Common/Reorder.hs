{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Competences.Frontend.Common.Reorder
  ( ReorderModel
  , initialReorderModel
  , ReorderAction
  , updateReorderModel
  , viewReorderItem
  )
where

import Competences.Command (Command)
import Competences.Document.Id (Id)
import Competences.Document.Order (Orderable, Reorder (..), idL)
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((.~), (^.), (?~))

newtype ReorderModel a = ReorderModel
  { reorderFrom :: Maybe (Id a)
  }
  deriving (Eq, Show, Generic)

initialReorderModel :: ReorderModel a
initialReorderModel = ReorderModel Nothing

data ReorderAction a
  = ReorderFrom !(Id a)
  | ReorderToTop
  | ReorderToBottom
  | ReorderUp
  | ReorderDown
  | ReorderBefore !(Id a)
  | ReorderAfter !(Id a)
  | CancelReorder
  deriving (Eq, Show, Generic)

type MkReorderCommand a = Id a -> Reorder a -> Maybe Command

updateReorderModel
  :: SyncDocumentRef
  -> MkReorderCommand a
  -> ReorderAction a
  -> M.Effect p (ReorderModel a) (ReorderAction a)
updateReorderModel _ _ (ReorderFrom id') =
  M.modify (#reorderFrom ?~ id')
updateReorderModel _ _ CancelReorder =
  M.modify $ #reorderFrom .~ Nothing
updateReorderModel r f ReorderToTop = issueReorderCommand r f Front
updateReorderModel r f ReorderToBottom = issueReorderCommand r f Back
updateReorderModel r f ReorderUp = issueReorderCommand r f Forward
updateReorderModel r f ReorderDown = issueReorderCommand r f Backward
updateReorderModel r f (ReorderBefore id') = issueReorderCommand r f (Before id')
updateReorderModel r f (ReorderAfter id') = issueReorderCommand r f (After id')

issueReorderCommand
  :: forall a p
   . SyncDocumentRef -> MkReorderCommand a -> Reorder a -> M.Effect p (ReorderModel a) (ReorderAction a)
issueReorderCommand r f to = do
  M.get >>= mapM_ (\c -> do
            M.io_ $ modifySyncDocument r c) . mkChangeOrderCommand
  M.modify $ #reorderFrom .~ Nothing
  where
    mkChangeOrderCommand :: ReorderModel a -> Maybe Command
    mkChangeOrderCommand m = do
      from <- m.reorderFrom
      f from to

viewReorderItem :: (Orderable a) => ReorderModel a -> a -> M.View m (ReorderAction a)
viewReorderItem m item =
  case m.reorderFrom of
    Nothing -> moveButton
    Just from ->
      M.div_ [] $
        if from == item ^. idL
          then [cancelButton]
          else [moveBeforeButton, moveAfterButton]
  where
    moveButton = M.button_ [M.onClick $ ReorderFrom $ item ^. idL] [M.text "Move"]
    cancelButton = M.button_ [M.onClick CancelReorder] [M.text "Cancel"]
    moveBeforeButton = M.button_ [M.onClick $ ReorderBefore $ item ^. idL] [M.text "Move before"]
    moveAfterButton = M.button_ [M.onClick $ ReorderAfter $ item ^. idL] [M.text "Move after"]
