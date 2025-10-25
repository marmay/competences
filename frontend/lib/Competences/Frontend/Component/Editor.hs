module Competences.Frontend.Component.Editor
  ( Editor
  , editor
  , addField
  , addNamedField
  , textEditorField
  , dayEditorField
  , Editable (..)
  , editable
  , editorComponent
  , msIso
  )
where

import Competences.Command (Command, ModifyCommand (..))
import Competences.Common.IxSet qualified as IxSet
import Competences.Document (Document (..), User (..), UserId)
import Competences.Frontend.Component.Editor.Editable
  ( Editable (..)
  , editable
  , withDelete
  , withModify
  , withReorder
  )
import Competences.Frontend.Component.Editor.EditorField
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.Editor.View
  ( DeleteState (..)
  , EditState (..)
  , EditorView
  , EditorViewData (..)
  , EditorViewItem (..)
  , MoveState (..)
  )
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocument (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , readSyncDocument
  , subscribeDocument
  , syncDocumentEnv
  )
import Data.Foldable (for_, toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%), (%~), (&), (.~), (?~), (^.))

data Editor a f n = Editor
  { view :: !(EditorView a f n)
  , editable :: !(Editable f a)
  , fields :: ![(n, EditorField a f)]
  }
  deriving (Generic)

editor :: EditorView a f n -> Editable f a -> Editor a f n
editor v e = Editor {editable = e, view = v, fields = []}

addField :: Editor a f () -> EditorField a f -> Editor a f ()
addField e f = e & #fields %~ (<> [((), f)])

addNamedField :: Editor a f n -> (n, EditorField a f) -> Editor a f n
addNamedField e f = e & #fields %~ (<> [f])

editorComponent
  :: forall a f n p
   . (Functor f, Foldable f, Ord a)
  => Editor a f n -> SyncDocumentRef -> M.Component p (Model a f) (Action a)
editorComponent e r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Nothing Map.empty Nothing Map.empty

    runCommand :: Maybe Command -> M.Effect p (Model a f) (Action a)
    runCommand Nothing = pure ()
    runCommand (Just c) = M.io_ (modifySyncDocument r c)

    runCommand' :: (Document -> Maybe Command) -> M.Effect p (Model a f) (Action a)
    runCommand' f = M.io_ $ do
      d <- (.localDocument) <$> readSyncDocument r
      for_ (f d) $ \c -> modifySyncDocument r c

    update (StartEditing a) = runCommand $ withModify e.editable a Lock
    update (CancelEditing a) = runCommand $ withModify e.editable a (Release Nothing)
    update (FinishEditing a) = do
      patches <- (^. #patches) <$> M.get
      runCommand $ do
        p <- patches Map.!? a
        withModify e.editable a (Release $ Just (a, p))
    update (StartMoving a) = M.modify (#reorderFrom ?~ a)
    update CancelMoving = M.modify (#reorderFrom .~ Nothing)
    update (FinishMoving !reorderAction) = do
      reorderFrom <- (^. #reorderFrom) <$> M.get
      runCommand' $ \d -> do
        reorderFrom' <- reorderFrom
        withReorder e.editable d reorderFrom' reorderAction
      M.modify (#reorderFrom .~ Nothing)
    update (Delete !a) = runCommand $ withDelete e.editable a
    update (UpdatePatch original patched) =
      M.modify $ #patches %~ Map.insert original patched
    update (UpdateDocument (DocumentChange newDocument _)) =
      M.modify $ updateModel newDocument

    updateModel :: Document -> Model a f -> Model a f
    updateModel d (Model _ patches reorderFrom _) =
      let es = e.editable.get d
          myEdits = filter (\(_, u) -> u == Just (syncDocumentEnv r ^. #connectedUser % #id)) (toList es)
          patches' = Map.fromList $ map (\(e', _) -> (e', fromMaybe e' (Map.lookup e' patches))) myEdits
          users = Map.fromList $ map (\u -> (u ^. #id, u)) (IxSet.toList $ d ^. #users)
       in Model (Just es) patches' reorderFrom users

    view :: Model a f -> M.View (Model a f) (Action a)
    view m = case m.entries of
      (Just entries) ->
        e.view $
          EditorViewData
            { fields = map fst e.fields
            , items = fmap (viewItem m) entries
            }
      Nothing -> M.div_ [] []

    viewItem :: Model a f -> (a, Maybe UserId) -> EditorViewItem a f n
    viewItem m (item, user) =
      let editState =
            if isNothing e.editable.modify
              then EditingNotAvailable
              else case user of
                (Just u)
                  | u == syncDocumentEnv r ^. #connectedUser % #id -> Editing
                  | otherwise -> NotEditableBecauseLocked (m.users Map.!? u)
                Nothing -> NotEditing
          moveState =
            if isNothing e.editable.reorder
              then MovingNotAvailable
              else case m.reorderFrom of
                (Just moveSource) -> if item == moveSource then MoveSource else PotentialMoveTarget
                Nothing -> NotMoving
          deleteState =
            if isNothing e.editable.delete
              then DeleteNotAvailable
              else Deletable
          item' = fromMaybe item (Map.lookup item m.patches)
          fieldData
            | editState == Editing = fmap (\(n, f) -> (n, f.editor item item')) e.fields
            | otherwise = fmap (\(n, f) -> (n, f.viewer item)) e.fields
       in EditorViewItem {item, editState, moveState, deleteState, fieldData}
